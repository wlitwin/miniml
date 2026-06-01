(* Oracle — the reference interpreter. THE EXECUTABLE SPEC for MiniML evaluation.

   This is a deliberately naive big-step evaluator over the typed AST as it
   exists AFTER typechecking + constraint transformation but BEFORE handler
   classification, match-tree lowering, and optimization. It is independent of
   every backend lowering decision, so diffing any backend against it tests
   those lowerings.

   Design rules (do not "improve" these away):
   - No performance tricks. Clarity over speed, always.
   - Effects use the textbook deep-handler semantics via an outcome monad:
     evaluation either completes (Done) or stops at a perform (Perform) carrying
     a resumption function. Handlers are ~20 lines. Multishot resume works
     because resumptions are ordinary OCaml functions.
   - Control flow (return / break / continue / fold-continue) uses the same
     outcome mechanism (Ctl), caught at the boundary that owns it.
   - The heap is shared across resumes (refs / mutable fields / arrays);
     control state is implicitly copied because resumptions are pure closures.
     This matches the VM's capture-promotion semantics.
   - Builtins are NOT reimplemented: first-order builtins delegate to the
     bytecode VM's registered externals (Bytecode.VExternal) through value
     conversion. The builtin spec is the OCaml implementations; the oracle is
     the spec for EVALUATION.

   What the oracle rejects (raises Oracle_error / Unsupported):
   - TEMatchTree (post-lowering form — the oracle never sees lowered IR)
   - PatMap / PatSet patterns (not yet supported; tracked)
*)

exception Oracle_error of string
exception Unsupported of string

let err fmt = Printf.ksprintf (fun s -> raise (Oracle_error s)) fmt
let unsupported what = raise (Unsupported ("oracle: unsupported: " ^ what))

(* ---- Values ---------------------------------------------------------- *)

type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VString of string
  | VByte of int
  | VRune of int
  | VUnit
  | VTuple of value list
  | VList of value list
  | VRecord of (string * value ref) list (* field order preserved *)
  | VVariant of string * value option
  | VArray of value array
  | VClosure of closure
  | VBuiltin of builtin
  | VContinuation of continuation
  | VRef of value ref

and closure = {
  c_env : env;
  c_param : string;
  c_body : Typechecker.texpr;
  (* Recursive closures see themselves via env entries added at TELetRec. *)
}

(* Continuations are ONE-SHOT, like every backend: resuming twice is an error
   unless the continuation was duplicated with copy_continuation (which shares
   the resumption function but gets a fresh use flag). *)
and continuation = { k_resume : value -> outcome; k_used : bool ref }

and builtin = {
  b_name : string;
  b_arity : int;
  b_impl : builtin_impl;
  b_args : value list; (* partial application, oldest first *)
}

(* Most builtins delegate to the VM's implementations (BridgedVM) through value
   conversion. Builtins whose semantics depend on PHYSICAL IDENTITY or that
   MUTATE their arguments cannot cross the conversion boundary (conversion
   copies), so they are implemented natively on oracle values (OracleNative):
   phys_equal, array_set / Array.set, copy_continuation. *)
and builtin_impl =
  | BridgedVM of (Bytecode.value list -> Bytecode.value)
  | OracleNative of (value list -> outcome)

and env = { vars : (string * value ref) list; globals : globals }

and globals = {
  (* Name -> VM external lookup, for bridging builtins (print, String.split,
     typeclass primitives, ...). Populated from the interp setup's globals. *)
  lookup_external : string -> Bytecode.external_fn option;
  type_env : Types.type_env;
}

(* ---- Outcomes: the result of evaluating an expression ----------------- *)

and outcome =
  | Done of value
  | Perform of perform
      (* evaluation stopped at `perform op arg`; [resume] continues it *)
  | Ctl of ctl (* non-local control flow, caught at its owning boundary *)

and perform = { p_op : string; p_arg : value; p_resume : value -> outcome }

and ctl =
  | CReturn of value (* `return e`        -> caught at function application *)
  | CBreak of value (* `break e`          -> caught at while/for loop *)
  | CContinue (* `continue`               -> caught at while loop *)
  | CFoldContinue of value (* for-loop `continue` -> caught at fold callback *)

(* Monadic bind: thread sub-evaluation outcomes. A Perform or Ctl in a
   sub-expression suspends/aborts the whole expression; for Perform the
   continuation is extended so that resuming re-runs the rest of this
   expression. This single function is what makes multishot resume work. *)
let rec bind (o : outcome) (k : value -> outcome) : outcome =
  match o with
  | Done v -> k v
  | Perform p -> Perform { p with p_resume = (fun v -> bind (p.p_resume v) k) }
  | Ctl c -> Ctl c

(* Evaluate a list of sub-expressions left to right, then continue. *)
let rec bind_list (os : (unit -> outcome) list) (acc : value list)
    (k : value list -> outcome) : outcome =
  match os with
  | [] -> k (List.rev acc)
  | o :: rest -> bind (o ()) (fun v -> bind_list rest (v :: acc) k)

(* ---- Environment ------------------------------------------------------ *)

(* Builtins that must operate on oracle values directly (identity / mutation /
   continuations cannot cross the conversion boundary). Checked before the
   bridged externals. The implementations live further down (after [apply]);
   they are tied through this forward reference. *)
let native_builtins : (string * int) list =
  [
    ("phys_equal", 2);
    ("Stdlib.phys_equal", 2);
    ("array_set", 3);
    ("Array.set", 3);
    ("copy_continuation", 1);
    ("Stdlib.copy_continuation", 1);
  ]

let native_impl_ref : (string -> value list -> outcome) ref =
  ref (fun _ _ -> err "oracle: native builtins not initialized")

let lookup env name =
  match List.assoc_opt name env.vars with
  | Some r -> Some !r
  | None -> (
      match List.assoc_opt name native_builtins with
      | Some arity ->
          Some
            (VBuiltin
               {
                 b_name = name;
                 b_arity = arity;
                 b_impl = OracleNative (fun args -> !native_impl_ref name args);
                 b_args = [];
               })
      | None -> (
          (* Fall back to bridged VM externals (builtins, stdlib module fns) *)
          match env.globals.lookup_external name with
          | Some ext ->
              Some
                (VBuiltin
                   {
                     b_name = ext.Bytecode.ext_name;
                     b_arity = ext.Bytecode.ext_arity;
                     b_impl = BridgedVM ext.Bytecode.ext_fn;
                     b_args = [];
                   })
          | None -> None))

let bind_var env name v = { env with vars = (name, ref v) :: env.vars }
let bind_ref env name r = { env with vars = (name, r) :: env.vars }

(* ---- Conversion to/from the VM's value type --------------------------- *)
(* Used only at the builtin boundary and for printing results. First-order
   values convert exactly; closures/continuations cannot cross. *)

let rec to_vm (v : value) : Bytecode.value =
  match v with
  | VInt n -> Bytecode.VInt n
  | VFloat f -> Bytecode.VFloat f
  | VBool b -> Bytecode.VBool b
  | VString s -> Bytecode.VString s
  | VByte b -> Bytecode.VByte b
  | VRune r -> Bytecode.VRune r
  | VUnit -> Bytecode.VUnit
  | VTuple vs -> Bytecode.VTuple (Array.of_list (List.map to_vm vs))
  | VList vs -> Bytecode.VList (List.map to_vm vs)
  | VArray vs -> Bytecode.VArray (Array.map to_vm vs)
  | VRef r -> Bytecode.VRef (ref (to_vm !r))
  | VRecord fields ->
      let names = Array.of_list (List.map fst fields) in
      let index = Hashtbl.create (Array.length names) in
      Array.iteri (fun i n -> Hashtbl.replace index n i) names;
      Bytecode.VRecord
        ( { Bytecode.rs_fields = names; rs_index = index },
          Array.of_list (List.map (fun (_, r) -> to_vm !r) fields) )
  | VVariant (name, payload) ->
      (* Tag numbers are not semantically meaningful for printing/equality at
         the oracle level; use 0. (The VM uses tags for dispatch, which the
         oracle does structurally by name.) *)
      Bytecode.VVariant (0, name, Option.map to_vm payload)
  | VClosure _ | VBuiltin _ | VContinuation _ ->
      (* Closures crossing into builtins: represent as an opaque external so
         pp_value prints something function-like. Builtins that CALL their
         function arguments cannot be bridged (none of the registered ones do —
         higher-order stdlib functions are MiniML source, not externals). *)
      Bytecode.VExternal
        { ext_name = "<fun>"; ext_arity = 1; ext_fn = (fun _ -> Bytecode.VUnit); ext_args = [] }

let rec from_vm (v : Bytecode.value) : value =
  match v with
  | Bytecode.VInt n -> VInt n
  | Bytecode.VFloat f -> VFloat f
  | Bytecode.VBool b -> VBool b
  | Bytecode.VString s -> VString s
  | Bytecode.VByte b -> VByte b
  | Bytecode.VRune r -> VRune r
  | Bytecode.VUnit -> VUnit
  | Bytecode.VTuple vs -> VTuple (Array.to_list (Array.map from_vm vs))
  | Bytecode.VList vs -> VList (List.map from_vm vs)
  | Bytecode.VArray vs -> VArray (Array.map from_vm vs)
  | Bytecode.VRef r -> VRef (ref (from_vm !r))
  | Bytecode.VRecord (shape, values) ->
      VRecord
        (Array.to_list
           (Array.mapi
              (fun i name -> (name, ref (from_vm values.(i))))
              shape.Bytecode.rs_fields))
  | Bytecode.VVariant (_, name, payload) -> VVariant (name, Option.map from_vm payload)
  | Bytecode.VExternal ext ->
      VBuiltin
        {
          b_name = ext.Bytecode.ext_name;
          b_arity = ext.Bytecode.ext_arity;
          b_impl = BridgedVM ext.Bytecode.ext_fn;
          b_args = [];
        }
  | Bytecode.VClosure _ | Bytecode.VPartial _ | Bytecode.VProto _
  | Bytecode.VContinuation _ ->
      err "oracle: cannot convert VM closure/continuation into oracle value"

(* Pretty-print via the VM's printer so output format matches every backend. *)
let pp (v : value) : string = Bytecode.pp_value (to_vm v)

(* ---- Structural equality / comparison --------------------------------- *)
(* Defined on oracle values directly (cannot delegate: closures compare by
   physical identity, and conversion would lose that). Mirrors vm.ml. *)

let rec equal (a : value) (b : value) : bool =
  match (a, b) with
  | VInt a, VInt b -> a = b
  | VFloat a, VFloat b -> a = b
  | VBool a, VBool b -> a = b
  | VString a, VString b -> a = b
  | VByte a, VByte b -> a = b
  | VRune a, VRune b -> a = b
  | VUnit, VUnit -> true
  | VTuple a, VTuple b ->
      List.length a = List.length b && List.for_all2 equal a b
  | VList a, VList b -> List.length a = List.length b && List.for_all2 equal a b
  | VArray a, VArray b ->
      Array.length a = Array.length b
      && Array.for_all2 (fun x y -> equal x y) a b
  | VRecord a, VRecord b ->
      List.length a = List.length b
      && List.for_all2 (fun (n1, r1) (n2, r2) -> n1 = n2 && equal !r1 !r2) a b
  | VVariant (n1, p1), VVariant (n2, p2) -> (
      n1 = n2
      && match (p1, p2) with
         | None, None -> true
         | Some a, Some b -> equal a b
         | _ -> false)
  | VRef a, VRef b -> equal !a !b
  | (VClosure _ | VBuiltin _ | VContinuation _), _ -> a == b
  | _, (VClosure _ | VBuiltin _ | VContinuation _) -> a == b
  | _ -> false

let rec compare_values (a : value) (b : value) : int =
  match (a, b) with
  | VInt a, VInt b -> compare a b
  | VFloat a, VFloat b -> compare a b
  | VBool a, VBool b -> compare a b
  | VString a, VString b -> compare a b
  | VByte a, VByte b -> compare a b
  | VRune a, VRune b -> compare a b
  | VUnit, VUnit -> 0
  | VTuple a, VTuple b | VList a, VList b ->
      let rec go a b =
        match (a, b) with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        | x :: xs, y :: ys ->
            let c = compare_values x y in
            if c <> 0 then c else go xs ys
      in
      go a b
  | VArray a, VArray b ->
      compare_values (VList (Array.to_list a)) (VList (Array.to_list b))
  | VVariant (n1, p1), VVariant (n2, p2) -> (
      (* Order by constructor name then payload; matches structural compare *)
      let c = compare n1 n2 in
      if c <> 0 then c
      else
        match (p1, p2) with
        | None, None -> 0
        | None, Some _ -> -1
        | Some _, None -> 1
        | Some a, Some b -> compare_values a b)
  | VRecord a, VRecord b ->
      let rec go a b =
        match (a, b) with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        | (_, r1) :: xs, (_, r2) :: ys ->
            let c = compare_values !r1 !r2 in
            if c <> 0 then c else go xs ys
      in
      go a b
  | _ -> err "oracle: cannot compare %s and %s" (pp a) (pp b)

(* ---- Pattern matching -------------------------------------------------- *)
(* Returns Some bindings if the pattern matches, None otherwise.
   Bindings are (name, value) pairs in source order. *)

let rec match_pattern env (v : value) (pat : Ast.pattern) :
    (string * value) list option =
  match (pat, v) with
  | Ast.PatWild, _ -> Some []
  | Ast.PatVar name, _ -> Some [ (name, v) ]
  | Ast.PatInt n, VInt m -> if n = m then Some [] else None
  | Ast.PatFloat f, VFloat g -> if f = g then Some [] else None
  | Ast.PatBool b, VBool c -> if b = c then Some [] else None
  | Ast.PatString s, VString t -> if s = t then Some [] else None
  | Ast.PatInt n, VByte m -> if n = m then Some [] else None
  | Ast.PatInt n, VRune m -> if n = m then Some [] else None
  | Ast.PatUnit, VUnit -> Some []
  | Ast.PatTuple pats, VTuple vs ->
      if List.length pats <> List.length vs then None
      else match_all env vs pats
  | Ast.PatNil, VList [] -> Some []
  | Ast.PatNil, VList _ -> None
  | Ast.PatCons (hp, tp), VList (h :: t) ->
      Option.bind (match_pattern env h hp) (fun b1 ->
          Option.map (fun b2 -> b1 @ b2) (match_pattern env (VList t) tp))
  | Ast.PatCons _, VList [] -> None
  | Ast.PatConstruct (cname, payload_pat), VVariant (vname, payload) -> (
      (* Constructor names in patterns may be qualified (Mod.Ctor); variant
         values store the bare or qualified name as constructed. Compare the
         final segments. *)
      let last_seg s =
        match String.rindex_opt s '.' with
        | Some i -> String.sub s (i + 1) (String.length s - i - 1)
        | None -> s
      in
      if last_seg cname <> last_seg vname then None
      else
        match (payload_pat, payload) with
        | None, None -> Some []
        | Some p, Some pv -> match_pattern env pv p
        | None, Some _ -> None (* zero-arg pattern for payload ctor *)
        | Some _, None -> None)
  | Ast.PatConstruct (cname, payload_pat), _ -> (
      (* Newtype constructors are erased at runtime: `MMap p` matches the
         underlying value directly. *)
      match
        List.assoc_opt cname env.globals.type_env.Types.constructors
      with
      | Some info
        when List.mem info.Types.ctor_type_name
               env.globals.type_env.Types.newtypes -> (
          match payload_pat with
          | Some p -> match_pattern env v p
          | None -> None)
      | _ -> None)
  | Ast.PatRecord field_pats, VRecord fields ->
      let rec go bindings = function
        | [] -> Some bindings
        | (fname, fpat) :: rest -> (
            match List.assoc_opt fname fields with
            | None -> None
            | Some r ->
                Option.bind (match_pattern env !r fpat) (fun bs ->
                    go (bindings @ bs) rest))
      in
      go [] field_pats
  | Ast.PatAs (p, name), _ ->
      Option.map (fun bs -> bs @ [ (name, v) ]) (match_pattern env v p)
  | Ast.PatOr (p1, p2), _ -> (
      match match_pattern env v p1 with
      | Some bs -> Some bs
      | None -> match_pattern env v p2)
  | Ast.PatArray pats, VArray vs ->
      if List.length pats <> Array.length vs then None
      else match_all env (Array.to_list vs) pats
  | Ast.PatPin name, _ -> (
      (* Pin: match against the current value of an existing binding *)
      match lookup env name with
      | Some pinned -> if equal v pinned then Some [] else None
      | None -> err "oracle: unbound variable in pin pattern: %s" name)
  | Ast.PatAnnot (p, _), _ -> match_pattern env v p
  | Ast.PatPolyVariant (tag, payload_pat), VVariant (vtag, payload) -> (
      if tag <> vtag then None
      else
        match (payload_pat, payload) with
        | None, None -> Some []
        | Some p, Some pv -> match_pattern env pv p
        | _ -> None)
  | Ast.PatMap _, _ -> unsupported "map pattern (PatMap)"
  | Ast.PatSet _, _ -> unsupported "set pattern (PatSet)"
  | _ -> None

and match_all env vs pats =
  let rec go acc vs pats =
    match (vs, pats) with
    | [], [] -> Some acc
    | v :: vs, p :: pats ->
        Option.bind (match_pattern env v p) (fun bs -> go (acc @ bs) vs pats)
    | _ -> None
  in
  go [] vs pats

(* ---- Binary / unary operators ----------------------------------------- *)
(* Post-transform_constraints, arithmetic on user types has been rewritten to
   dictionary calls; TEBinop only sees primitives (plus structural =/<>/< etc.
   which work on any value, and ^ on strings, etc.). *)

let eval_binop (op : Ast.binop) (a : value) (b : value) : value =
  match (op, a, b) with
  | Ast.Add, VInt x, VInt y -> VInt (x + y)
  | Ast.Add, VFloat x, VFloat y -> VFloat (x +. y)
  | Ast.Sub, VInt x, VInt y -> VInt (x - y)
  | Ast.Sub, VFloat x, VFloat y -> VFloat (x -. y)
  | Ast.Mul, VInt x, VInt y -> VInt (x * y)
  | Ast.Mul, VFloat x, VFloat y -> VFloat (x *. y)
  | Ast.Div, VInt x, VInt y ->
      if y = 0 then err "division by zero" else VInt (x / y)
  | Ast.Div, VFloat x, VFloat y -> VFloat (x /. y)
  | Ast.Mod, VInt x, VInt y ->
      if y = 0 then err "modulo by zero" else VInt (x mod y)
  | Ast.Eq, _, _ -> VBool (equal a b)
  | Ast.Neq, _, _ -> VBool (not (equal a b))
  | Ast.Lt, _, _ -> VBool (compare_values a b < 0)
  | Ast.Gt, _, _ -> VBool (compare_values a b > 0)
  | Ast.Le, _, _ -> VBool (compare_values a b <= 0)
  | Ast.Ge, _, _ -> VBool (compare_values a b >= 0)
  | Ast.And, VBool x, VBool y -> VBool (x && y)
  | Ast.Or, VBool x, VBool y -> VBool (x || y)
  | Ast.Concat, VString x, VString y -> VString (x ^ y)
  | Ast.Land, VInt x, VInt y -> VInt (x land y)
  | Ast.Lor, VInt x, VInt y -> VInt (x lor y)
  | Ast.Lxor, VInt x, VInt y -> VInt (x lxor y)
  | Ast.Lsl, VInt x, VInt y -> VInt (x lsl y)
  | Ast.Lsr, VInt x, VInt y -> VInt (x lsr y)
  | Ast.Pipe, _, _ -> err "oracle: Pipe binop should be handled in eval"
  | _ ->
      err "oracle: invalid binop operands: %s %s" (pp a) (pp b)

let eval_unop (op : Ast.unop) (a : value) : value =
  match (op, a) with
  | Ast.Neg, VInt x -> VInt (-x)
  | Ast.Neg, VFloat x -> VFloat (-.x)
  | Ast.Not, VBool b -> VBool (not b)
  | Ast.Lnot, VInt x -> VInt (lnot x)
  | _ -> err "oracle: invalid unop operand: %s" (pp a)

(* ---- The evaluator ----------------------------------------------------- *)

let rec eval (env : env) (te : Typechecker.texpr) : outcome =
  match te.Typechecker.expr with
  (* -- Literals -- *)
  | Typechecker.TEInt n -> Done (VInt n)
  | Typechecker.TEFloat f -> Done (VFloat f)
  | Typechecker.TEBool b -> Done (VBool b)
  | Typechecker.TEString s -> Done (VString s)
  | Typechecker.TEByte b -> Done (VByte b)
  | Typechecker.TERune r -> Done (VRune r)
  | Typechecker.TEUnit -> Done VUnit
  | Typechecker.TENil -> Done (VList [])
  (* -- Variables and binding -- *)
  | Typechecker.TEVar name -> (
      match lookup env name with
      | Some v -> Done v
      | None -> err "oracle: unbound variable: %s" name)
  | Typechecker.TELet (name, _, e1, e2) ->
      bind (eval env e1) (fun v1 -> eval (bind_var env name v1) e2)
  | Typechecker.TELetMut (name, e1, e2) ->
      bind (eval env e1) (fun v1 -> eval (bind_var env name v1) e2)
  | Typechecker.TELetRec (name, _, fn, e2) ->
      let cell = ref VUnit in
      let env' = bind_ref env name cell in
      bind (eval env' fn) (fun fv ->
          cell := fv;
          eval env' e2)
  | Typechecker.TELetRecAnd (binds, e2) ->
      let cells = List.map (fun (name, _) -> (name, ref VUnit)) binds in
      let env' =
        List.fold_left (fun e (name, cell) -> bind_ref e name cell) env cells
      in
      bind_list
        (List.map (fun (_, fn) () -> eval env' fn) binds)
        []
        (fun fvs ->
          List.iter2 (fun (_, cell) fv -> cell := fv) cells fvs;
          eval env' e2)
  | Typechecker.TEFun (param, body, _) ->
      Done (VClosure { c_env = env; c_param = param; c_body = body })
  | Typechecker.TEApp (fn, arg) ->
      bind (eval env fn) (fun fv ->
          bind (eval env arg) (fun av -> apply fv av))
  (* -- Control flow -- *)
  | Typechecker.TEIf (cond, then_e, else_e) ->
      bind (eval env cond) (fun cv ->
          match cv with
          | VBool true -> eval env then_e
          | VBool false -> eval env else_e
          | v -> err "oracle: if condition is not a bool: %s" (pp v))
  | Typechecker.TESeq (e1, e2) -> bind (eval env e1) (fun _ -> eval env e2)
  | Typechecker.TEReturn e -> bind (eval env e) (fun v -> Ctl (CReturn v))
  | Typechecker.TEBreak e -> bind (eval env e) (fun v -> Ctl (CBreak v))
  | Typechecker.TEContinueLoop -> Ctl CContinue
  | Typechecker.TEFoldContinue e ->
      bind (eval env e) (fun v -> Ctl (CFoldContinue v))
  | Typechecker.TEWhile { tw_cond; tw_body; tw_step } ->
      (* While/for loop. Result is the break value, or unit if the condition
         ends the loop. `continue` skips to the next iteration (running the
         step first, like every backend). *)
      let rec loop () =
        bind (eval env tw_cond) (fun cv ->
            match cv with
            | VBool false -> Done VUnit
            | VBool true -> (
                let body_outcome = eval env tw_body in
                let after_body o =
                  match o with
                  | Done _ -> step_then_loop ()
                  | Ctl (CBreak v) -> Done v
                  | Ctl CContinue -> step_then_loop ()
                  | other -> other (* CReturn / Perform propagate *)
                in
                match body_outcome with
                | Perform p ->
                    (* Effects performed in the loop body suspend the whole
                       loop; resuming continues the iteration then loops. *)
                    Perform
                      { p with p_resume = (fun v -> after_body (bind (p.p_resume v) (fun v -> Done v))) }
                | o -> after_body o)
            | v -> err "oracle: while condition is not a bool: %s" (pp v))
      and step_then_loop () =
        match tw_step with
        | None -> loop ()
        | Some step -> bind (eval env step) (fun _ -> loop ())
      in
      loop ()
  | Typechecker.TEForLoop fold_expr ->
      (* For-loops over collections desugar to a fold application whose
         callback may raise CBreak (caught here) / CFoldContinue (caught at
         the callback boundary in [apply]). *)
      let o = eval env fold_expr in
      catch_break o
  (* -- Data construction -- *)
  | Typechecker.TETuple es ->
      bind_list (List.map (fun e () -> eval env e) es) [] (fun vs ->
          Done (VTuple vs))
  | Typechecker.TEArray es ->
      bind_list (List.map (fun e () -> eval env e) es) [] (fun vs ->
          Done (VArray (Array.of_list vs)))
  | Typechecker.TECons (h, t) ->
      bind (eval env h) (fun hv ->
          bind (eval env t) (fun tv ->
              match tv with
              | VList l -> Done (VList (hv :: l))
              | v -> err "oracle: cons onto non-list: %s" (pp v)))
  | Typechecker.TERecord fields ->
      bind_list
        (List.map (fun (_, e) () -> eval env e) fields)
        []
        (fun vs ->
          Done
            (VRecord (List.map2 (fun (name, _) v -> (name, ref v)) fields vs)))
  | Typechecker.TERecordUpdate (base, overrides) ->
      bind (eval env base) (fun bv ->
          match bv with
          | VRecord fields ->
              bind_list
                (List.map (fun (_, e) () -> eval env e) overrides)
                []
                (fun vs ->
                  let updated = List.map2 (fun (n, _) v -> (n, v)) overrides vs in
                  Done
                    (VRecord
                       (List.map
                          (fun (name, r) ->
                            match List.assoc_opt name updated with
                            | Some v -> (name, ref v)
                            | None -> (name, ref !r))
                          fields)))
          | v -> err "oracle: record update on non-record: %s" (pp v))
  | Typechecker.TERecordUpdateIdx (base, pairs) ->
      (* Indexed update: { base with [k] = v } for arrays/maps. Evaluate base,
         indexes, values; for arrays produce an updated copy. *)
      bind (eval env base) (fun bv ->
          let rec go pairs (acc : value) =
            match pairs with
            | [] -> Done acc
            | (idx_e, val_e) :: rest ->
                bind (eval env idx_e) (fun idx ->
                    bind (eval env val_e) (fun v ->
                        match (acc, idx) with
                        | VArray arr, VInt i ->
                            let copy = Array.copy arr in
                            if i < 0 || i >= Array.length copy then
                              err "array index out of bounds: %d" i
                            else (
                              copy.(i) <- v;
                              go rest (VArray copy))
                        | _ -> unsupported "indexed record update on this type"))
          in
          go pairs bv)
  | Typechecker.TEConstruct (name, payload) -> (
      let is_newtype =
        match
          List.assoc_opt name env.globals.type_env.Types.constructors
        with
        | Some info ->
            List.mem info.Types.ctor_type_name
              env.globals.type_env.Types.newtypes
        | None -> false
      in
      match payload with
      | None -> Done (VVariant (name, None))
      | Some e ->
          bind (eval env e) (fun v ->
              if is_newtype then Done v (* newtype ctors are erased *)
              else Done (VVariant (name, Some v))))
  (* -- Data access -- *)
  | Typechecker.TEField (e, fname) ->
      bind (eval env e) (fun v ->
          match v with
          | VRecord fields -> (
              match List.assoc_opt fname fields with
              | Some r -> Done !r
              | None -> err "oracle: no field %s in record" fname)
          | v -> err "oracle: field access on non-record: %s" (pp v))
  | Typechecker.TEIndex (coll, idx) ->
      bind (eval env coll) (fun cv ->
          bind (eval env idx) (fun iv ->
              match (cv, iv) with
              | VArray arr, VInt i ->
                  if i < 0 || i >= Array.length arr then
                    err "array index out of bounds: %d (length %d)" i
                      (Array.length arr)
                  else Done arr.(i)
              | VString s, VInt i ->
                  if i < 0 || i >= String.length s then
                    err "string index out of bounds: %d (length %d)" i
                      (String.length s)
                  else Done (VByte (Char.code s.[i]))
              | _ -> unsupported "indexing on this type"))
  (* -- Operators -- *)
  | Typechecker.TEBinop (Ast.Pipe, lhs, rhs) ->
      (* x |> f  ==  f x *)
      bind (eval env lhs) (fun lv ->
          bind (eval env rhs) (fun rv -> apply rv lv))
  | Typechecker.TEBinop (Ast.And, lhs, rhs) ->
      (* Short-circuit *)
      bind (eval env lhs) (fun lv ->
          match lv with
          | VBool false -> Done (VBool false)
          | VBool true -> eval env rhs
          | v -> err "oracle: && on non-bool: %s" (pp v))
  | Typechecker.TEBinop (Ast.Or, lhs, rhs) ->
      bind (eval env lhs) (fun lv ->
          match lv with
          | VBool true -> Done (VBool true)
          | VBool false -> eval env rhs
          | v -> err "oracle: || on non-bool: %s" (pp v))
  | Typechecker.TEBinop (op, lhs, rhs) ->
      bind (eval env lhs) (fun lv ->
          bind (eval env rhs) (fun rv -> Done (eval_binop op lv rv)))
  | Typechecker.TEUnop (op, e) ->
      bind (eval env e) (fun v -> Done (eval_unop op v))
  (* -- Mutation -- *)
  | Typechecker.TEAssign (name, e) ->
      bind (eval env e) (fun v ->
          match List.assoc_opt name env.vars with
          | Some r ->
              r := v;
              Done VUnit
          | None -> err "oracle: assignment to unbound variable: %s" name)
  | Typechecker.TEFieldAssign (re, fname, ve) ->
      bind (eval env re) (fun rv ->
          bind (eval env ve) (fun v ->
              match rv with
              | VRecord fields -> (
                  match List.assoc_opt fname fields with
                  | Some r ->
                      r := v;
                      Done VUnit
                  | None -> err "oracle: no field %s in record" fname)
              | v -> err "oracle: field assignment on non-record: %s" (pp v)))
  (* -- Pattern matching -- *)
  | Typechecker.TEMatch (scrut, arms, _) ->
      bind (eval env scrut) (fun sv ->
          let rec try_arms = function
            | [] ->
                err "non-exhaustive match at line %d: no pattern matched %s"
                  te.Typechecker.loc.Token.line (pp sv)
            | (pat, guard, body) :: rest -> (
                match match_pattern env sv pat with
                | None -> try_arms rest
                | Some bindings -> (
                    let env' =
                      List.fold_left
                        (fun e (n, v) -> bind_var e n v)
                        env bindings
                    in
                    match guard with
                    | None -> eval env' body
                    | Some g ->
                        bind (eval env' g) (fun gv ->
                            match gv with
                            | VBool true -> eval env' body
                            | VBool false -> try_arms rest
                            | v -> err "oracle: guard is not a bool: %s" (pp v))))
          in
          try_arms arms)
  | Typechecker.TEMatchTree _ ->
      err
        "oracle: TEMatchTree encountered — the oracle interprets pre-lowering \
         AST only"
  (* -- Effects: the heart of the spec -- *)
  | Typechecker.TEPerform (op_name, arg) ->
      bind (eval env arg) (fun av ->
          Perform { p_op = op_name; p_arg = av; p_resume = (fun v -> Done v) })
  | Typechecker.TEResume (k_expr, val_expr) ->
      bind (eval env k_expr) (fun kv ->
          bind (eval env val_expr) (fun vv ->
              match kv with
              | VContinuation k -> resume_continuation k vv
              | v -> err "oracle: resume of non-continuation: %s" (pp v)))
  | Typechecker.TEHandle (body, arms) -> eval_handle env body arms

(* Catch CBreak at a for-loop boundary. *)
and catch_break (o : outcome) : outcome =
  match o with
  | Ctl (CBreak v) -> Done v
  | Perform p -> Perform { p with p_resume = (fun v -> catch_break (p.p_resume v)) }
  | other -> other

(* Function application. Catches CReturn (early return) and CFoldContinue
   (fold-callback continue) at the function boundary; everything else
   (Perform, CBreak escaping a fold callback) propagates to the caller. *)
and apply (fn : value) (arg : value) : outcome =
  match fn with
  | VClosure c ->
      let env' = bind_var c.c_env c.c_param arg in
      let o = eval env' c.c_body in
      catch_function_ctl o
  | VContinuation k -> resume_continuation k arg
  | VBuiltin b ->
      let args = b.b_args @ [ arg ] in
      if List.length args >= b.b_arity then
        match b.b_impl with
        | OracleNative fn -> fn args
        | BridgedVM fn ->
            (* Saturated: call the VM implementation through value conversion *)
            let vm_args = List.map to_vm args in
            let result =
              try fn vm_args
              with Vm.Runtime_error msg -> raise (Oracle_error msg)
            in
            Done (from_vm result)
      else Done (VBuiltin { b with b_args = args })
  | v -> err "oracle: application of non-function: %s" (pp v)

(* One-shot enforcement: matches the VM ("continuation already resumed"). *)
and resume_continuation (k : continuation) (v : value) : outcome =
  if !(k.k_used) then err "continuation already resumed"
  else begin
    k.k_used := true;
    k.k_resume v
  end

and catch_function_ctl (o : outcome) : outcome =
  match o with
  | Ctl (CReturn v) -> Done v
  | Ctl (CFoldContinue v) -> Done v
  | Perform p ->
      Perform { p with p_resume = (fun v -> catch_function_ctl (p.p_resume v)) }
  | other -> other

(* ---- Native oracle builtins ------------------------------------------- *)
(* Implementations for the identity / mutation / continuation builtins that
   cannot be bridged. Installed into [native_impl_ref] at module init. *)

and native_impl (name : string) (args : value list) : outcome =
  match (name, args) with
  | ("phys_equal" | "Stdlib.phys_equal"), [ a; b ] ->
      (* Physical equality: identity for boxed/mutable values, value equality
         for immediates. Mirrors OCaml's == as used by the VM's impl. *)
      let result =
        match (a, b) with
        | VInt x, VInt y -> x = y
        | VFloat x, VFloat y -> x == y
        | VBool x, VBool y -> x = y
        | VString x, VString y -> x == y
        | VByte x, VByte y -> x = y
        | VRune x, VRune y -> x = y
        | VUnit, VUnit -> true
        | VRecord a, VRecord b -> a == b
        | VArray a, VArray b -> a == b
        | VList a, VList b -> a == b
        | VTuple a, VTuple b -> a == b
        | VVariant (n1, p1), VVariant (n2, p2) -> (
            n1 = n2
            &&
            match (p1, p2) with
            | None, None -> true
            | Some x, Some y -> x == y
            | _ -> false)
        | VRef a, VRef b -> a == b
        | _ -> a == b
      in
      Done (VBool result)
  | ("array_set" | "Array.set"), [ arr; idx; v ] -> (
      match (arr, idx) with
      | VArray a, VInt i ->
          if i < 0 || i >= Array.length a then
            err "array index out of bounds: %d (length %d)" i (Array.length a)
          else begin
            a.(i) <- v;
            Done VUnit
          end
      | _ -> err "oracle: array_set on non-array")
  | ("copy_continuation" | "Stdlib.copy_continuation"), [ k ] -> (
      match k with
      | VContinuation k ->
          (* Same resumption, fresh use flag: the copy can be resumed once more. *)
          Done (VContinuation { k_resume = k.k_resume; k_used = ref false })
      | v ->
          (* copy on a non-continuation is identity (matches the VM builtin) *)
          Done v)
  | _ -> err "oracle: unknown native builtin %s/%d" name (List.length args)

(* Effect handlers: textbook deep-handler semantics.

   handle BODY with | return x -> RET | op arg k -> ARM ...

   - If BODY completes with v, evaluate RET with x = v (or yield v if no
     return arm).
   - If BODY performs op that this handler handles, evaluate ARM with the
     argument bound and k bound to a continuation that, when resumed, continues
     BODY from the perform point UNDER THIS HANDLER AGAIN (deep semantics) and
     whose final value goes through the return arm.
   - If BODY performs an op this handler does not handle, let it pass through,
     but extend its resumption so that when the outer handler resumes it, this
     handler is back in place.

   Multishot: k is a pure function; calling it twice re-runs the rest of BODY
   twice. Heap state (refs) is shared across runs. *)
and eval_handle env body arms : outcome =
  let return_arm =
    List.find_map
      (function Typechecker.THReturn (n, e) -> Some (n, e) | _ -> None)
      arms
  in
  let find_op_arm op_name =
    List.find_map
      (function
        | Typechecker.THOp { op_name = n; arg; k; body } when n = op_name ->
            Some (`Op (arg, k, body))
        | Typechecker.THOpProvide (n, arg, e) when n = op_name ->
            Some (`Provide (arg, e))
        | Typechecker.THOpTry (n, arg, e) when n = op_name ->
            Some (`Try (arg, e))
        | _ -> None)
      arms
  in
  let rec handle (o : outcome) : outcome =
    match o with
    | Done v -> (
        (* Body finished: route through the return arm *)
        match return_arm with
        | None -> Done v
        | Some (name, ret_body) -> eval (bind_var env name v) ret_body)
    | Ctl c -> Ctl c (* return/break/continue escape the handler *)
    | Perform p -> (
        match find_op_arm p.p_op with
        | None ->
            (* Not ours: pass through, but reinstall this handler around the
               resumption (so the handler is live when the outer resumes). *)
            Perform { p with p_resume = (fun v -> handle (p.p_resume v)) }
        | Some (`Op (arg_name, k_name, arm_body)) ->
            (* Deep handler: the continuation re-enters this handler. *)
            let k =
              VContinuation
                {
                  k_resume = (fun v -> handle (p.p_resume v));
                  k_used = ref false;
                }
            in
            let env' = bind_var (bind_var env arg_name p.p_arg) k_name k in
            eval env' arm_body
        | Some (`Provide (arg_name, value_expr)) ->
            (* Tail-resumptive sugar: op arg -> resume k VALUE *)
            let env' = bind_var env arg_name p.p_arg in
            bind (eval env' value_expr) (fun v -> handle (p.p_resume v))
        | Some (`Try (arg_name, fallback)) ->
            (* Non-resuming sugar: op arg -> FALLBACK (continuation dropped).
               The fallback's value goes through the return arm? No — try-style
               arms produce the handle result directly (matches VM semantics). *)
            let env' = bind_var env arg_name p.p_arg in
            eval env' fallback)
  in
  handle (eval env body)

(* ---- Programs ---------------------------------------------------------- *)

(* Evaluate a top-level declaration, returning the updated environment and the
   last computed value (for TDExpr). Effects performed at the top level with no
   handler are an error (matches every backend). *)
let run_outcome (what : string) (o : outcome) : value =
  match o with
  | Done v -> v
  | Perform p -> err "Unhandled effect: %s (in %s)" p.p_op what
  | Ctl (CReturn _) -> err "return outside of function (in %s)" what
  | Ctl (CBreak _) -> err "break outside of loop (in %s)" what
  | Ctl CContinue -> err "continue outside of loop (in %s)" what
  | Ctl (CFoldContinue _) -> err "continue outside of loop (in %s)" what

let rec eval_decl (env : env) (decl : Typechecker.tdecl) : env * value =
  match decl with
  | Typechecker.TDLet (name, e) | Typechecker.TDLetMut (name, e) ->
      let v = run_outcome name (eval env e) in
      (bind_var env name v, VUnit)
  | Typechecker.TDLetRec (name, e) ->
      let cell = ref VUnit in
      let env' = bind_ref env name cell in
      let v = run_outcome name (eval env' e) in
      cell := v;
      (env', VUnit)
  | Typechecker.TDLetRecAnd binds ->
      let cells = List.map (fun (name, _) -> (name, ref VUnit)) binds in
      let env' =
        List.fold_left (fun e (name, cell) -> bind_ref e name cell) env cells
      in
      List.iter2
        (fun (_, cell) (name, e) -> cell := run_outcome name (eval env' e))
        cells binds;
      (env', VUnit)
  | Typechecker.TDExpr e ->
      let v = run_outcome "<toplevel>" (eval env e) in
      (env, v)
  | Typechecker.TDModule (_, decls, _) ->
      (* Module members were qualified by the typechecker; just evaluate them
         in sequence. *)
      let env' =
        List.fold_left (fun e d -> fst (eval_decl e d)) env decls
      in
      (env', VUnit)
  | Typechecker.TDExtern (name, _) ->
      (* Bind the extern to its bridged VM implementation now (fail at use
         time, not declaration time, if it has no implementation). *)
      (match env.globals.lookup_external name with
      | Some _ -> () (* will be found via lookup fallback *)
      | None -> ());
      ignore name;
      (env, VUnit)
  | Typechecker.TDEffect _ | Typechecker.TDType _ | Typechecker.TDClass _
  | Typechecker.TDOpen _ ->
      (env, VUnit)

let eval_program (env : env) (program : Typechecker.tprogram) : env * value =
  List.fold_left
    (fun (env, _) decl -> eval_decl env decl)
    (env, VUnit) program

(* ---- Entry point ------------------------------------------------------- *)

let make_env ~(lookup_external : string -> Bytecode.external_fn option)
    ~(type_env : Types.type_env) : env =
  { vars = []; globals = { lookup_external; type_env } }

(* Tie the forward reference for native builtin implementations. *)
let () = native_impl_ref := native_impl
