(* Deterministic textual serialization of the LOWERED IR (a post-Pipeline.lower
   [Typechecker.tprogram]).

   Purpose (roadmap #13): dump the normalized IR each backend consumes so the two
   compilers' output can be diffed — IR-level parity catches type/lowering
   divergence that value parity misses (e.g. one compiler optimizing or
   classifying a handler differently). The dump is a pretty-printed S-expression,
   designed to be:
   - DETERMINISTIC: no addresses, no tvar ids, no source locations. Types are
     rendered with [Types.pp_ty_normalized] (generalize-then-print), so two
     structurally-equal types print identically regardless of internal tvar ids.
   - SHARED: written in the translatable subset so tools/ocaml_to_mml lowers it
     into the self-hosted compiler, giving both compilers `--emit-ir` for free.

   The reader is a human or a line-diff; there is no deserializer. *)

open Typechecker

(* ---- Emitter: a buffer + current indent, producing one S-expr per line at the
   structural boundaries that matter for diffing. ---- *)

type emitter = { buf : Buffer.t; mutable indent : int }

let make () = { buf = Buffer.create 4096; indent = 0 }

let pad e = Buffer.add_string e.buf (String.make (e.indent * 2) ' ')
let str e s = Buffer.add_string e.buf s
let line e s =
  pad e;
  Buffer.add_string e.buf s;
  Buffer.add_char e.buf '\n'

(* A quoted, escaped string atom (so identifiers with spaces/newlines stay one
   token). Mirrors Serialize.json_escape_string minus the JSON specifics. *)
let q s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\t' -> Buffer.add_string b "\\t"
      | '\r' -> Buffer.add_string b "\\r"
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let ty_atom ty = q (Types.pp_ty_canonical ty)

(* Rename type-variable tokens (`'a`, `'f221`, …) by first appearance within a
   string, to `'a`, `'b`, …. pp_scheme renumbers a scheme's quantified TGens
   canonically but renders fundep-determined / phantom UNBOUND tvars with their
   raw id-based names (`'f221` vs `'t198`) — which differ between the two
   compilers' tvar-id allocation. A scheme string contains `'` only in tvar
   names, so a by-appearance rewrite makes alpha-equivalent schemes print
   identically (roadmap #13). Applied only to scheme strings; node types already
   go through pp_ty_canonical, which generalizes every unbound var to a
   canonically-numbered TGen. *)
let canonicalize_tyvars (s : string) : string =
  let n = String.length s in
  let buf = Buffer.create n in
  let map = ref [] in
  let counter = ref 0 in
  let rec assoc k = function
    | [] -> None
    | (a, b) :: _ when a = k -> Some b
    | _ :: rest -> assoc k rest
  in
  (* Build the letter via String.sub on a literal rather than Char.chr + "%c":
     ocaml_to_mml lowers "%c" to char interpolation, which the self-host renders
     as a byte literal ("#61") instead of the character — a cross-compiler
     divergence in this very canonicalizer. String.sub on a constant is portable. *)
  let letters = "abcdefghijklmnopqrstuvwxyz" in
  let canon () =
    let i = !counter in
    counter := i + 1;
    let letter = String.sub letters (i mod 26) 1 in
    if i < 26 then "'" ^ letter else "'" ^ letter ^ string_of_int (i / 26)
  in
  let is_alnum c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
  in
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    if c = '\'' && !i + 1 < n && s.[!i + 1] >= 'a' && s.[!i + 1] <= 'z' then begin
      let j = ref (!i + 1) in
      while !j < n && is_alnum s.[!j] do
        incr j
      done;
      let tok = String.sub s !i (!j - !i) in
      let cn =
        match assoc tok !map with
        | Some c -> c
        | None ->
            let c = canon () in
            map := (tok, c) :: !map;
            c
      in
      Buffer.add_string buf cn;
      i := !j
    end
    else begin
      Buffer.add_char buf c;
      incr i
    end
  done;
  Buffer.contents buf

let scheme_atom (s : Types.scheme) = q (canonicalize_tyvars (Types.pp_scheme s))

(* Render a float with the codebase's canonical BARE %g format (what pp_value /
   show / string interpolation use), NOT OCaml's stdlib string_of_float — the
   latter appends a trailing "." for integer-valued floats ("3.") while the
   self-host VM's string_of_float does not ("3"), which would break IR parity.
   ocaml_to_mml lowers Printf.sprintf "%g" to the self-host's bare interpolation,
   so both compilers render this identically. *)
let float_atom f = q (Printf.sprintf "%g" f)

(* ---- binop / unop names (stable spellings, no dependence on pp) ---- *)

let binop_name (op : Ast.binop) =
  match op with
  | Ast.Add -> "+"
  | Ast.Sub -> "-"
  | Ast.Mul -> "*"
  | Ast.Div -> "/"
  | Ast.Mod -> "mod"
  | Ast.Eq -> "="
  | Ast.Neq -> "<>"
  | Ast.Lt -> "<"
  | Ast.Gt -> ">"
  | Ast.Le -> "<="
  | Ast.Ge -> ">="
  | Ast.And -> "&&"
  | Ast.Or -> "||"
  | Ast.Concat -> "^"
  | Ast.Land -> "land"
  | Ast.Lor -> "lor"
  | Ast.Lxor -> "lxor"
  | Ast.Lsl -> "lsl"
  | Ast.Lsr -> "lsr"
  | Ast.Pipe -> "|>"

let unop_name (op : Ast.unop) =
  match op with
  | Ast.Neg -> "neg"
  | Ast.Not -> "not"
  | Ast.Lnot -> "lnot"

(* ---- Match-tree IR ---- *)

let map_key_atom (k : Match_tree_types.map_key) =
  match k with
  | Match_tree_types.MKInt i -> Printf.sprintf "(MKInt %d)" i
  | Match_tree_types.MKString s -> Printf.sprintf "(MKString %s)" (q s)
  | Match_tree_types.MKBool b -> Printf.sprintf "(MKBool %b)" b
  | Match_tree_types.MKFloat f -> Printf.sprintf "(MKFloat %s)" (float_atom f)
  | Match_tree_types.MKPin s -> Printf.sprintf "(MKPin %s)" (q s)

let access_atom (a : Match_tree_types.access) =
  match a with
  | Match_tree_types.ATupleField i -> Printf.sprintf "(ATupleField %d)" i
  | Match_tree_types.ARecordField (n, i) -> Printf.sprintf "(ARecordField %s %d)" (q n) i
  | Match_tree_types.AVariantPayload -> "AVariantPayload"
  | Match_tree_types.AConsHead -> "AConsHead"
  | Match_tree_types.AConsTail -> "AConsTail"
  | Match_tree_types.AArrayElem i -> Printf.sprintf "(AArrayElem %d)" i
  | Match_tree_types.AMapValue k -> Printf.sprintf "(AMapValue %s)" (map_key_atom k)

let occ_atom (occ : Match_tree_types.occurrence) =
  "(" ^ String.concat " " (List.map access_atom occ) ^ ")"

let test_atom (t : Match_tree_types.test) =
  match t with
  | Match_tree_types.TConstructor (n, tag) -> Printf.sprintf "(TConstructor %s %d)" (q n) tag
  | Match_tree_types.TPolyVariant (n, h) -> Printf.sprintf "(TPolyVariant %s %d)" (q n) h
  | Match_tree_types.TBoolLit b -> Printf.sprintf "(TBoolLit %b)" b
  | Match_tree_types.TIntLit i -> Printf.sprintf "(TIntLit %d)" i
  | Match_tree_types.TFloatLit f -> Printf.sprintf "(TFloatLit %s)" (float_atom f)
  | Match_tree_types.TStringLit s -> Printf.sprintf "(TStringLit %s)" (q s)
  | Match_tree_types.TUnit -> "TUnit"
  | Match_tree_types.TNil -> "TNil"
  | Match_tree_types.TCons -> "TCons"
  | Match_tree_types.TArrayLen i -> Printf.sprintf "(TArrayLen %d)" i
  | Match_tree_types.TMapHasKey k -> Printf.sprintf "(TMapHasKey %s)" (map_key_atom k)
  | Match_tree_types.TPin s -> Printf.sprintf "(TPin %s)" (q s)

let binding_atom (b : Match_tree_types.binding) =
  Printf.sprintf "(bind %s %s %s)" (q b.Match_tree_types.var_name)
    (occ_atom b.Match_tree_types.bind_occ) (ty_atom b.Match_tree_types.bind_ty)

let bindings_atom bs = "(" ^ String.concat " " (List.map binding_atom bs) ^ ")"

(* ---- Expressions ---- *)

let rec emit_expr e (te : texpr) =
  let k = "(:T " ^ ty_atom te.ty ^ ")" in
  match te.expr with
  | TEInt i -> line e (Printf.sprintf "(TEInt %d %s)" i k)
  | TEFloat f -> line e (Printf.sprintf "(TEFloat %s %s)" (float_atom f) k)
  | TEBool b -> line e (Printf.sprintf "(TEBool %b %s)" b k)
  | TEString s -> line e (Printf.sprintf "(TEString %s %s)" (q s) k)
  | TEByte i -> line e (Printf.sprintf "(TEByte %d %s)" i k)
  | TERune i -> line e (Printf.sprintf "(TERune %d %s)" i k)
  | TEUnit -> line e (Printf.sprintf "(TEUnit %s)" k)
  | TENil -> line e (Printf.sprintf "(TENil %s)" k)
  | TEContinueLoop -> line e (Printf.sprintf "(TEContinueLoop %s)" k)
  | TEVar n -> line e (Printf.sprintf "(TEVar %s %s)" (q n) k)
  | TELet (n, sc, e1, e2) -> emit_binding e "TELet" n sc e1 e2 k
  | TELetRec (n, sc, e1, e2) -> emit_binding e "TELetRec" n sc e1 e2 k
  | TELetMut (n, e1, e2) ->
      block e (Printf.sprintf "TELetMut %s %s" (q n) k) (fun () ->
          emit_expr e e1;
          emit_expr e e2)
  | TEFun (p, body, has_ret) ->
      block e (Printf.sprintf "TEFun %s ret=%b %s" (q p) has_ret k) (fun () ->
          emit_expr e body)
  | TEApp (f, a) ->
      block e (Printf.sprintf "TEApp %s" k) (fun () ->
          emit_expr e f;
          emit_expr e a)
  | TEIf (c, t, el) ->
      block e (Printf.sprintf "TEIf %s" k) (fun () ->
          emit_expr e c;
          emit_expr e t;
          emit_expr e el)
  | TEBinop (op, l, r) ->
      block e (Printf.sprintf "TEBinop %s %s" (q (binop_name op)) k) (fun () ->
          emit_expr e l;
          emit_expr e r)
  | TEUnop (op, x) ->
      block e (Printf.sprintf "TEUnop %s %s" (q (unop_name op)) k) (fun () ->
          emit_expr e x)
  | TETuple es ->
      block e (Printf.sprintf "TETuple %s" k) (fun () -> List.iter (emit_expr e) es)
  | TEArray es ->
      block e (Printf.sprintf "TEArray %s" k) (fun () -> List.iter (emit_expr e) es)
  | TERecord fields ->
      block e (Printf.sprintf "TERecord %s" k) (fun () -> emit_fields e fields)
  | TERecordUpdate (base, fields) ->
      block e (Printf.sprintf "TERecordUpdate %s" k) (fun () ->
          emit_expr e base;
          emit_fields e fields)
  | TERecordUpdateIdx (base, pairs) ->
      block e (Printf.sprintf "TERecordUpdateIdx %s" k) (fun () ->
          emit_expr e base;
          List.iter
            (fun (idx, v) ->
              block e "idx" (fun () ->
                  emit_expr e idx;
                  emit_expr e v))
            pairs)
  | TEField (base, name) ->
      block e (Printf.sprintf "TEField %s %s" (q name) k) (fun () -> emit_expr e base)
  | TEIndex (base, idx) ->
      block e (Printf.sprintf "TEIndex %s" k) (fun () ->
          emit_expr e base;
          emit_expr e idx)
  | TECons (h, t) ->
      block e (Printf.sprintf "TECons %s" k) (fun () ->
          emit_expr e h;
          emit_expr e t)
  | TEConstruct (n, None, tag) ->
      line e (Printf.sprintf "(TEConstruct %s %d %s)" (q n) tag k)
  | TEConstruct (n, Some arg, tag) ->
      block e
        (Printf.sprintf "TEConstruct %s %d %s" (q n) tag k)
        (fun () -> emit_expr e arg)
  | TEMatch (scrut, arms, _kind) ->
      (* Should not survive Pipeline.lower; serialized anyway for un-lowered dumps. *)
      block e (Printf.sprintf "TEMatch %s" k) (fun () ->
          emit_expr e scrut;
          List.iter
            (fun (_pat, guard, body) ->
              block e "arm" (fun () ->
                  (match guard with
                  | Some g -> block e "guard" (fun () -> emit_expr e g)
                  | None -> ());
                  emit_expr e body))
            arms)
  | TEAssign (n, v) ->
      block e (Printf.sprintf "TEAssign %s %s" (q n) k) (fun () -> emit_expr e v)
  | TEFieldAssign (base, name, v) ->
      block e (Printf.sprintf "TEFieldAssign %s %s" (q name) k) (fun () ->
          emit_expr e base;
          emit_expr e v)
  | TESeq (e1, e2) ->
      block e (Printf.sprintf "TESeq %s" k) (fun () ->
          emit_expr e e1;
          emit_expr e e2)
  | TEPerform (op, arg) ->
      block e (Printf.sprintf "TEPerform %s %s" (q op) k) (fun () -> emit_expr e arg)
  | TEHandle (body, arms) ->
      block e (Printf.sprintf "TEHandle %s" k) (fun () ->
          emit_expr e body;
          List.iter (emit_arm e) arms)
  | TEResume (kont, v) ->
      block e (Printf.sprintf "TEResume %s" k) (fun () ->
          emit_expr e kont;
          emit_expr e v)
  | TEWhile { tw_cond; tw_body; tw_step } ->
      block e (Printf.sprintf "TEWhile %s" k) (fun () ->
          block e "cond" (fun () -> emit_expr e tw_cond);
          block e "body" (fun () -> emit_expr e tw_body);
          match tw_step with
          | Some s -> block e "step" (fun () -> emit_expr e s)
          | None -> ())
  | TEBreak v ->
      block e (Printf.sprintf "TEBreak %s" k) (fun () -> emit_expr e v)
  | TEFoldContinue v ->
      block e (Printf.sprintf "TEFoldContinue %s" k) (fun () -> emit_expr e v)
  | TEForLoop fold ->
      block e (Printf.sprintf "TEForLoop %s" k) (fun () -> emit_expr e fold)
  | TEReturn v ->
      block e (Printf.sprintf "TEReturn %s" k) (fun () -> emit_expr e v)
  | TELetRecAnd (binds, body) ->
      block e (Printf.sprintf "TELetRecAnd %s" k) (fun () ->
          List.iter
            (fun (n, fn) -> block e (Printf.sprintf "and %s" (q n)) (fun () -> emit_expr e fn))
            binds;
          block e "in" (fun () -> emit_expr e body))
  | TEMatchTree cm -> emit_match_tree e cm k

and emit_binding e tag n sc e1 e2 k =
  let sc_s = match sc with Some s -> " scheme=" ^ scheme_atom s | None -> "" in
  block e (Printf.sprintf "%s %s%s %s" tag (q n) sc_s k) (fun () ->
      emit_expr e e1;
      emit_expr e e2)

and emit_fields e fields =
  List.iter
    (fun (name, v) -> block e (Printf.sprintf "field %s" (q name)) (fun () -> emit_expr e v))
    fields

and emit_arm e (arm : thandle_arm) =
  match arm with
  | THReturn (n, body) ->
      block e (Printf.sprintf "THReturn %s" (q n)) (fun () -> emit_expr e body)
  | THOp { op_name; arg; k; body } ->
      block e (Printf.sprintf "THOp %s arg=%s k=%s" (q op_name) (q arg) (q k))
        (fun () -> emit_expr e body)
  | THOpProvide (op, arg, v) ->
      block e (Printf.sprintf "THOpProvide %s arg=%s" (q op) (q arg)) (fun () ->
          emit_expr e v)
  | THOpTry (op, arg, v) ->
      block e (Printf.sprintf "THOpTry %s arg=%s" (q op) (q arg)) (fun () ->
          emit_expr e v)

and emit_match_tree e (cm : texpr Match_tree_types.compiled_match) k =
  block e (Printf.sprintf "TEMatchTree %s" k) (fun () ->
      block e "scrutinee" (fun () -> emit_expr e cm.Match_tree_types.scrutinee);
      line e (Printf.sprintf "(scrut-ty %s)" (ty_atom cm.Match_tree_types.scrutinee_ty));
      Array.iteri
        (fun i (arm : texpr Match_tree_types.match_arm) ->
          block e (Printf.sprintf "arm %d" i) (fun () ->
              emit_expr e arm.Match_tree_types.arm_body))
        cm.Match_tree_types.match_arms;
      block e "tree" (fun () -> emit_dtree e cm.Match_tree_types.tree))

and emit_dtree e (t : texpr Match_tree_types.dtree) =
  match t with
  | Match_tree_types.DSwitch { occ; occ_ty; cases; default } ->
      block e
        (Printf.sprintf "DSwitch occ=%s ty=%s" (occ_atom occ) (ty_atom occ_ty))
        (fun () ->
          List.iter
            (fun (test, binds, sub) ->
              block e
                (Printf.sprintf "case %s binds=%s" (test_atom test) (bindings_atom binds))
                (fun () -> emit_dtree e sub))
            cases;
          match default with
          | Some d -> block e "default" (fun () -> emit_dtree e d)
          | None -> ())
  | Match_tree_types.DGuard { arm_idx; guard; bindings; on_true; on_false } ->
      block e
        (Printf.sprintf "DGuard arm=%d binds=%s" arm_idx (bindings_atom bindings))
        (fun () ->
          block e "guard" (fun () -> emit_expr e guard);
          block e "on_true" (fun () -> emit_dtree e on_true);
          block e "on_false" (fun () -> emit_dtree e on_false))
  | Match_tree_types.DLeaf { arm_idx; bindings } ->
      line e (Printf.sprintf "(DLeaf arm=%d binds=%s)" arm_idx (bindings_atom bindings))
  | Match_tree_types.DFail _ -> line e "(DFail)"

(* Emit [header] as a parenthesised group whose children [body] are indented one
   level. The closing paren sits on its own line so subtrees diff cleanly. *)
and block e header body =
  line e ("(" ^ header);
  e.indent <- e.indent + 1;
  body ();
  e.indent <- e.indent - 1;
  line e ")"

(* ---- Declarations ---- *)

let rec emit_decl e (d : tdecl) =
  match d with
  | TDLet (n, te) -> block e (Printf.sprintf "TDLet %s" (q n)) (fun () -> emit_expr e te)
  | TDLetMut (n, te) -> block e (Printf.sprintf "TDLetMut %s" (q n)) (fun () -> emit_expr e te)
  | TDLetRec (n, te) -> block e (Printf.sprintf "TDLetRec %s" (q n)) (fun () -> emit_expr e te)
  | TDExpr te -> block e "TDExpr" (fun () -> emit_expr e te)
  | TDLetRecAnd binds ->
      block e "TDLetRecAnd" (fun () ->
          List.iter
            (fun (n, te) -> block e (Printf.sprintf "and %s" (q n)) (fun () -> emit_expr e te))
            binds)
  | TDType (n, _) -> line e (Printf.sprintf "(TDType %s)" (q n))
  | TDClass n -> line e (Printf.sprintf "(TDClass %s)" (q n))
  | TDEffect n -> line e (Printf.sprintf "(TDEffect %s)" (q n))
  | TDExtern (n, sc) -> line e (Printf.sprintf "(TDExtern %s %s)" (q n) (scheme_atom sc))
  | TDFfi (n, _sc, sym, _ps, _r) -> line e (Printf.sprintf "(TDFfi %s %s)" (q n) (q sym))
  | TDOpen pairs ->
      line e
        (Printf.sprintf "(TDOpen %s)"
           (String.concat " " (List.map (fun (a, b) -> q (a ^ "." ^ b)) pairs)))
  | TDModule (n, decls, _schemes) ->
      block e (Printf.sprintf "TDModule %s" (q n)) (fun () -> List.iter (emit_decl e) decls)

(* Constraint elaboration generates dictionary/evidence parameters named
   `__dict_<Class>_<id>` and `__ev_<id>`, where <id> derives from the constrained
   type variable's internal id. The two compilers allocate those ids in different
   orders, so the SAME elaborated program gets alpha-equivalent-but-differently-
   numbered dict params (e.g. `__dict_Show_0`/`_1` swapped). That is a naming
   difference, not a structural one. Canonicalize it: rewrite every `__dict_…` /
   `__ev_…` token to a fresh `__dict$0`, `__dict$1`, … in first-appearance order
   over the whole dump. Both compilers apply the same rewrite to alpha-equivalent
   dumps, so the canonical forms match; a genuine structural divergence still
   shows because the rewrite is order-preserving, not structure-erasing. *)
let canonicalize_gensyms (s : string) : string =
  let n = String.length s in
  let buf = Buffer.create n in
  let map = ref [] (* (original_token, canonical) *) in
  let counter = ref 0 in
  let is_ident_char c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9') || c = '_'
  in
  let starts_at i prefix =
    let pl = String.length prefix in
    i + pl <= n && String.sub s i pl = prefix
  in
  let rec assoc k = function
    | [] -> None
    | (a, b) :: _ when a = k -> Some b
    | _ :: rest -> assoc k rest
  in
  let i = ref 0 in
  while !i < n do
    if starts_at !i "__dict_" || starts_at !i "__ev_" then begin
      let j = ref !i in
      while !j < n && is_ident_char s.[!j] do incr j done;
      let tok = String.sub s !i (!j - !i) in
      let canon =
        match assoc tok !map with
        | Some c -> c
        | None ->
            let c = Printf.sprintf "__dict$%d" !counter in
            incr counter;
            map := (tok, c) :: !map;
            c
      in
      Buffer.add_string buf canon;
      i := !j
    end
    else begin
      Buffer.add_char buf s.[!i];
      incr i
    end
  done;
  Buffer.contents buf

(* Serialize a lowered program to a deterministic S-expression string. *)
let serialize_program (program : tprogram) : string =
  let e = make () in
  List.iter (emit_decl e) program;
  canonicalize_gensyms (Buffer.contents e.buf)
