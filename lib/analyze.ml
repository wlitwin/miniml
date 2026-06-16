(* Static analysis of typed AST for optimization opportunities *)

open Typechecker

type stats = {
  mutable total_nodes : int;
  (* Constant folding *)
  mutable const_fold_int : int; (* TEBinop on two TEInt *)
  mutable const_fold_float : int; (* TEBinop on two TEFloat *)
  mutable const_fold_bool : int;
      (* TEBinop on two TEBool; TEUnop Not on TEBool *)
  mutable const_fold_string : int; (* Concat on two TEString *)
  mutable const_fold_if : int; (* TEIf with TEBool condition *)
  (* Dead code *)
  mutable dead_let : int; (* TELet where binding is unused in body *)
  mutable dead_let_names : string list; (* names of dead lets, for reporting *)
  (* Inlining candidates *)
  mutable single_use_let : int; (* TELet where binding is used exactly once *)
  mutable beta_reducible : int; (* TEApp(TEFun, arg) — immediate beta-redex *)
  (* Known-constructor elimination *)
  mutable known_ctor_match : int;
      (* TEMatch on TEConstruct — statically resolvable *)
  mutable known_tuple_match : int; (* TEMatch on TETuple *)
  mutable known_bool_match : int; (* TEMatch on TEBool *)
  (* Function size distribution *)
  mutable fun_sizes : int list; (* body sizes of all TEFun nodes *)
  (* Let binding stats *)
  mutable total_lets : int;
  mutable total_letrecs : int;
  (* Identity operations *)
  mutable identity_ops : int; (* x + 0, x * 1, x ^ "", etc. *)
  (* Nested lets *)
  mutable let_in_let_rhs : int; (* let x = (let y = ... in ...) in ... *)
  (* Single-use let breakdown *)
  mutable single_use_alias : int; (* let x = y — trivial alias *)
  mutable single_use_literal : int; (* let x = <literal> *)
  mutable single_use_small : int; (* let x = <small expr, <=5 nodes> *)
  mutable single_use_large : int; (* let x = <larger expr> *)
  (* Pipeline patterns *)
  mutable pipe_chains : int; (* |> chains — potential for fusion *)
  mutable partial_app : int; (* TEFun wrapping a single TEApp — eta-reducible *)
  (* Cross-pass opportunities *)
  mutable match_on_let : int;
      (* match (let x = e1 in e2) — inline could expose known ctor *)
  mutable if_on_app : int;
      (* if f(...) — function inlining could make const-foldable *)
  mutable total_apps : int; (* total function applications *)
  mutable app_of_var : int;
      (* f(x) where f is a variable — potential inline target *)
  mutable app_of_field : int; (* obj.method(x) — virtual dispatch *)
}

let create_stats () =
  {
    total_nodes = 0;
    const_fold_int = 0;
    const_fold_float = 0;
    const_fold_bool = 0;
    const_fold_string = 0;
    const_fold_if = 0;
    dead_let = 0;
    dead_let_names = [];
    single_use_let = 0;
    beta_reducible = 0;
    known_ctor_match = 0;
    known_tuple_match = 0;
    known_bool_match = 0;
    fun_sizes = [];
    total_lets = 0;
    total_letrecs = 0;
    identity_ops = 0;
    let_in_let_rhs = 0;
    single_use_alias = 0;
    single_use_literal = 0;
    single_use_small = 0;
    single_use_large = 0;
    pipe_chains = 0;
    partial_app = 0;
    match_on_let = 0;
    if_on_app = 0;
    total_apps = 0;
    app_of_var = 0;
    app_of_field = 0;
  }

(* Count AST nodes in an expression *)
let rec expr_size te =
  let n = ref 1 in
  iter_texpr_children (fun child -> n := !n + expr_size child) te;
  !n

(* Count free occurrences of a variable name in an expression, correctly
   handling shadowing (delegates to the optimizer's shadowing-aware counter so
   statistics match what the optimizer actually sees). *)
let count_var_uses name te = Texpr_opt.count_free_uses name te

(* Check if something is a literal *)
let is_literal te =
  match te.expr with
  | TEInt _ | TEFloat _ | TEBool _ | TEString _ | TEByte _ | TERune _ | TEUnit
  | TENil ->
      true
  | _ -> false

(* Check for identity operations like x + 0, x * 1, etc. *)
let is_identity_op te =
  match te.expr with
  | TEBinop (Ast.Add, l, r) -> (
      match (l.expr, r.expr) with TEInt 0, _ | _, TEInt 0 -> true | _ -> false)
  | TEBinop (Ast.Sub, _, r) -> (
      match r.expr with TEInt 0 -> true | _ -> false)
  | TEBinop (Ast.Mul, l, r) -> (
      match (l.expr, r.expr) with TEInt 1, _ | _, TEInt 1 -> true | _ -> false)
  | TEBinop (Ast.Concat, l, r) -> (
      match (l.expr, r.expr) with
      | TEString "", _ | _, TEString "" -> true
      | _ -> false)
  | TEBinop (Ast.Or, l, r) -> (
      match (l.expr, r.expr) with
      | TEBool false, _ | _, TEBool false -> true
      | _ -> false)
  | TEBinop (Ast.And, l, r) -> (
      match (l.expr, r.expr) with
      | TEBool true, _ | _, TEBool true -> true
      | _ -> false)
  | _ -> false

let analyze_expr stats te =
  let rec go te =
    stats.total_nodes <- stats.total_nodes + 1;
    (* Constant folding opportunities *)
    (match te.expr with
    | TEBinop (_, l, r) -> (
        match (l.expr, r.expr) with
        | TEInt _, TEInt _ -> stats.const_fold_int <- stats.const_fold_int + 1
        | TEFloat _, TEFloat _ ->
            stats.const_fold_float <- stats.const_fold_float + 1
        | TEBool _, TEBool _ ->
            stats.const_fold_bool <- stats.const_fold_bool + 1
        | TEString _, TEString _ ->
            stats.const_fold_string <- stats.const_fold_string + 1
        | _ -> ())
    | TEUnop (Ast.Not, e) -> (
        match e.expr with
        | TEBool _ -> stats.const_fold_bool <- stats.const_fold_bool + 1
        | _ -> ())
    | TEUnop (Ast.Neg, e) -> (
        match e.expr with
        | TEInt _ -> stats.const_fold_int <- stats.const_fold_int + 1
        | TEFloat _ -> stats.const_fold_float <- stats.const_fold_float + 1
        | _ -> ())
    | _ -> ());
    (* Constant-condition if *)
    (match te.expr with
    | TEIf ({ expr = TEBool _; _ }, _, _) ->
        stats.const_fold_if <- stats.const_fold_if + 1
    | _ -> ());
    (* Identity ops *)
    if is_identity_op te then stats.identity_ops <- stats.identity_ops + 1;
    (* Dead/single-use let analysis *)
    (match te.expr with
    | TELet (name, _, rhs, body) ->
        stats.total_lets <- stats.total_lets + 1;
        let uses = count_var_uses name body in
        if uses = 0 then begin
          stats.dead_let <- stats.dead_let + 1;
          stats.dead_let_names <- name :: stats.dead_let_names
        end;
        if uses = 1 then begin
          stats.single_use_let <- stats.single_use_let + 1;
          match rhs.expr with
          | TEVar _ -> stats.single_use_alias <- stats.single_use_alias + 1
          | _ when is_literal rhs ->
              stats.single_use_literal <- stats.single_use_literal + 1
          | _ ->
              let sz = expr_size rhs in
              if sz <= 5 then
                stats.single_use_small <- stats.single_use_small + 1
              else stats.single_use_large <- stats.single_use_large + 1
        end
    | TELetRec _ -> stats.total_letrecs <- stats.total_letrecs + 1
    | _ -> ());
    (* Beta-reducible: (fun x -> body) arg *)
    (match te.expr with
    | TEApp ({ expr = TEFun _; _ }, _) ->
        stats.beta_reducible <- stats.beta_reducible + 1
    | _ -> ());
    (* Known-constructor match *)
    (match te.expr with
    | TEMatch (scrut, _, _) -> (
        match scrut.expr with
        | TEConstruct _ -> stats.known_ctor_match <- stats.known_ctor_match + 1
        | TETuple _ -> stats.known_tuple_match <- stats.known_tuple_match + 1
        | TEBool _ -> stats.known_bool_match <- stats.known_bool_match + 1
        | _ -> ())
    | _ -> ());
    (* Function sizes *)
    (match te.expr with
    | TEFun (_, body, _) -> stats.fun_sizes <- expr_size body :: stats.fun_sizes
    | _ -> ());
    (* Let-in-let RHS *)
    (match te.expr with
    | TELet (_, _, rhs, _) -> (
        match rhs.expr with
        | TELet _ | TELetRec _ ->
            stats.let_in_let_rhs <- stats.let_in_let_rhs + 1
        | _ -> ())
    | _ -> ());
    (* Pipe chains: x |> f |> g — shows up as TEApp(TEApp(Pipe, ...), ...) or TEBinop(Pipe) *)
    (match te.expr with
    | TEBinop (Ast.Pipe, { expr = TEBinop (Ast.Pipe, _, _); _ }, _) ->
        stats.pipe_chains <- stats.pipe_chains + 1
    | _ -> ());
    (* Eta-reducible: fun x -> f x *)
    (match te.expr with
    | TEFun (param, { expr = TEApp (_, { expr = TEVar x; _ }); _ }, _)
      when String.equal param x ->
        stats.partial_app <- stats.partial_app + 1
    | _ -> ());
    (* Cross-pass: match on let-bound value *)
    (match te.expr with
    | TEMatch ({ expr = TELet _; _ }, _, _) ->
        stats.match_on_let <- stats.match_on_let + 1
    | _ -> ());
    (* Cross-pass: if on function call *)
    (match te.expr with
    | TEIf ({ expr = TEApp _; _ }, _, _) ->
        stats.if_on_app <- stats.if_on_app + 1
    | _ -> ());
    (* Application analysis *)
    (match te.expr with
    | TEApp _ -> (
        stats.total_apps <- stats.total_apps + 1;
        (* Unwrap curried applications to find the function head *)
        let rec get_head e =
          match e.expr with TEApp (f, _) -> get_head f | _ -> e
        in
        let head = get_head te in
        match head.expr with
        | TEVar _ -> stats.app_of_var <- stats.app_of_var + 1
        | TEField _ -> stats.app_of_field <- stats.app_of_field + 1
        | _ -> ())
    | _ -> ());
    (* Recurse *)
    iter_texpr_children go te
  in
  go te

let rec analyze_decl stats decl =
  match decl with
  | TDLet (_, e) | TDLetMut (_, e) | TDLetRec (_, e) | TDExpr e ->
      analyze_expr stats e
  | TDLetRecAnd binds -> List.iter (fun (_, e) -> analyze_expr stats e) binds
  | TDModule (_, decls, _) -> List.iter (analyze_decl stats) decls
  | TDType _ | TDClass _ | TDEffect _ | TDExtern _ | TDFfi _ | TDOpen _ -> ()

let analyze_program (program : tprogram) =
  let stats = create_stats () in
  List.iter (analyze_decl stats) program;
  stats

let print_stats stats =
  Printf.printf "\n=== Typed AST Optimization Analysis ===\n\n";
  Printf.printf "Total AST nodes: %d\n\n" stats.total_nodes;

  Printf.printf "--- Constant Folding Opportunities ---\n";
  Printf.printf "  Int arithmetic on literals:    %d\n" stats.const_fold_int;
  Printf.printf "  Float arithmetic on literals:  %d\n" stats.const_fold_float;
  Printf.printf "  Boolean ops on literals:       %d\n" stats.const_fold_bool;
  Printf.printf "  String concat on literals:     %d\n" stats.const_fold_string;
  Printf.printf "  If with constant condition:    %d\n" stats.const_fold_if;
  Printf.printf "  Identity operations (x+0 etc): %d\n" stats.identity_ops;
  let total_const =
    stats.const_fold_int + stats.const_fold_float + stats.const_fold_bool
    + stats.const_fold_string + stats.const_fold_if + stats.identity_ops
  in
  Printf.printf "  TOTAL constant fold:           %d (%.1f%% of nodes)\n\n"
    total_const
    (100.0 *. float_of_int total_const /. float_of_int (max 1 stats.total_nodes));

  Printf.printf "--- Dead Code Elimination ---\n";
  Printf.printf "  Total let bindings:            %d\n" stats.total_lets;
  Printf.printf "  Total letrec bindings:         %d\n" stats.total_letrecs;
  Printf.printf "  Dead let (0 uses in body):     %d (%.1f%% of lets)\n"
    stats.dead_let
    (100.0
    *. float_of_int stats.dead_let
    /. float_of_int (max 1 stats.total_lets));
  if stats.dead_let_names <> [] then begin
    let names = List.rev stats.dead_let_names in
    let shown =
      if List.length names > 20 then
        List.filteri (fun i _ -> i < 20) names @ [ "..." ]
      else names
    in
    Printf.printf "    Examples: %s\n" (String.concat ", " shown)
  end;
  Printf.printf "\n";

  Printf.printf "--- Inlining Candidates ---\n";
  Printf.printf "  Single-use let bindings:       %d (%.1f%% of lets)\n"
    stats.single_use_let
    (100.0
    *. float_of_int stats.single_use_let
    /. float_of_int (max 1 stats.total_lets));
  Printf.printf "    Aliases (let x = y):         %d\n" stats.single_use_alias;
  Printf.printf "    Literals (let x = 42):       %d\n" stats.single_use_literal;
  Printf.printf "    Small (<=5 nodes):           %d\n" stats.single_use_small;
  Printf.printf "    Larger (>5 nodes):           %d\n" stats.single_use_large;
  Printf.printf "  Beta-reducible (fun applied):  %d\n" stats.beta_reducible;
  Printf.printf "  Eta-reducible (fun x -> f x):  %d\n" stats.partial_app;
  Printf.printf "  Pipe chains (x |> f |> g):     %d\n" stats.pipe_chains;
  Printf.printf "  Let-in-let (flatten candidate):%d\n\n" stats.let_in_let_rhs;

  Printf.printf "--- Known-Constructor Match ---\n";
  Printf.printf "  Match on known constructor:    %d\n" stats.known_ctor_match;
  Printf.printf "  Match on known tuple:          %d\n" stats.known_tuple_match;
  Printf.printf "  Match on known bool:           %d\n" stats.known_bool_match;
  let total_known =
    stats.known_ctor_match + stats.known_tuple_match + stats.known_bool_match
  in
  Printf.printf "  TOTAL statically resolvable:   %d\n\n" total_known;

  Printf.printf "--- Function Size Distribution ---\n";
  let sorted = List.sort compare stats.fun_sizes in
  let n = List.length sorted in
  if n > 0 then begin
    let arr = Array.of_list sorted in
    Printf.printf "  Total functions:               %d\n" n;
    Printf.printf "  Min body size:                 %d nodes\n" arr.(0);
    Printf.printf "  Max body size:                 %d nodes\n" arr.(n - 1);
    Printf.printf "  Median body size:              %d nodes\n" arr.(n / 2);
    let avg = float_of_int (List.fold_left ( + ) 0 sorted) /. float_of_int n in
    Printf.printf "  Mean body size:                %.1f nodes\n" avg;
    let small = List.length (List.filter (fun s -> s <= 5) sorted) in
    let medium = List.length (List.filter (fun s -> s > 5 && s <= 20) sorted) in
    let large =
      List.length (List.filter (fun s -> s > 20 && s <= 100) sorted)
    in
    let huge = List.length (List.filter (fun s -> s > 100) sorted) in
    Printf.printf
      "  Tiny (<=5 nodes):              %d (%.0f%%) — trivially inlinable\n"
      small
      (100.0 *. float_of_int small /. float_of_int n);
    Printf.printf
      "  Small (6-20 nodes):            %d (%.0f%%) — likely inlinable\n" medium
      (100.0 *. float_of_int medium /. float_of_int n);
    Printf.printf "  Medium (21-100 nodes):         %d (%.0f%%)\n" large
      (100.0 *. float_of_int large /. float_of_int n);
    Printf.printf "  Large (>100 nodes):            %d (%.0f%%)\n" huge
      (100.0 *. float_of_int huge /. float_of_int n)
  end;

  Printf.printf "--- Application Analysis ---\n";
  Printf.printf "  Total applications:            %d\n" stats.total_apps;
  Printf.printf "  Direct calls (f x):            %d (%.0f%%)\n"
    stats.app_of_var
    (100.0
    *. float_of_int stats.app_of_var
    /. float_of_int (max 1 stats.total_apps));
  Printf.printf "  Method calls (obj.m x):        %d (%.0f%%)\n"
    stats.app_of_field
    (100.0
    *. float_of_int stats.app_of_field
    /. float_of_int (max 1 stats.total_apps));
  Printf.printf "  Other (closures, etc):         %d\n\n"
    (stats.total_apps - stats.app_of_var - stats.app_of_field);

  Printf.printf "--- Cross-Pass Opportunities ---\n";
  Printf.printf
    "  match on let-bound value:      %d (inlining let could expose known ctor)\n"
    stats.match_on_let;
  Printf.printf
    "  if on function call:           %d (inlining callee could make \
     const-foldable)\n\n"
    stats.if_on_app;

  Printf.printf "\n--- Estimated Impact Summary ---\n";
  let inlinable_single = List.length (List.filter (fun s -> s <= 20) sorted) in
  let inlinable_single = min inlinable_single stats.single_use_let in
  Printf.printf
    "  Single-use + small body lets:  ~%d nodes eliminable via inlining\n"
    inlinable_single;
  Printf.printf "  Dead let elimination:          ~%d let bindings removable\n"
    stats.dead_let;
  Printf.printf "  Constant folding:              ~%d expressions reducible\n"
    total_const;
  Printf.printf "  Known-match elimination:       ~%d match exprs reducible\n"
    total_known;
  Printf.printf
    "  Beta reduction:                ~%d function applications eliminable\n"
    stats.beta_reducible;
  let total_reducible =
    inlinable_single + stats.dead_let + total_const + total_known
    + stats.beta_reducible
  in
  Printf.printf "  TOTAL estimated reductions:    ~%d (%.1f%% of %d nodes)\n"
    total_reducible
    (100.0
    *. float_of_int total_reducible
    /. float_of_int (max 1 stats.total_nodes))
    stats.total_nodes
