exception Error of string

let wrap_errors f =
  try f () with
  | Lexer.Lex_error (msg, loc) ->
      raise
        (Error (Printf.sprintf "Lex error at %d:%d: %s" loc.line loc.col msg))
  | Parser.Parse_error (msg, loc) ->
      raise
        (Error (Printf.sprintf "Parse error at %d:%d: %s" loc.line loc.col msg))
  | Typechecker.Type_error (msg, loc) ->
      if loc.line > 0 then
        raise (Error (Printf.sprintf "Type error at line %d: %s" loc.line msg))
      else raise (Error (Printf.sprintf "Type error: %s" msg))
  | Compiler.Compile_error msg ->
      raise (Error (Printf.sprintf "Compile error: %s" msg))
  | Vm.Runtime_error msg ->
      raise (Error (Printf.sprintf "Runtime error: %s" msg))

(* ---- Built-in definitions ---- *)
(* Core builtin IMPLEMENTATIONS, registered by name. Their type signatures live
   in stdlib/builtins.mml — the single source of truth shared with the
   self-hosted compiler (self_host/main.mml). At startup, setup_default_classes
   typechecks stdlib/builtins.mml (via Stdlib_sources.builtins) and cross-checks
   that every extern there has an implementation here and vice versa.

   Operator builtins (mod, ^, &&, ||, not) are declared there with the
   parenthesized form: `extern ( ^ ) : string -> string -> string`. *)

type builtin_def = {
  name : string;
  arity : int;
  impl : Bytecode.value list -> Bytecode.value;
}

let as_int = function
  | Bytecode.VInt n -> n
  | _ -> raise (Vm.Runtime_error "expected int")

let as_float = function
  | Bytecode.VFloat f -> f
  | _ -> raise (Vm.Runtime_error "expected float")

let as_bool = function
  | Bytecode.VBool b -> b
  | _ -> raise (Vm.Runtime_error "expected bool")

let as_string = function
  | Bytecode.VString s -> s
  | v ->
      raise
        (Vm.Runtime_error
           (Printf.sprintf "expected string, got %s" (Bytecode.pp_value v)))

let as_byte = function
  | Bytecode.VByte n -> n
  | _ -> raise (Vm.Runtime_error "expected byte")

let as_rune = function
  | Bytecode.VRune n -> n
  | _ -> raise (Vm.Runtime_error "expected rune")

let as_array = function
  | Bytecode.VArray a -> a
  | _ -> raise (Vm.Runtime_error "expected array")

let builtins output_fn : builtin_def list =
  [
    (* Int modulo (stays as builtin, not in Num class) *)
    {
      name = "mod";
      arity = 2;
      impl =
        (fun args ->
          let b = as_int (List.nth args 1) in
          if b = 0 then raise (Vm.Runtime_error "modulo by zero");
          VInt (as_int (List.nth args 0) mod b));
    };
    (* Print *)
    {
      name = "print";
      arity = 1;
      impl =
        (fun args ->
          !output_fn (Bytecode.pp_value (List.nth args 0));
          VUnit);
    };
    (* Failwith *)
    {
      name = "failwith";
      arity = 1;
      impl =
        (fun args -> raise (Vm.Runtime_error (as_string (List.nth args 0))));
    };
    (* String *)
    {
      name = "^";
      arity = 2;
      impl =
        (fun args ->
          VString (as_string (List.nth args 0) ^ as_string (List.nth args 1)));
    };
    (* Boolean *)
    {
      name = "&&";
      arity = 2;
      impl =
        (fun args ->
          VBool (as_bool (List.nth args 0) && as_bool (List.nth args 1)));
    };
    {
      name = "||";
      arity = 2;
      impl =
        (fun args ->
          VBool (as_bool (List.nth args 0) || as_bool (List.nth args 1)));
    };
    {
      name = "not";
      arity = 1;
      impl = (fun args -> VBool (not (as_bool (List.nth args 0))));
    };
    (* Physical equality *)
    {
      name = "phys_equal";
      arity = 2;
      impl = (fun args -> VBool (List.nth args 0 == List.nth args 1));
    };
    (* Conversions *)
    {
      name = "float_of_int";
      arity = 1;
      impl = (fun args -> VFloat (Float.of_int (as_int (List.nth args 0))));
    };
    {
      name = "int_of_float";
      arity = 1;
      impl = (fun args -> VInt (Float.to_int (as_float (List.nth args 0))));
    };
    (* String conversions *)
    {
      name = "string_of_int";
      arity = 1;
      impl = (fun args -> VString (string_of_int (as_int (List.nth args 0))));
    };
    {
      name = "string_of_float";
      arity = 1;
      impl =
        (fun args -> VString (Printf.sprintf "%g" (as_float (List.nth args 0))));
    };
    {
      name = "string_of_bool";
      arity = 1;
      impl =
        (fun args ->
          VString (if as_bool (List.nth args 0) then "true" else "false"));
    };
    (* Array operations (array_get and array_length kept for internal use by Iter/Show instances) *)
    {
      name = "array_get";
      arity = 2;
      impl =
        (fun args ->
          let arr = as_array (List.nth args 0) in
          let idx = as_int (List.nth args 1) in
          if idx < 0 || idx >= Array.length arr then
            raise
              (Vm.Runtime_error
                 (Printf.sprintf "array index %d out of bounds (length %d)" idx
                    (Array.length arr)))
          else arr.(idx));
    };
    {
      name = "array_length";
      arity = 1;
      impl =
        (fun args ->
          let arr = as_array (List.nth args 0) in
          VInt (Array.length arr));
    };
    (* Byte primitives — type conversions need native support *)
    {
      name = "__byte_to_int";
      arity = 1;
      impl = (fun args -> VInt (as_byte (List.nth args 0)));
    };
    {
      name = "__byte_of_int";
      arity = 1;
      impl = (fun args -> VByte (as_int (List.nth args 0) land 0xFF));
    };
    {
      name = "__byte_to_string";
      arity = 1;
      impl =
        (fun args ->
          let b = as_byte (List.nth args 0) in
          VString (String.make 1 (Char.chr b)));
    };
    (* Rune primitives — type conversions and UTF-8 encoding need native support *)
    {
      name = "__rune_to_int";
      arity = 1;
      impl = (fun args -> VInt (as_rune (List.nth args 0)));
    };
    {
      name = "__rune_of_int";
      arity = 1;
      impl = (fun args -> VRune (as_int (List.nth args 0)));
    };
    {
      name = "__rune_to_string";
      arity = 1;
      impl =
        (fun args ->
          let cp = as_rune (List.nth args 0) in
          VString (Utf8.rune_to_string cp));
    };
    (* Math primitives — float operations need native support *)
    {
      name = "__math_pow";
      arity = 2;
      impl =
        (fun args ->
          VFloat
            (Float.pow
               (as_float (List.nth args 0))
               (as_float (List.nth args 1))));
    };
    {
      name = "__math_sqrt";
      arity = 1;
      impl = (fun args -> VFloat (Float.sqrt (as_float (List.nth args 0))));
    };
    {
      name = "__math_floor";
      arity = 1;
      impl =
        (fun args -> VInt (int_of_float (floor (as_float (List.nth args 0)))));
    };
    {
      name = "__math_ceil";
      arity = 1;
      impl =
        (fun args -> VInt (int_of_float (ceil (as_float (List.nth args 0)))));
    };
    {
      name = "__math_round";
      arity = 1;
      impl =
        (fun args ->
          VInt (Float.to_int (Float.round (as_float (List.nth args 0)))));
    };
    {
      name = "__float_bits_hex";
      arity = 1;
      impl =
        (fun args ->
          VString
            (Printf.sprintf "%LX"
               (Int64.bits_of_float (as_float (List.nth args 0)))));
    };
    (* Format specifier builtins *)
    {
      name = "__fmt_float";
      arity = 2;
      impl =
        (fun args ->
          VString
            (Printf.sprintf "%.*f"
               (as_int (List.nth args 0))
               (as_float (List.nth args 1))));
    };
    {
      name = "__fmt_hex";
      arity = 1;
      impl =
        (fun args -> VString (Printf.sprintf "%x" (as_int (List.nth args 0))));
    };
    {
      name = "__fmt_hex_upper";
      arity = 1;
      impl =
        (fun args -> VString (Printf.sprintf "%X" (as_int (List.nth args 0))));
    };
    {
      name = "__fmt_oct";
      arity = 1;
      impl =
        (fun args -> VString (Printf.sprintf "%o" (as_int (List.nth args 0))));
    };
    {
      name = "__fmt_bin";
      arity = 1;
      impl =
        (fun args ->
          let n = as_int (List.nth args 0) in
          let rec go n acc =
            if n = 0 then acc else go (n lsr 1) (string_of_int (n land 1) ^ acc)
          in
          VString (if n = 0 then "0" else go n ""));
    };
    {
      name = "__fmt_zero_pad";
      arity = 2;
      impl =
        (fun args ->
          let width = as_int (List.nth args 0) in
          let s = as_string (List.nth args 1) in
          let pad = max 0 (width - String.length s) in
          VString (String.make pad '0' ^ s));
    };
    {
      name = "__fmt_pad_left";
      arity = 2;
      impl =
        (fun args ->
          let width = as_int (List.nth args 0) in
          let s = as_string (List.nth args 1) in
          let pad = max 0 (width - String.length s) in
          VString (String.make pad ' ' ^ s));
    };
    {
      name = "__fmt_pad_right";
      arity = 2;
      impl =
        (fun args ->
          let width = as_int (List.nth args 0) in
          let s = as_string (List.nth args 1) in
          let pad = max 0 (width - String.length s) in
          VString (s ^ String.make pad ' '));
    };
    {
      name = "__show_value";
      arity = 1;
      impl =
        (fun args -> Bytecode.VString (Bytecode.pp_value (List.nth args 0)));
    };
  ]

let copy_fiber (f : Bytecode.fiber) : Bytecode.fiber =
  let new_stack = Array.copy f.fiber_stack in
  let new_frames =
    List.map
      (fun (frame : Bytecode.call_frame) ->
        Bytecode.
          {
            frame_closure = frame.frame_closure;
            frame_ip = frame.frame_ip;
            frame_base_sp = frame.frame_base_sp;
          })
      f.fiber_frames
  in
  Bytecode.
    {
      fiber_stack = new_stack;
      fiber_sp = f.fiber_sp;
      fiber_frames = new_frames;
      fiber_frame_depth = f.fiber_frame_depth;
      fiber_extra_args =
        List.map (fun l -> List.map (fun v -> v) l) f.fiber_extra_args;
      (* Per-fiber control state: copied so a multishot resume re-enters
         loops/functions with intact markers (entries are immutable). *)
      fiber_control = f.fiber_control;
      fiber_return = f.fiber_return;
    }

let copy_continuation = function
  | Bytecode.VContinuation cd ->
      (* Copying an already-resumed continuation is an error (semantics.md
         §12): the fiber's stack was consumed by the resume, so the "copy"
         would be a corpse — resuming it crashes. Copy before resuming. *)
      if cd.cd_used then
        raise
          (Vm.Runtime_error "cannot copy an already resumed continuation");
      let new_fiber = copy_fiber cd.cd_fiber in
      let new_body_fiber =
        if cd.cd_body_fiber == cd.cd_fiber then new_fiber
        else copy_fiber cd.cd_body_fiber
      in
      Bytecode.VContinuation
        {
          cd_fiber = new_fiber;
          cd_return_handler = cd.cd_return_handler;
          cd_op_handlers = cd.cd_op_handlers;
          cd_body_fiber = new_body_fiber;
          cd_intermediate_handlers = cd.cd_intermediate_handlers;
          cd_used = false;
        }
  | v ->
      raise
        (Vm.Runtime_error
           (Printf.sprintf "copy: expected continuation, got %s"
              (Bytecode.pp_value v)))

let make_external (b : builtin_def) : Bytecode.value =
  Bytecode.VExternal
    { ext_name = b.name; ext_arity = b.arity; ext_fn = b.impl; ext_args = [] }

(* ---- State with built-ins ---- *)

type repl_state = {
  ctx : Typechecker.ctx;
  global_names : string Dynarray.t;
  mutable_globals : (string, unit) Hashtbl.t;
  globals : (int, Bytecode.value) Hashtbl.t;
  output_fn : (string -> unit) ref;
  argv : string array ref;
  setup_protos : Bytecode.prototype list;
  setup_typed : (Types.type_env * Typechecker.tprogram) list;
  setup_typed_unlowered : (Types.type_env * Typechecker.tprogram) list;
      (* pre-lowering (post-transform_constraints) ASTs of the same setup
         programs — consumed by the Oracle reference interpreter, which is
         independent of the lowering passes *)
  state_ref : repl_state option ref;
}

let builtin_table (state : repl_state) : Deserialize.builtin_table =
  let tbl : Deserialize.builtin_table = Hashtbl.create 256 in
  Hashtbl.iter
    (fun _idx value ->
      match value with
      | Bytecode.VExternal ext ->
          if not (Hashtbl.mem tbl ext.ext_name) then
            Hashtbl.replace tbl ext.ext_name
              Deserialize.{ arity = ext.ext_arity; impl = ext.ext_fn }
      | Bytecode.VRecord (_shape, values) ->
          Array.iter
            (fun v ->
              match v with
              | Bytecode.VExternal ext ->
                  if not (Hashtbl.mem tbl ext.ext_name) then
                    Hashtbl.replace tbl ext.ext_name
                      Deserialize.{ arity = ext.ext_arity; impl = ext.ext_fn }
              | _ -> ())
            values
      | _ -> ())
    state.globals;
  tbl

let setup_builtins () =
  let output_fn = ref print_endline in
  let argv = ref Sys.argv in
  let global_names = Dynarray.create () in
  let globals = Hashtbl.create 64 in
  (* Register builtin IMPLEMENTATIONS by name (and under the Stdlib. prefix).
     Types are bound later, when setup_default_classes typechecks
     stdlib/builtins.mml (the single source of truth for builtin signatures)
     and cross-checks it against these registrations. *)
  let register_impl name arity impl =
    let ext =
      Bytecode.VExternal
        { ext_name = name; ext_arity = arity; ext_fn = impl; ext_args = [] }
    in
    let idx = Dynarray.length global_names in
    Dynarray.add_last global_names name;
    Hashtbl.replace globals idx ext;
    (* Also register under Stdlib. prefix *)
    let sidx = Dynarray.length global_names in
    Dynarray.add_last global_names ("Stdlib." ^ name);
    Hashtbl.replace globals sidx ext
  in
  List.iter
    (fun (b : builtin_def) -> register_impl b.name b.arity b.impl)
    (builtins output_fn);
  (* copy_continuation's impl closes over the copy_continuation function above,
     so it can't live in the builtins list (defined before it). *)
  register_impl "copy_continuation" 1 (fun args ->
      copy_continuation (List.nth args 0));
  (* No-op cache functions — used by the self-hosted compiler's extern declarations.
     In the bytecode VM, caching is not needed (setup runs once per process).
     The self-host declares their types itself (`extern __cache_has : ...`), so
     only impls are registered here. *)
  let cache_fns =
    [
      ("__cache_has", 1, fun _args -> Bytecode.VBool false);
      ("__cache_get", 1, fun _args -> Bytecode.VUnit);
      ("__cache_set", 2, fun _args -> Bytecode.VUnit);
    ]
  in
  List.iter
    (fun (name, arity, impl) ->
      let idx = Dynarray.length global_names in
      Dynarray.add_last global_names name;
      Hashtbl.replace globals idx
        (Bytecode.VExternal
           { ext_name = name; ext_arity = arity; ext_fn = impl; ext_args = [] }))
    cache_fns;
  let ctx =
    Typechecker.
      {
        vars = [];
        mutable_vars = [];
        type_env = Types.empty_type_env;
        loop_info = None;
        current_module = None;
        constraint_tvars = [];
        current_eff = Types.EffEmpty;
        return_type = None;
        inside_handler = false;
        return_used = ref false;
        loc = Token.{ line = 0; col = 0; offset = 0 };
      }
  in
  (ctx, global_names, globals, output_fn, argv)

(* ---- Embedding API: register custom external functions ---- *)

let register_external state name ty arity impl =
  let idx = Dynarray.length state.global_names in
  Dynarray.add_last state.global_names name;
  Hashtbl.replace state.globals idx
    (Bytecode.VExternal
       { ext_name = name; ext_arity = arity; ext_fn = impl; ext_args = [] });
  let ctx =
    Typechecker.
      { state.ctx with vars = (name, Types.mono ty) :: state.ctx.vars }
  in
  { state with ctx }

(* ---- Embedding API: register type classes and instances ---- *)

let register_class state ~name ~tyvars ~fundeps ~methods =
  let num_params = List.length tyvars in
  let class_def =
    Types.
      {
        class_name = name;
        class_params = tyvars;
        class_methods = methods;
        class_fundeps = fundeps;
      }
  in
  let vars =
    List.fold_left
      (fun vars (mname, mty) ->
        let max_gen = Typechecker.max_tgen_in_ty mty in
        let quant = max (max_gen + 1) num_params in
        ( mname,
          {
            Types.quant;
            equant = 0;
            pvquant = 0;
            rquant = 0;
            constraints = [];
            record_evidences = [];
            body = mty;
          } )
        :: vars)
      state.ctx.Typechecker.vars methods
  in
  let type_env =
    {
      state.ctx.type_env with
      Types.classes = class_def :: state.ctx.type_env.classes;
    }
  in
  let ctx =
    Typechecker.
      {
        vars;
        mutable_vars = state.ctx.mutable_vars;
        type_env;
        loop_info = None;
        current_module = None;
        constraint_tvars = [];
        current_eff = Types.EffEmpty;
        return_type = None;
        inside_handler = false;
        return_used = ref false;
        loc = Token.{ line = 0; col = 0; offset = 0 };
      }
  in
  { state with ctx }

let register_instance state ~class_name ~tys ~methods =
  let dname = Types.dict_name class_name tys in
  let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) methods in
  let field_names = List.map fst sorted in
  let values = Array.of_list (List.map snd sorted) in
  let dict_value = Bytecode.make_record field_names values in
  let idx = Dynarray.length state.global_names in
  Dynarray.add_last state.global_names dname;
  Hashtbl.replace state.globals idx dict_value;
  (* Also register individual method globals for direct dispatch *)
  List.iter
    (fun (method_name, method_value) ->
      let method_global_name = dname ^ "$" ^ method_name in
      let midx = Dynarray.length state.global_names in
      Dynarray.add_last state.global_names method_global_name;
      Hashtbl.replace state.globals midx method_value)
    sorted;
  let inst_def =
    Types.
      {
        inst_class = class_name;
        inst_tys = tys;
        inst_dict_name = dname;
        inst_constraints = [];
      }
  in
  let type_env =
    {
      state.ctx.type_env with
      Types.instances = inst_def :: state.ctx.type_env.instances;
    }
  in
  let ctx = Typechecker.{ state.ctx with type_env } in
  { state with ctx }

let register_fns state mod_name
    (fns : (string * int * (Bytecode.value list -> Bytecode.value)) list) =
  List.fold_left
    (fun st (fn_name, arity, impl) ->
      let qualified = mod_name ^ "." ^ fn_name in
      let idx = Dynarray.length st.global_names in
      Dynarray.add_last st.global_names qualified;
      Hashtbl.replace st.globals idx
        (Bytecode.VExternal
           {
             ext_name = qualified;
             ext_arity = arity;
             ext_fn = impl;
             ext_args = [];
           });
      st)
    state fns

let register_module state mod_name
    (fns :
      (string * Types.ty * int * (Bytecode.value list -> Bytecode.value)) list)
    =
  let pub_vars = ref [] in
  let state =
    List.fold_left
      (fun st (fn_name, ty, arity, impl) ->
        let qualified = mod_name ^ "." ^ fn_name in
        let idx = Dynarray.length st.global_names in
        Dynarray.add_last st.global_names qualified;
        Hashtbl.replace st.globals idx
          (Bytecode.VExternal
             {
               ext_name = qualified;
               ext_arity = arity;
               ext_fn = impl;
               ext_args = [];
             });
        let max_gen = Typechecker.max_tgen_in_ty ty in
        let scheme =
          if max_gen >= 0 then
            {
              Types.quant = max_gen + 1;
              equant = 0;
              pvquant = 0;
              rquant = 0;
              constraints = [];
              record_evidences = [];
              body = ty;
            }
          else Types.mono ty
        in
        pub_vars := (fn_name, scheme) :: !pub_vars;
        {
          st with
          ctx =
            Typechecker.
              { st.ctx with vars = (qualified, scheme) :: st.ctx.vars };
        })
      state fns
  in
  let minfo =
    Types.
      {
        mod_name;
        mod_pub_vars = !pub_vars;
        mod_pub_mutable_vars = [];
        mod_pub_types = [];
        mod_opaque_types = [];
        mod_newtypes = [];
        mod_pub_constructors = [];
        mod_instances = [];
        mod_submodules = [];
        mod_pub_classes = [];
      }
  in
  let type_env =
    {
      state.ctx.type_env with
      Types.modules = (mod_name, minfo) :: state.ctx.type_env.modules;
    }
  in
  { state with ctx = Typechecker.{ state.ctx with type_env } }

let make_ext name arity fn =
  Bytecode.VExternal
    { ext_name = name; ext_arity = arity; ext_fn = fn; ext_args = [] }

let eval_setup state source =
  let tokens = Lexer.tokenize source in
  let program = Parser.parse_program tokens in
  let ctx', typed_program =
    Typechecker.check_program_in_ctx state.ctx program
  in
  List.iter
    (fun w -> Printf.eprintf "%s\n%!" w)
    (Typechecker.take_warnings ());
  let unlowered = Typechecker.transform_constraints ctx' typed_program in
  let typed_program =
    Pipeline.lower ctx'.Typechecker.type_env unlowered
  in
  let compiled =
    Compiler.compile_program_with_globals ctx'.Typechecker.type_env
      state.global_names state.mutable_globals typed_program
  in
  let _ = Vm.execute_with_globals compiled state.globals in
  {
    state with
    ctx = ctx';
    setup_protos = state.setup_protos @ [ compiled.main ];
    setup_typed =
      state.setup_typed @ [ (ctx'.Typechecker.type_env, typed_program) ];
    setup_typed_unlowered =
      state.setup_typed_unlowered
      @ [ (ctx'.Typechecker.type_env, unlowered) ];
  }

(* Load core builtin type signatures from stdlib/builtins.mml — the single
   source of truth shared with the self-hosted compiler. Implementations were
   registered by name in setup_builtins; the extern declarations bind their
   types. Returns the updated state plus the (name, scheme) list of builtins
   for the Stdlib module. Fails loudly if the signature file and the registered
   implementations disagree. *)
let setup_builtin_signatures state =
  let state = eval_setup state Stdlib_sources.builtins in
  (* The builtins program is the last setup_typed entry; collect its externs. *)
  let _, builtins_program = List.nth state.setup_typed (List.length state.setup_typed - 1) in
  let extern_sigs =
    List.filter_map
      (function
        | Typechecker.TDExtern (name, scheme) -> Some (name, scheme)
        | _ -> None)
      builtins_program
  in
  (* Cross-check: every extern signature must have a registered implementation. *)
  let impl_registered name =
    let found = ref false in
    Dynarray.iter
      (fun n -> if String.equal n name then found := true)
      state.global_names;
    !found
  in
  List.iter
    (fun (name, _) ->
      if not (impl_registered name) then
        raise
          (Error
             (Printf.sprintf
                "builtin '%s' is declared in stdlib/builtins.mml but has no \
                 registered implementation (lib/interp.ml builtins)"
                name)))
    extern_sigs;
  (* Cross-check: every registered implementation must have a signature. *)
  let impl_names =
    List.map (fun (b : builtin_def) -> b.name) (builtins state.output_fn)
    @ [ "copy_continuation" ]
  in
  List.iter
    (fun name ->
      if not (List.mem_assoc name extern_sigs) then
        raise
          (Error
             (Printf.sprintf
                "builtin '%s' has a registered implementation but no \
                 signature in stdlib/builtins.mml"
                name)))
    impl_names;
  (* Bind Stdlib.-qualified names to the same schemes. *)
  let stdlib_qualified =
    List.map (fun (name, scheme) -> ("Stdlib." ^ name, scheme)) extern_sigs
  in
  let ctx =
    Typechecker.
      { state.ctx with vars = stdlib_qualified @ state.ctx.vars }
  in
  ({ state with ctx }, extern_sigs)

let setup_default_classes state =
  (* Bind core builtin types from stdlib/builtins.mml first — later stdlib
     sources (classes.mml, list.mml, ...) reference print/failwith/etc. *)
  let state, stdlib_pub_vars = setup_builtin_signatures state in
  (* Register typeclass primitive extern implementations.
     These are referenced by extern declarations in stdlib/classes.mml *)
  let reg name arity impl =
    let idx = Dynarray.length state.global_names in
    Dynarray.add_last state.global_names name;
    Hashtbl.replace state.globals idx
      (Bytecode.VExternal
         { ext_name = name; ext_arity = arity; ext_fn = impl; ext_args = [] })
  in
  (* Num primitives *)
  reg "__num_add_int" 2 (fun args ->
      VInt (as_int (List.nth args 0) + as_int (List.nth args 1)));
  reg "__num_sub_int" 2 (fun args ->
      VInt (as_int (List.nth args 0) - as_int (List.nth args 1)));
  reg "__num_mul_int" 2 (fun args ->
      VInt (as_int (List.nth args 0) * as_int (List.nth args 1)));
  reg "__num_div_int" 2 (fun args ->
      let b = as_int (List.nth args 1) in
      if b = 0 then raise (Vm.Runtime_error "division by zero");
      VInt (as_int (List.nth args 0) / b));
  reg "__num_neg_int" 1 (fun args -> VInt (-as_int (List.nth args 0)));
  reg "__num_add_float" 2 (fun args ->
      VFloat (as_float (List.nth args 0) +. as_float (List.nth args 1)));
  reg "__num_sub_float" 2 (fun args ->
      VFloat (as_float (List.nth args 0) -. as_float (List.nth args 1)));
  reg "__num_mul_float" 2 (fun args ->
      VFloat (as_float (List.nth args 0) *. as_float (List.nth args 1)));
  reg "__num_div_float" 2 (fun args ->
      VFloat (as_float (List.nth args 0) /. as_float (List.nth args 1)));
  reg "__num_neg_float" 1 (fun args -> VFloat (-.as_float (List.nth args 0)));
  (* Eq primitives *)
  reg "__eq_int" 2 (fun args ->
      VBool (as_int (List.nth args 0) = as_int (List.nth args 1)));
  reg "__neq_int" 2 (fun args ->
      VBool (as_int (List.nth args 0) <> as_int (List.nth args 1)));
  reg "__eq_float" 2 (fun args ->
      VBool (as_float (List.nth args 0) = as_float (List.nth args 1)));
  reg "__neq_float" 2 (fun args ->
      VBool (as_float (List.nth args 0) <> as_float (List.nth args 1)));
  reg "__eq_string" 2 (fun args ->
      VBool (as_string (List.nth args 0) = as_string (List.nth args 1)));
  reg "__neq_string" 2 (fun args ->
      VBool (as_string (List.nth args 0) <> as_string (List.nth args 1)));
  reg "__eq_bool" 2 (fun args ->
      VBool (as_bool (List.nth args 0) = as_bool (List.nth args 1)));
  reg "__neq_bool" 2 (fun args ->
      VBool (as_bool (List.nth args 0) <> as_bool (List.nth args 1)));
  reg "__eq_byte" 2 (fun args ->
      VBool (as_byte (List.nth args 0) = as_byte (List.nth args 1)));
  reg "__neq_byte" 2 (fun args ->
      VBool (as_byte (List.nth args 0) <> as_byte (List.nth args 1)));
  reg "__eq_rune" 2 (fun args ->
      VBool (as_rune (List.nth args 0) = as_rune (List.nth args 1)));
  reg "__neq_rune" 2 (fun args ->
      VBool (as_rune (List.nth args 0) <> as_rune (List.nth args 1)));
  (* Ord primitives *)
  reg "__lt_int" 2 (fun args ->
      VBool (as_int (List.nth args 0) < as_int (List.nth args 1)));
  reg "__gt_int" 2 (fun args ->
      VBool (as_int (List.nth args 0) > as_int (List.nth args 1)));
  reg "__le_int" 2 (fun args ->
      VBool (as_int (List.nth args 0) <= as_int (List.nth args 1)));
  reg "__ge_int" 2 (fun args ->
      VBool (as_int (List.nth args 0) >= as_int (List.nth args 1)));
  reg "__lt_float" 2 (fun args ->
      VBool (as_float (List.nth args 0) < as_float (List.nth args 1)));
  reg "__gt_float" 2 (fun args ->
      VBool (as_float (List.nth args 0) > as_float (List.nth args 1)));
  reg "__le_float" 2 (fun args ->
      VBool (as_float (List.nth args 0) <= as_float (List.nth args 1)));
  reg "__ge_float" 2 (fun args ->
      VBool (as_float (List.nth args 0) >= as_float (List.nth args 1)));
  reg "__lt_string" 2 (fun args ->
      VBool
        (String.compare
           (as_string (List.nth args 0))
           (as_string (List.nth args 1))
        < 0));
  reg "__gt_string" 2 (fun args ->
      VBool
        (String.compare
           (as_string (List.nth args 0))
           (as_string (List.nth args 1))
        > 0));
  reg "__le_string" 2 (fun args ->
      VBool
        (String.compare
           (as_string (List.nth args 0))
           (as_string (List.nth args 1))
        <= 0));
  reg "__ge_string" 2 (fun args ->
      VBool
        (String.compare
           (as_string (List.nth args 0))
           (as_string (List.nth args 1))
        >= 0));
  reg "__lt_byte" 2 (fun args ->
      VBool (as_byte (List.nth args 0) < as_byte (List.nth args 1)));
  reg "__gt_byte" 2 (fun args ->
      VBool (as_byte (List.nth args 0) > as_byte (List.nth args 1)));
  reg "__le_byte" 2 (fun args ->
      VBool (as_byte (List.nth args 0) <= as_byte (List.nth args 1)));
  reg "__ge_byte" 2 (fun args ->
      VBool (as_byte (List.nth args 0) >= as_byte (List.nth args 1)));
  reg "__lt_rune" 2 (fun args ->
      VBool (as_rune (List.nth args 0) < as_rune (List.nth args 1)));
  reg "__gt_rune" 2 (fun args ->
      VBool (as_rune (List.nth args 0) > as_rune (List.nth args 1)));
  reg "__le_rune" 2 (fun args ->
      VBool (as_rune (List.nth args 0) <= as_rune (List.nth args 1)));
  reg "__ge_rune" 2 (fun args ->
      VBool (as_rune (List.nth args 0) >= as_rune (List.nth args 1)));
  (* Bitwise primitives *)
  reg "__band_int" 2 (fun args ->
      VInt (as_int (List.nth args 0) land as_int (List.nth args 1)));
  reg "__bor_int" 2 (fun args ->
      VInt (as_int (List.nth args 0) lor as_int (List.nth args 1)));
  reg "__bxor_int" 2 (fun args ->
      VInt (as_int (List.nth args 0) lxor as_int (List.nth args 1)));
  reg "__bshl_int" 2 (fun args ->
      VInt (as_int (List.nth args 0) lsl as_int (List.nth args 1)));
  reg "__bshr_int" 2 (fun args ->
      VInt (as_int (List.nth args 0) lsr as_int (List.nth args 1)));
  reg "__bnot_int" 1 (fun args -> VInt (lnot (as_int (List.nth args 0))));
  (* Show primitives *)
  reg "__show_int" 1 (fun args ->
      Bytecode.VString (string_of_int (as_int (List.nth args 0))));
  reg "__show_float" 1 (fun args ->
      Bytecode.VString (Printf.sprintf "%g" (as_float (List.nth args 0))));
  reg "__show_bool" 1 (fun args ->
      Bytecode.VString (if as_bool (List.nth args 0) then "true" else "false"));
  reg "__show_string" 1 (fun args -> List.nth args 0);
  reg "__show_unit" 1 (fun _args -> Bytecode.VString "()");
  reg "__show_byte" 1 (fun args ->
      Bytecode.VString (Printf.sprintf "#%02x" (as_byte (List.nth args 0))));
  reg "__show_rune" 1 (fun args ->
      Bytecode.VString (Bytecode.pp_value (List.nth args 0)));
  (* Structural comparison primitives — bypass typeclass dispatch *)
  reg "__structural_eq" 2 (fun args ->
      Bytecode.VBool (Vm.values_equal (List.nth args 0) (List.nth args 1)));
  reg "__structural_neq" 2 (fun args ->
      Bytecode.VBool (not (Vm.values_equal (List.nth args 0) (List.nth args 1))));
  reg "__structural_lt" 2 (fun args ->
      Bytecode.VBool (Vm.values_compare (List.nth args 0) (List.nth args 1) < 0));
  reg "__structural_gt" 2 (fun args ->
      Bytecode.VBool (Vm.values_compare (List.nth args 0) (List.nth args 1) > 0));
  reg "__structural_le" 2 (fun args ->
      Bytecode.VBool (Vm.values_compare (List.nth args 0) (List.nth args 1) <= 0));
  reg "__structural_ge" 2 (fun args ->
      Bytecode.VBool (Vm.values_compare (List.nth args 0) (List.nth args 1) >= 0));
  (* Structural hash — bypass typeclass dispatch *)
  reg "__poly_hash" 1 (fun args ->
      Bytecode.VInt (Vm.value_hash (List.nth args 0)));
  (* Index primitives *)
  reg "__index_at_array" 2 (fun args ->
      let idx = as_int (List.nth args 0) in
      let arr = as_array (List.nth args 1) in
      if idx < 0 || idx >= Array.length arr then
        raise
          (Vm.Runtime_error
             (Printf.sprintf
                "[line 0] array index out of bounds: %d (length %d)" idx
                (Array.length arr)));
      arr.(idx));
  reg "__index_at_string" 2 (fun args ->
      let idx = as_int (List.nth args 0) in
      let s = as_string (List.nth args 1) in
      if idx < 0 || idx >= String.length s then
        raise
          (Vm.Runtime_error
             (Printf.sprintf
                "[line 0] string index out of bounds: %d (length %d)" idx
                (String.length s)));
      Bytecode.VByte (Char.code s.[idx]));
  (* Load all class definitions, primitive instances, option type, etc. from stdlib *)
  let state = eval_setup state Stdlib_sources.classes in
  let state = eval_setup state Stdlib_sources.option_type in
  let state = eval_setup state Stdlib_sources.eq in
  let state = eval_setup state Stdlib_sources.ord in
  let state = eval_setup state Stdlib_sources.iter in
  let state = eval_setup state Stdlib_sources.map_class in
  let state = eval_setup state Stdlib_sources.show in
  (* Build Stdlib module_info and register it *)
  let stdlib_constructors =
    List.filter_map
      (fun (name, info) ->
        if name = "None" || name = "Some" then Some (name, info) else None)
      state.ctx.type_env.Types.constructors
  in
  let stdlib_info =
    Types.
      {
        mod_name = "Stdlib";
        mod_pub_vars = stdlib_pub_vars;
        mod_pub_mutable_vars = [];
        mod_pub_types = [];
        mod_opaque_types = [];
        mod_newtypes = [];
        mod_pub_constructors = stdlib_constructors;
        mod_instances = [];
        mod_submodules = [];
        mod_pub_classes = [];
      }
  in
  let type_env =
    {
      state.ctx.type_env with
      Types.modules =
        ("Stdlib", stdlib_info) :: state.ctx.type_env.Types.modules;
    }
  in
  { state with ctx = Typechecker.{ state.ctx with type_env } }

(* ---- Non-REPL execution ---- *)

(* ---- Dynamic eval support ---- *)

let eval_source state_ref source =
  match !state_ref with
  | None -> raise (Error "eval: interpreter state not initialized")
  | Some state ->
      let tokens = Lexer.tokenize source in
      let program = Parser.parse_program tokens in
      let ctx', typed_program =
        Typechecker.check_program_in_ctx state.ctx program
      in
      let stdlib_programs = List.map snd state.setup_typed in
      let compiled =
        typed_program
        |> Typechecker.transform_constraints ctx'
        |> Pipeline.lower ~stdlib_programs ctx'.Typechecker.type_env
        |> Compiler.compile_program_with_globals ctx'.Typechecker.type_env
             state.global_names state.mutable_globals
      in
      let result = Vm.execute_with_globals compiled state.globals in
      state_ref := Some { state with ctx = ctx' };
      result

let run_string_in_state state source =
  let tokens = Lexer.tokenize source in
  let program = Parser.parse_program tokens in
  let ctx', typed_program =
    Typechecker.check_program_in_ctx state.ctx program
  in
  List.iter
    (fun w -> Printf.eprintf "%s\n%!" w)
    (Typechecker.take_warnings ());
  let typed_program = Typechecker.transform_constraints ctx' typed_program in
  let stdlib_programs = List.map snd state.setup_typed in
  let typed_program =
    Pipeline.lower ~stdlib_programs ctx'.Typechecker.type_env typed_program
  in
  let compiled =
    Compiler.compile_program_with_globals ctx'.Typechecker.type_env
      state.global_names state.mutable_globals typed_program
  in
  (* Update state_ref so Runtime.eval can see all bindings *)
  let pre_exec_state = { state with ctx = ctx' } in
  state.state_ref := Some pre_exec_state;
  let result = Vm.execute_with_globals compiled state.globals in
  (* If Runtime.eval ran during execution, it updated state_ref with new ctx.
     Propagate that back so the caller sees the merged state. *)
  (match !(state.state_ref) with
  | Some es when es.ctx != pre_exec_state.ctx ->
      state.state_ref := Some { state with ctx = es.ctx }
  | _ -> ());
  result

(* ---- Oracle (reference interpreter) entry points ----------------------- *)

(* Bridge from a name to the VM external registered under it (builtins and
   stdlib module functions). The oracle delegates first-order builtins to
   these implementations. *)
let oracle_lookup_external state name =
  let found = ref None in
  Dynarray.iteri
    (fun idx n ->
      if !found = None && String.equal n name then
        match Hashtbl.find_opt state.globals idx with
        | Some (Bytecode.VExternal ext) -> found := Some ext
        | _ -> ())
    state.global_names;
  !found

(* Build the oracle's evaluation environment: bridge to VM externals, then
   evaluate every setup program's UNLOWERED AST with the oracle itself, so
   stdlib functions and typeclass dictionaries become oracle values. *)
let oracle_env_of_state state =
  let env =
    Oracle.make_env
      ~lookup_external:(oracle_lookup_external state)
      ~type_env:state.ctx.Typechecker.type_env
  in
  List.fold_left
    (fun env (_, program) -> fst (Oracle.eval_program env program))
    env state.setup_typed_unlowered

(* Run a source string with the Oracle reference interpreter. Same frontend as
   run_string_in_state (parse, typecheck, transform constraints) but NO
   lowering and NO compilation: the typed AST is evaluated directly. The result
   is converted to a Bytecode.value so callers can print/compare it uniformly. *)
let oracle_run_string_in_state state source =
  let tokens = Lexer.tokenize source in
  let program = Parser.parse_program tokens in
  let ctx', typed_program =
    Typechecker.check_program_in_ctx state.ctx program
  in
  List.iter
    (fun w -> Printf.eprintf "%s
%!" w)
    (Typechecker.take_warnings ());
  let typed_program = Typechecker.transform_constraints ctx' typed_program in
  (* Enforce language restrictions that are validated on the CLASSIFIED AST
     (e.g. `return` cannot escape a fiber handler -- Typechecker.check_returns).
     These are deliberate language rules, not lowering artifacts, so the oracle
     enforces them too. The classified result is discarded: the oracle
     interprets the unclassified tree. *)
  let _ = Typechecker.classify_handlers typed_program in
  let env = oracle_env_of_state state in
  (* The user program may declare its own types/newtypes; give the oracle the
     post-typecheck type_env. *)
  let env =
    Oracle.
      {
        env with
        globals = { env.globals with type_env = ctx'.Typechecker.type_env };
      }
  in
  let _, result = Oracle.eval_program env typed_program in
  Oracle.to_vm result

let run_string source =
  wrap_errors (fun () ->
      let ctx, global_names, globals, output_fn, argv = setup_builtins () in
      let mutable_globals = Hashtbl.create 8 in
      let state_ref = ref None in
      let state =
        {
          ctx;
          global_names;
          mutable_globals;
          globals;
          output_fn;
          argv;
          setup_protos = [];
          setup_typed = [];
          setup_typed_unlowered = [];
          state_ref;
        }
      in
      let state = setup_default_classes state in
      run_string_in_state state source)

let run_file filename =
  let ic = open_in filename in
  let source = In_channel.input_all ic in
  close_in ic;
  run_string source

let run_string_show source =
  try
    let result = run_string source in
    match result with
    | Bytecode.VUnit -> ()
    | v -> print_endline (Bytecode.pp_value v)
  with Error msg -> Printf.eprintf "%s\n" msg

let run_file_show filename =
  try
    let _ = run_file filename in
    ()
  with Error msg ->
    Printf.eprintf "%s\n" msg;
    exit 1

(* ---- REPL with persistent state ---- *)

let repl_state_init () =
  let ctx, global_names, globals, output_fn, argv = setup_builtins () in
  let mutable_globals = Hashtbl.create 8 in
  let state_ref = ref None in
  let state =
    {
      ctx;
      global_names;
      mutable_globals;
      globals;
      output_fn;
      argv;
      setup_protos = [];
      setup_typed = [];
      setup_typed_unlowered = [];
      state_ref;
    }
  in
  setup_default_classes state

let emit_json_bundle state ?source () =
  let main_proto =
    match source with
    | None ->
        Bytecode.
          {
            name = "<main>";
            arity = 0;
            num_locals = 0;
            code = [| HALT |];
            constants = [||];
            line_table = [| 0 |];
          }
    | Some src ->
        let tokens = Lexer.tokenize src in
        let program = Parser.parse_program tokens in
        let ctx', typed_program =
          Typechecker.check_program_in_ctx state.ctx program
        in
        let typed_program =
          Typechecker.transform_constraints ctx' typed_program
        in
        let stdlib_programs = List.map snd state.setup_typed in
        let typed_program =
          Pipeline.lower ~stdlib_programs ctx'.Typechecker.type_env typed_program
        in
        let compiled =
          Compiler.compile_program_with_globals ctx'.Typechecker.type_env
            state.global_names state.mutable_globals typed_program
        in
        compiled.main
  in
  let native_globals_json = Serialize.build_native_globals_json state.globals in
  Serialize.serialize_bundle ~global_names:state.global_names
    ~native_globals_json ~setup_protos:state.setup_protos ~main_proto

let emit_js state ?source () =
  match source with
  | None -> ""
  | Some src ->
      let tokens = Lexer.tokenize src in
      let program = Parser.parse_program tokens in
      let ctx', typed_program =
        Typechecker.check_program_in_ctx state.ctx program
      in
      let typed_program =
        Typechecker.transform_constraints ctx' typed_program
      in
      let stdlib_programs = List.map snd state.setup_typed in
      let typed_program =
        Pipeline.lower ~stdlib_programs ctx'.Typechecker.type_env typed_program
      in
      Js_codegen.compile_program_with_stdlib ctx'.Typechecker.type_env
        state.setup_typed typed_program

(* Serialize the LOWERED IR (post Pipeline.lower) — the structural dump used for
   cross-compiler IR parity / golden tests (roadmap #13). Same front-end as
   emit_js so the IR matches what the backends actually consume. *)
let emit_ir state ?source () =
  match source with
  | None -> ""
  | Some src ->
      let tokens = Lexer.tokenize src in
      let program = Parser.parse_program tokens in
      let ctx', typed_program =
        Typechecker.check_program_in_ctx state.ctx program
      in
      let typed_program =
        Typechecker.transform_constraints ctx' typed_program
      in
      let stdlib_programs = List.map snd state.setup_typed in
      let typed_program =
        Pipeline.lower ~stdlib_programs ctx'.Typechecker.type_env typed_program
      in
      Ir_serialize.serialize_program typed_program

let typecheck_source state source =
  let tokens = Lexer.tokenize source in
  let program = Parser.parse_program tokens in
  let ctx', typed_program =
    Typechecker.check_program_in_ctx state.ctx program
  in
  List.iter
    (fun w -> Printf.eprintf "%s\n%!" w)
    (Typechecker.take_warnings ());
  let typed_program = Typechecker.transform_constraints ctx' typed_program in
  let stdlib_programs = List.map snd state.setup_typed in
  Pipeline.lower ~stdlib_programs ctx'.Typechecker.type_env typed_program

let emit_binary_bundle state ?source () =
  let main_proto =
    match source with
    | None ->
        Bytecode.
          {
            name = "<main>";
            arity = 0;
            num_locals = 0;
            code = [| HALT |];
            constants = [||];
            line_table = [| 0 |];
          }
    | Some src ->
        let tokens = Lexer.tokenize src in
        let program = Parser.parse_program tokens in
        let ctx', typed_program =
          Typechecker.check_program_in_ctx state.ctx program
        in
        let typed_program =
          Typechecker.transform_constraints ctx' typed_program
        in
        let stdlib_programs = List.map snd state.setup_typed in
        let typed_program =
          Pipeline.lower ~stdlib_programs ctx'.Typechecker.type_env typed_program
        in
        let compiled =
          Compiler.compile_program_with_globals ctx'.Typechecker.type_env
            state.global_names state.mutable_globals typed_program
        in
        compiled.main
  in
  Serialize_bin.serialize_bundle ~global_names:state.global_names
    ~globals:state.globals ~setup_protos:state.setup_protos ~main_proto

let eval_repl state source =
  wrap_errors (fun () ->
      let tokens = Lexer.tokenize source in
      let program = Parser.parse_program tokens in
      let ctx', typed_program =
        Typechecker.check_program_in_ctx state.ctx program
      in
      let typed_program =
        Typechecker.transform_constraints ctx' typed_program
      in
      let stdlib_programs = List.map snd state.setup_typed in
      let typed_program =
        Pipeline.lower ~stdlib_programs ctx'.Typechecker.type_env typed_program
      in
      let synonyms = ctx'.Typechecker.type_env.Types.type_synonyms in
      let lookup_scheme name =
        match List.assoc_opt name ctx'.Typechecker.vars with
        | Some s -> Types.pp_scheme_with synonyms s
        | None -> Types.pp_ty_normalized_with synonyms Types.TUnit
        (* fallback *)
      in
      let type_info =
        match List.rev typed_program with
        | Typechecker.TDExpr te :: _ ->
            Some (None, Types.pp_ty_normalized_with synonyms te.ty)
        | Typechecker.TDLet (name, _) :: _ ->
            Some (Some name, lookup_scheme name)
        | Typechecker.TDLetMut (name, _) :: _ ->
            Some (Some name, lookup_scheme name)
        | Typechecker.TDLetRec (name, _) :: _ ->
            Some (Some name, lookup_scheme name)
        | _ -> None
      in
      let compiled =
        Compiler.compile_program_with_globals ctx'.Typechecker.type_env
          state.global_names state.mutable_globals typed_program
      in
      let pre_exec_state = { state with ctx = ctx' } in
      state.state_ref := Some pre_exec_state;
      let result = Vm.execute_with_globals compiled state.globals in
      (* If Runtime.eval/eval_file ran during execution, they updated state_ref
       with new modules/bindings. Use that ctx instead of our local ctx'. *)
      let final_ctx =
        match !(state.state_ref) with
        | Some es when es.ctx != pre_exec_state.ctx -> es.ctx
        | _ -> ctx'
      in
      ({ state with ctx = final_ctx }, result, type_info))

let typeof_source state source =
  wrap_errors (fun () ->
      let tokens = Lexer.tokenize source in
      let program = Parser.parse_program tokens in
      let ctx', typed_program =
        Typechecker.check_program_in_ctx state.ctx program
      in
      let synonyms = ctx'.Typechecker.type_env.Types.type_synonyms in
      let lookup_scheme name =
        match List.assoc_opt name ctx'.Typechecker.vars with
        | Some s -> Types.pp_scheme_with synonyms s
        | None -> "?"
      in
      match List.rev typed_program with
      | Typechecker.TDExpr { expr = Typechecker.TEVar name; _ } :: _ -> (
          (* For bare variable references, show the scheme with constraints *)
          match List.assoc_opt name state.ctx.Typechecker.vars with
          | Some s -> Types.pp_scheme_with synonyms s
          | None ->
              Types.pp_ty_normalized_with synonyms
                (List.assoc name ctx'.Typechecker.vars).Types.body)
      | Typechecker.TDExpr te :: _ -> Types.pp_ty_normalized_with synonyms te.ty
      | Typechecker.TDLet (name, _) :: _ -> name ^ " : " ^ lookup_scheme name
      | Typechecker.TDLetMut (name, _) :: _ ->
          name ^ " : " ^ lookup_scheme name ^ " (mutable)"
      | Typechecker.TDLetRec (name, _) :: _ -> name ^ " : " ^ lookup_scheme name
      | Typechecker.TDExtern (name, scheme) :: _ ->
          "extern " ^ name ^ " : " ^ Types.pp_scheme_with synonyms scheme
      | _ -> "<no type>")

let list_modules state =
  let modules = state.ctx.Typechecker.type_env.Types.modules in
  if modules = [] then "No modules loaded."
  else
    let names = List.map (fun (name, _) -> name) modules in
    let sorted = List.sort_uniq String.compare names in
    String.concat "\n" ("Modules:" :: List.map (fun n -> "  " ^ n) sorted)

let browse_module state mod_name =
  let modules = state.ctx.Typechecker.type_env.Types.modules in
  let matching =
    List.filter_map
      (fun (name, info) ->
        if String.equal name mod_name then Some info else None)
      modules
  in
  match matching with
  | [] -> Printf.sprintf "Unknown module: %s" mod_name
  | _ ->
      let buf = Buffer.create 256 in
      Buffer.add_string buf (Printf.sprintf "module %s:\n" mod_name);
      let seen_vars = Hashtbl.create 16 in
      let type_env = state.ctx.Typechecker.type_env in
      let all_synonyms = type_env.Types.type_synonyms in
      let all_variants = type_env.Types.variants in
      let all_records = type_env.Types.records in
      let format_params num_params =
        if num_params = 0 then ""
        else if num_params = 1 then "'a "
        else
          "("
          ^ String.concat ", "
              (List.init num_params (fun i ->
                   "'" ^ String.make 1 (Char.chr (Char.code 'a' + i))))
          ^ ") "
      in
      (* Collect module-scoped type synonyms for pretty-printing *)
      let module_synonyms = ref [] in
      (* Standard library synonyms: name matches lowercase module name (e.g. "set" for Set) *)
      let low_mod = String.lowercase_ascii mod_name in
      List.iter
        (fun (syn_name, num_params, expanded_ty) ->
          if String.equal syn_name low_mod then begin
            module_synonyms :=
              (syn_name, num_params, expanded_ty) :: !module_synonyms;
            Buffer.add_string buf
              (Printf.sprintf "  type %s%s = %s\n" (format_params num_params)
                 syn_name
                 (Types.pp_ty_raw expanded_ty))
          end)
        all_synonyms;
      List.iter
        (fun (minfo : Types.module_info) ->
          (* Types: look up synonym expansion for type aliases *)
          let mod_prefix = mod_name ^ "." in
          let shown_variant_types = Hashtbl.create 8 in
          List.iter
            (fun tname ->
              let short =
                match String.rindex_opt tname '.' with
                | Some i ->
                    String.sub tname (i + 1) (String.length tname - i - 1)
                | None -> tname
              in
              match
                List.find_opt
                  (fun (n, _, _) -> String.equal n tname)
                  all_synonyms
              with
              | Some (_, num_params, expanded_ty) ->
                  module_synonyms :=
                    (short, num_params, expanded_ty) :: !module_synonyms;
                  Buffer.add_string buf
                    (Printf.sprintf "  type %s%s = %s\n"
                       (format_params num_params) short
                       (Types.pp_ty_raw expanded_ty))
              | None ->
                  let display =
                    if
                      String.length tname > String.length mod_prefix
                      && String.sub tname 0 (String.length mod_prefix)
                         = mod_prefix
                    then short
                    else tname
                  in
                  let is_opaque = List.mem tname minfo.mod_opaque_types in
                  if is_opaque then
                    Buffer.add_string buf (Printf.sprintf "  type %s\n" display)
                  else begin
                    (* Show full definition for public types *)
                    match
                      List.find_opt
                        (fun (n, _, _, _) -> String.equal n tname)
                        all_variants
                    with
                    | Some (_, num_params, ctors, _) ->
                        Hashtbl.replace shown_variant_types tname true;
                        Buffer.add_string buf
                          (Printf.sprintf "  type %s%s =\n"
                             (format_params num_params) display);
                        List.iter
                          (fun (cname, arg_ty) ->
                            let ctor_short =
                              match String.rindex_opt cname '.' with
                              | Some i ->
                                  String.sub cname (i + 1)
                                    (String.length cname - i - 1)
                              | None -> cname
                            in
                            match arg_ty with
                            | None ->
                                Buffer.add_string buf
                                  (Printf.sprintf "    | %s\n" ctor_short)
                            | Some t ->
                                Buffer.add_string buf
                                  (Printf.sprintf "    | %s of %s\n" ctor_short
                                     (Types.pp_ty t)))
                          ctors
                    | None -> (
                        match
                          List.find_opt
                            (fun (n, _) -> String.equal n tname)
                            all_records
                        with
                        | Some (_, fields) ->
                            Buffer.add_string buf
                              (Printf.sprintf "  type %s%s = {\n"
                                 (format_params 0) display);
                            List.iter
                              (fun (fname, fty) ->
                                let mut_prefix =
                                  if
                                    List.mem fname type_env.Types.mutable_fields
                                  then "mut "
                                  else ""
                                in
                                Buffer.add_string buf
                                  (Printf.sprintf "    %s%s: %s;\n" mut_prefix
                                     fname (Types.pp_ty fty)))
                              fields;
                            Buffer.add_string buf "  }\n"
                        | None ->
                            Buffer.add_string buf
                              (Printf.sprintf "  type %s\n" display))
                  end)
            minfo.mod_pub_types;
          (* Constructors — only show those not already shown in type definitions *)
          List.iter
            (fun (cname, cinfo) ->
              if
                not (Hashtbl.mem shown_variant_types cinfo.Types.ctor_type_name)
              then begin
                let ty_str =
                  match cinfo.Types.ctor_arg_ty with
                  | None -> cname
                  | Some t -> cname ^ " of " ^ Types.pp_ty t
                in
                Buffer.add_string buf (Printf.sprintf "  %s\n" ty_str)
              end)
            minfo.mod_pub_constructors;
          (* Classes *)
          List.iter
            (fun qname ->
              let short =
                match String.rindex_opt qname '.' with
                | Some i ->
                    String.sub qname (i + 1) (String.length qname - i - 1)
                | None -> qname
              in
              match
                List.find_opt
                  (fun (c : Types.class_def) -> String.equal c.class_name qname)
                  state.ctx.Typechecker.type_env.Types.classes
              with
              | Some cls ->
                  Buffer.add_string buf
                    (Printf.sprintf "  class %s %s =\n" short
                       (String.concat " "
                          (List.map (fun p -> "'" ^ p) cls.class_params)));
                  List.iter
                    (fun (mname, mty) ->
                      Buffer.add_string buf
                        (Printf.sprintf "    val %s : %s\n" mname
                           (Types.pp_ty mty)))
                    cls.class_methods
              | None -> ())
            minfo.mod_pub_classes;
          (* Use module synonyms + global synonyms for function signature printing *)
          let synonyms = !module_synonyms @ all_synonyms in
          (* Functions/values — skip duplicates *)
          List.iter
            (fun (name, scheme) ->
              if not (Hashtbl.mem seen_vars name) then begin
                Hashtbl.replace seen_vars name true;
                Buffer.add_string buf
                  (Printf.sprintf "  val %s : %s\n" name
                     (Types.pp_scheme_with synonyms scheme))
              end)
            (List.rev minfo.mod_pub_vars);
          (* Submodules *)
          List.iter
            (fun (subname, _) ->
              Buffer.add_string buf (Printf.sprintf "  module %s\n" subname))
            minfo.mod_submodules)
        matching;
      String.trim (Buffer.contents buf)

let list_classes state =
  let type_env = state.ctx.Typechecker.type_env in
  let classes = type_env.Types.classes in
  let instances = type_env.Types.instances in
  if classes = [] then "No type classes defined."
  else begin
    let buf = Buffer.create 256 in
    List.iter
      (fun (cls : Types.class_def) ->
        Buffer.add_string buf
          (Printf.sprintf "class %s %s:\n" cls.class_name
             (String.concat " " (List.map (fun p -> "'" ^ p) cls.class_params)));
        List.iter
          (fun (meth_name, meth_ty) ->
            Buffer.add_string buf
              (Printf.sprintf "  val %s : %s\n" meth_name (Types.pp_ty meth_ty)))
          cls.class_methods;
        (* Show instances for this class *)
        let cls_instances =
          List.filter
            (fun (inst : Types.instance_def) ->
              String.equal inst.inst_class cls.class_name)
            instances
        in
        List.iter
          (fun (inst : Types.instance_def) ->
            let tys_str =
              String.concat " " (List.map Types.pp_ty inst.inst_tys)
            in
            let constraints_str =
              match inst.inst_constraints with
              | [] -> ""
              | cs ->
                  " where "
                  ^ String.concat ", " (List.map Types.pp_constraint cs)
            in
            Buffer.add_string buf
              (Printf.sprintf "  instance %s%s\n" tys_str constraints_str))
          cls_instances)
      classes;
    String.trim (Buffer.contents buf)
  end

let list_externs state =
  (* Collect all VExternal entries from globals, grouped by module *)
  let toplevel = Buffer.create 256 in
  let modules : (string, (string * string) list ref) Hashtbl.t =
    Hashtbl.create 16
  in
  Dynarray.iteri
    (fun idx name ->
      match Hashtbl.find_opt state.globals idx with
      | Some (Bytecode.VExternal _) ->
          (* Skip Stdlib. duplicates *)
          if not (String.length name > 7 && String.sub name 0 7 = "Stdlib.")
          then begin
            let synonyms = state.ctx.Typechecker.type_env.Types.type_synonyms in
            let ty_str =
              match List.assoc_opt name state.ctx.Typechecker.vars with
              | Some scheme -> Types.pp_scheme_with synonyms scheme
              | None -> "?"
            in
            match String.index_opt name '.' with
            | Some i ->
                let mod_name = String.sub name 0 i in
                let fn_name =
                  String.sub name (i + 1) (String.length name - i - 1)
                in
                let entries =
                  match Hashtbl.find_opt modules mod_name with
                  | Some r -> r
                  | None ->
                      let r = ref [] in
                      Hashtbl.replace modules mod_name r;
                      r
                in
                entries := (fn_name, ty_str) :: !entries
            | None ->
                Buffer.add_string toplevel
                  (Printf.sprintf "  %s : %s\n" name ty_str)
          end
      | _ -> ())
    state.global_names;
  let buf = Buffer.create 512 in
  if Buffer.length toplevel > 0 then begin
    Buffer.add_string buf "Builtins:\n";
    Buffer.add_buffer buf toplevel
  end;
  let mod_names = Hashtbl.fold (fun k _ acc -> k :: acc) modules [] in
  let mod_names = List.sort String.compare mod_names in
  List.iter
    (fun mod_name ->
      Buffer.add_string buf (Printf.sprintf "%s:\n" mod_name);
      let entries = List.rev !(Hashtbl.find modules mod_name) in
      List.iter
        (fun (fn_name, ty_str) ->
          Buffer.add_string buf (Printf.sprintf "  %s : %s\n" fn_name ty_str))
        entries)
    mod_names;
  let s = Buffer.contents buf in
  if String.length s = 0 then "No externals registered." else String.trim s

let eval_repl_show state source =
  try
    let state', result, type_info = eval_repl state source in
    (match (result, type_info) with
    | Bytecode.VUnit, Some (Some name, ty_str) ->
        Printf.printf "%s : %s\n%!" name ty_str
    | Bytecode.VUnit, _ -> ()
    | v, Some (Some name, ty_str) ->
        Printf.printf "%s : %s = %s\n%!" name ty_str (Bytecode.pp_value v)
    | v, Some (None, ty_str) ->
        Printf.printf "%s : %s\n%!" (Bytecode.pp_value v) ty_str
    | v, None -> print_endline (Bytecode.pp_value v));
    state'
  with Error msg ->
    Printf.eprintf "%s\n%!" msg;
    state
