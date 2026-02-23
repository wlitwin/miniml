exception Error of string

let wrap_errors f =
  try f () with
  | Lexer.Lex_error (msg, loc) ->
    raise (Error (Printf.sprintf "Lex error at %d:%d: %s" loc.line loc.col msg))
  | Parser.Parse_error (msg, loc) ->
    raise (Error (Printf.sprintf "Parse error at %d:%d: %s" loc.line loc.col msg))
  | Typechecker.Type_error (msg, loc) ->
    if loc.line > 0 then
      raise (Error (Printf.sprintf "Type error at line %d: %s" loc.line msg))
    else
      raise (Error (Printf.sprintf "Type error: %s" msg))
  | Compiler.Compile_error msg ->
    raise (Error (Printf.sprintf "Compile error: %s" msg))
  | Vm.Runtime_error msg ->
    raise (Error (Printf.sprintf "Runtime error: %s" msg))

let output_fn = ref print_endline
let script_argv : string array ref = ref Sys.argv

(* ---- Built-in definitions ---- *)

type builtin_def = {
  name: string;
  ty: Types.ty;
  quant: int;
  arity: int;
  impl: Bytecode.value list -> Bytecode.value;
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
  | v -> raise (Vm.Runtime_error (Printf.sprintf "expected string, got %s" (Bytecode.pp_value v)))

let as_byte = function
  | Bytecode.VByte n -> n
  | _ -> raise (Vm.Runtime_error "expected byte")

let as_rune = function
  | Bytecode.VRune n -> n
  | _ -> raise (Vm.Runtime_error "expected rune")

let as_map = function
  | Bytecode.VMap m -> m
  | v -> raise (Vm.Runtime_error
      (Printf.sprintf "expected map, got %s" (Bytecode.pp_value v)))

let as_array = function
  | Bytecode.VArray a -> a
  | _ -> raise (Vm.Runtime_error "expected array")

let int2 = Types.TArrow (Types.TInt, Types.EffEmpty, Types.TArrow (Types.TInt, Types.EffEmpty, Types.TInt))
let float2 = Types.TArrow (Types.TFloat, Types.EffEmpty, Types.TArrow (Types.TFloat, Types.EffEmpty, Types.TFloat))
let bool2 = Types.TArrow (Types.TBool, Types.EffEmpty, Types.TArrow (Types.TBool, Types.EffEmpty, Types.TBool))

let builtins : builtin_def list = [
  (* Int modulo (stays as builtin, not in Num class) *)
  { name = "mod"; ty = int2; quant = 0; arity = 2;
    impl = fun args ->
      let b = as_int (List.nth args 1) in
      if b = 0 then raise (Vm.Runtime_error "modulo by zero");
      VInt (as_int (List.nth args 0) mod b) };

  (* Print *)
  { name = "print";
    ty = Types.TArrow (Types.TGen 0, Types.EffEmpty, Types.TUnit);
    quant = 1; arity = 1;
    impl = fun args ->
      !output_fn (Bytecode.pp_value (List.nth args 0));
      VUnit };

  (* Failwith *)
  { name = "failwith";
    ty = Types.TArrow (Types.TString, Types.EffEmpty, Types.TGen 0);
    quant = 1; arity = 1;
    impl = fun args ->
      raise (Vm.Runtime_error (as_string (List.nth args 0))) };

  (* String *)
  { name = "^"; ty = Types.TArrow (Types.TString, Types.EffEmpty, Types.TArrow (Types.TString, Types.EffEmpty, Types.TString));
    quant = 0; arity = 2;
    impl = fun args -> VString (as_string (List.nth args 0) ^ as_string (List.nth args 1)) };

  (* Boolean *)
  { name = "&&"; ty = bool2; quant = 0; arity = 2;
    impl = fun args -> VBool (as_bool (List.nth args 0) && as_bool (List.nth args 1)) };
  { name = "||"; ty = bool2; quant = 0; arity = 2;
    impl = fun args -> VBool (as_bool (List.nth args 0) || as_bool (List.nth args 1)) };
  { name = "not"; ty = Types.TArrow (Types.TBool, Types.EffEmpty, Types.TBool); quant = 0; arity = 1;
    impl = fun args -> VBool (not (as_bool (List.nth args 0))) };

  (* Physical equality *)
  { name = "phys_equal";
    ty = Types.TArrow (Types.TGen 0, Types.EffEmpty,
         Types.TArrow (Types.TGen 0, Types.EffEmpty, Types.TBool));
    quant = 1; arity = 2;
    impl = fun args -> VBool (List.nth args 0 == List.nth args 1) };

  (* Conversions *)
  { name = "float_of_int"; ty = Types.TArrow (Types.TInt, Types.EffEmpty, Types.TFloat); quant = 0; arity = 1;
    impl = fun args -> VFloat (Float.of_int (as_int (List.nth args 0))) };
  { name = "int_of_float"; ty = Types.TArrow (Types.TFloat, Types.EffEmpty, Types.TInt); quant = 0; arity = 1;
    impl = fun args -> VInt (Float.to_int (as_float (List.nth args 0))) };

  (* String conversions *)
  { name = "string_of_int"; ty = Types.TArrow (Types.TInt, Types.EffEmpty, Types.TString); quant = 0; arity = 1;
    impl = fun args -> VString (string_of_int (as_int (List.nth args 0))) };
  { name = "string_of_float"; ty = Types.TArrow (Types.TFloat, Types.EffEmpty, Types.TString); quant = 0; arity = 1;
    impl = fun args -> VString (Printf.sprintf "%g" (as_float (List.nth args 0))) };
  { name = "string_of_bool"; ty = Types.TArrow (Types.TBool, Types.EffEmpty, Types.TString); quant = 0; arity = 1;
    impl = fun args ->
      VString (if as_bool (List.nth args 0) then "true" else "false") };

  (* Map pattern matching helpers (used by compiler for PatMap) *)
  { name = "__map_has";
    ty = Types.TArrow (Types.TMap (Types.TGen 0, Types.TGen 1),
         Types.EffEmpty, Types.TArrow (Types.TGen 0, Types.EffEmpty, Types.TBool));
    quant = 2; arity = 2;
    impl = fun args ->
      let pairs = as_map (List.nth args 0) in
      let key = List.nth args 1 in
      VBool (List.mem_assoc key pairs) };
  { name = "__map_get";
    ty = Types.TArrow (Types.TMap (Types.TGen 0, Types.TGen 1),
         Types.EffEmpty, Types.TArrow (Types.TGen 0, Types.EffEmpty, Types.TGen 1));
    quant = 2; arity = 2;
    impl = fun args ->
      let pairs = as_map (List.nth args 0) in
      let key = List.nth args 1 in
      (match List.assoc_opt key pairs with
       | Some v -> v
       | None -> raise (Vm.Runtime_error "key not found in map")) };

  (* Array operations (array_get and array_length kept for internal use by Iter/Show instances) *)
  { name = "array_get";
    ty = Types.TArrow (Types.TArray (Types.TGen 0), Types.EffEmpty, Types.TArrow (Types.TInt, Types.EffEmpty, Types.TGen 0));
    quant = 1; arity = 2;
    impl = fun args ->
      let arr = as_array (List.nth args 0) in
      let idx = as_int (List.nth args 1) in
      if idx < 0 || idx >= Array.length arr then
        raise (Vm.Runtime_error (Printf.sprintf "array index %d out of bounds (length %d)" idx (Array.length arr)))
      else arr.(idx) };
  { name = "array_length";
    ty = Types.TArrow (Types.TArray (Types.TGen 0), Types.EffEmpty, Types.TInt);
    quant = 1; arity = 1;
    impl = fun args ->
      let arr = as_array (List.nth args 0) in
      VInt (Array.length arr) };

  (* Byte primitives — type conversions need native support *)
  { name = "__byte_to_int";
    ty = Types.TArrow (Types.TByte, Types.EffEmpty, Types.TInt);
    quant = 0; arity = 1;
    impl = fun args -> VInt (as_byte (List.nth args 0)) };
  { name = "__byte_of_int";
    ty = Types.TArrow (Types.TInt, Types.EffEmpty, Types.TByte);
    quant = 0; arity = 1;
    impl = fun args -> VByte (as_int (List.nth args 0) land 0xFF) };
  { name = "__byte_to_string";
    ty = Types.TArrow (Types.TByte, Types.EffEmpty, Types.TString);
    quant = 0; arity = 1;
    impl = fun args ->
      let b = as_byte (List.nth args 0) in
      VString (String.make 1 (Char.chr b)) };

  (* Rune primitives — type conversions and UTF-8 encoding need native support *)
  { name = "__rune_to_int";
    ty = Types.TArrow (Types.TRune, Types.EffEmpty, Types.TInt);
    quant = 0; arity = 1;
    impl = fun args -> VInt (as_rune (List.nth args 0)) };
  { name = "__rune_of_int";
    ty = Types.TArrow (Types.TInt, Types.EffEmpty, Types.TRune);
    quant = 0; arity = 1;
    impl = fun args -> VRune (as_int (List.nth args 0)) };
  { name = "__rune_to_string";
    ty = Types.TArrow (Types.TRune, Types.EffEmpty, Types.TString);
    quant = 0; arity = 1;
    impl = fun args ->
      let cp = as_rune (List.nth args 0) in
      let buf = Buffer.create 4 in
      if cp < 0x80 then Buffer.add_char buf (Char.chr cp)
      else if cp < 0x800 then begin
        Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6)));
        Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
      end else if cp < 0x10000 then begin
        Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12)));
        Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
        Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
      end else begin
        Buffer.add_char buf (Char.chr (0xF0 lor (cp lsr 18)));
        Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
        Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
        Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
      end;
      VString (Buffer.contents buf) };

  (* Math primitives — float operations need native support *)
  { name = "__math_pow";
    ty = Types.TArrow (Types.TFloat, Types.EffEmpty, Types.TArrow (Types.TFloat, Types.EffEmpty, Types.TFloat));
    quant = 0; arity = 2;
    impl = fun args ->
      VFloat (Float.pow (as_float (List.nth args 0)) (as_float (List.nth args 1))) };
  { name = "__math_sqrt";
    ty = Types.TArrow (Types.TFloat, Types.EffEmpty, Types.TFloat);
    quant = 0; arity = 1;
    impl = fun args -> VFloat (Float.sqrt (as_float (List.nth args 0))) };
  { name = "__math_floor";
    ty = Types.TArrow (Types.TFloat, Types.EffEmpty, Types.TInt);
    quant = 0; arity = 1;
    impl = fun args -> VInt (int_of_float (floor (as_float (List.nth args 0)))) };
  { name = "__math_ceil";
    ty = Types.TArrow (Types.TFloat, Types.EffEmpty, Types.TInt);
    quant = 0; arity = 1;
    impl = fun args -> VInt (int_of_float (ceil (as_float (List.nth args 0)))) };
  { name = "__math_round";
    ty = Types.TArrow (Types.TFloat, Types.EffEmpty, Types.TInt);
    quant = 0; arity = 1;
    impl = fun args -> VInt (Float.to_int (Float.round (as_float (List.nth args 0)))) };

  (* Format specifier builtins *)
  { name = "__fmt_float";
    ty = Types.TArrow (Types.TInt, Types.EffEmpty, Types.TArrow (Types.TFloat, Types.EffEmpty, Types.TString));
    quant = 0; arity = 2;
    impl = fun args -> VString (Printf.sprintf "%.*f" (as_int (List.nth args 0)) (as_float (List.nth args 1))) };
  { name = "__fmt_hex";
    ty = Types.TArrow (Types.TInt, Types.EffEmpty, Types.TString);
    quant = 0; arity = 1;
    impl = fun args -> VString (Printf.sprintf "%x" (as_int (List.nth args 0))) };
  { name = "__fmt_hex_upper";
    ty = Types.TArrow (Types.TInt, Types.EffEmpty, Types.TString);
    quant = 0; arity = 1;
    impl = fun args -> VString (Printf.sprintf "%X" (as_int (List.nth args 0))) };
  { name = "__fmt_oct";
    ty = Types.TArrow (Types.TInt, Types.EffEmpty, Types.TString);
    quant = 0; arity = 1;
    impl = fun args -> VString (Printf.sprintf "%o" (as_int (List.nth args 0))) };
  { name = "__fmt_bin";
    ty = Types.TArrow (Types.TInt, Types.EffEmpty, Types.TString);
    quant = 0; arity = 1;
    impl = fun args ->
      let n = as_int (List.nth args 0) in
      let rec go n acc = if n = 0 then acc else go (n lsr 1) (string_of_int (n land 1) ^ acc) in
      VString (if n = 0 then "0" else go n "") };
  { name = "__fmt_zero_pad";
    ty = Types.TArrow (Types.TInt, Types.EffEmpty, Types.TArrow (Types.TString, Types.EffEmpty, Types.TString));
    quant = 0; arity = 2;
    impl = fun args ->
      let width = as_int (List.nth args 0) in
      let s = as_string (List.nth args 1) in
      let pad = max 0 (width - String.length s) in
      VString (String.make pad '0' ^ s) };
  { name = "__fmt_pad_left";
    ty = Types.TArrow (Types.TInt, Types.EffEmpty, Types.TArrow (Types.TString, Types.EffEmpty, Types.TString));
    quant = 0; arity = 2;
    impl = fun args ->
      let width = as_int (List.nth args 0) in
      let s = as_string (List.nth args 1) in
      let pad = max 0 (width - String.length s) in
      VString (String.make pad ' ' ^ s) };
  { name = "__fmt_pad_right";
    ty = Types.TArrow (Types.TInt, Types.EffEmpty, Types.TArrow (Types.TString, Types.EffEmpty, Types.TString));
    quant = 0; arity = 2;
    impl = fun args ->
      let width = as_int (List.nth args 0) in
      let s = as_string (List.nth args 1) in
      let pad = max 0 (width - String.length s) in
      VString (s ^ String.make pad ' ') };
  { name = "__show_value";
    ty = Types.TArrow (Types.TGen 0, Types.EffEmpty, Types.TString);
    quant = 1; arity = 1;
    impl = fun args -> Bytecode.VString (Bytecode.pp_value (List.nth args 0)) };
]

let copy_fiber (f : Bytecode.fiber) : Bytecode.fiber =
  let new_stack = Array.copy f.fiber_stack in
  let new_frames = List.map (fun (frame : Bytecode.call_frame) ->
    Bytecode.{
      frame_closure = frame.frame_closure;
      frame_ip = frame.frame_ip;
      frame_locals = Array.copy frame.frame_locals;
      frame_base_sp = frame.frame_base_sp;
    }
  ) f.fiber_frames in
  Bytecode.{
    fiber_stack = new_stack;
    fiber_sp = f.fiber_sp;
    fiber_frames = new_frames;
  }

let copy_continuation = function
  | Bytecode.VContinuation cd ->
    Bytecode.VContinuation {
      cd_fiber = copy_fiber cd.cd_fiber;
      cd_return_handler = cd.cd_return_handler;
      cd_op_handlers = cd.cd_op_handlers;
      cd_used = false;
    }
  | v -> raise (Vm.Runtime_error
      (Printf.sprintf "copy: expected continuation, got %s" (Bytecode.pp_value v)))

let make_external (b : builtin_def) : Bytecode.value =
  Bytecode.VExternal {
    ext_name = b.name;
    ext_arity = b.arity;
    ext_fn = b.impl;
    ext_args = [];
  }

(* ---- State with built-ins ---- *)

type repl_state = {
  ctx: Typechecker.ctx;
  global_names: string Dynarray.t;
  mutable_globals: (string, unit) Hashtbl.t;
  globals: (int, Bytecode.value) Hashtbl.t;
}

let builtin_table (state : repl_state) : Deserialize.builtin_table =
  let tbl : Deserialize.builtin_table = Hashtbl.create 256 in
  Hashtbl.iter (fun _idx value ->
    match value with
    | Bytecode.VExternal ext ->
      if not (Hashtbl.mem tbl ext.ext_name) then
        Hashtbl.replace tbl ext.ext_name
          Deserialize.{ arity = ext.ext_arity; impl = ext.ext_fn }
    | Bytecode.VRecord (_shape, values) ->
      Array.iter (fun v ->
        match v with
        | Bytecode.VExternal ext ->
          if not (Hashtbl.mem tbl ext.ext_name) then
            Hashtbl.replace tbl ext.ext_name
              Deserialize.{ arity = ext.ext_arity; impl = ext.ext_fn }
        | _ -> ()
      ) values
    | _ -> ()
  ) state.globals;
  tbl

let setup_builtins () =
  let global_names = Dynarray.create () in
  let globals = Hashtbl.create 64 in
  let stdlib_pub_vars = ref [] in
  let vars = List.concat_map (fun (b : builtin_def) ->
    let idx = Dynarray.length global_names in
    Dynarray.add_last global_names b.name;
    Hashtbl.replace globals idx (make_external b);
    (* Also register under Stdlib. prefix *)
    let stdlib_name = "Stdlib." ^ b.name in
    let sidx = Dynarray.length global_names in
    Dynarray.add_last global_names stdlib_name;
    Hashtbl.replace globals sidx (make_external b);
    let scheme = { Types.quant = b.quant; equant = 0; pvquant = 0; rquant = 0; constraints = []; record_evidences = []; body = b.ty } in
    stdlib_pub_vars := (b.name, scheme) :: !stdlib_pub_vars;
    [(b.name, scheme); (stdlib_name, scheme)]
  ) builtins in
  (* Register polymorphic copy_continuation builtin: 'a -> 'a *)
  let copy_idx = Dynarray.length global_names in
  Dynarray.add_last global_names "copy_continuation";
  let copy_ext = Bytecode.VExternal {
    ext_name = "copy_continuation";
    ext_arity = 1;
    ext_fn = (fun args -> copy_continuation (List.nth args 0));
    ext_args = [];
  } in
  Hashtbl.replace globals copy_idx copy_ext;
  let copy_sidx = Dynarray.length global_names in
  Dynarray.add_last global_names "Stdlib.copy_continuation";
  Hashtbl.replace globals copy_sidx copy_ext;
  let copy_scheme = { Types.quant = 1; equant = 0; pvquant = 0; rquant = 0; constraints = []; record_evidences = [];
    body = Types.TArrow (Types.TGen 0, Types.EffEmpty, Types.TGen 0) } in
  stdlib_pub_vars := ("copy_continuation", copy_scheme) :: !stdlib_pub_vars;
  let vars = ("copy_continuation", copy_scheme) :: ("Stdlib.copy_continuation", copy_scheme) :: vars in
  let ctx = Typechecker.{
    vars;
    mutable_vars = [];
    type_env = Types.empty_type_env;
    loop_info = None;
    current_module = None;
    constraint_tvars = [];
    current_eff = Types.EffEmpty;
    return_type = None;
    inside_handler = false;
    return_used = ref false;
  } in
  (ctx, global_names, globals, !stdlib_pub_vars)

(* ---- Embedding API: register custom external functions ---- *)

let register_external state name ty arity impl =
  let idx = Dynarray.length state.global_names in
  Dynarray.add_last state.global_names name;
  Hashtbl.replace state.globals idx
    (Bytecode.VExternal {
      ext_name = name;
      ext_arity = arity;
      ext_fn = impl;
      ext_args = [];
    });
  let ctx = Typechecker.{ state.ctx with
    vars = (name, Types.mono ty) :: state.ctx.vars
  } in
  { state with ctx }

(* ---- Embedding API: register type classes and instances ---- *)

let register_class state ~name ~tyvars ~fundeps ~methods =
  let num_params = List.length tyvars in
  let class_def = Types.{
    class_name = name;
    class_params = tyvars;
    class_methods = methods;
    class_fundeps = fundeps;
  } in
  let vars = List.fold_left (fun vars (mname, mty) ->
    let max_gen = Typechecker.max_tgen_in_ty mty in
    let quant = max (max_gen + 1) num_params in
    (mname, { Types.quant = quant; equant = 0; pvquant = 0; rquant = 0; constraints = []; record_evidences = []; body = mty }) :: vars
  ) state.ctx.Typechecker.vars methods in
  let type_env = { state.ctx.type_env with
    Types.classes = class_def :: state.ctx.type_env.classes;
  } in
  let ctx = Typechecker.{ vars; mutable_vars = state.ctx.mutable_vars; type_env; loop_info = None; current_module = None; constraint_tvars = []; current_eff = Types.EffEmpty; return_type = None; inside_handler = false; return_used = ref false } in
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
  List.iter (fun (method_name, method_value) ->
    let method_global_name = dname ^ "$" ^ method_name in
    let midx = Dynarray.length state.global_names in
    Dynarray.add_last state.global_names method_global_name;
    Hashtbl.replace state.globals midx method_value
  ) sorted;
  let inst_def = Types.{
    inst_class = class_name;
    inst_tys = tys;
    inst_dict_name = dname;
    inst_constraints = [];
  } in
  let type_env = { state.ctx.type_env with
    Types.instances = inst_def :: state.ctx.type_env.instances;
  } in
  let ctx = Typechecker.{ state.ctx with type_env } in
  { state with ctx }

let register_fns state mod_name (fns : (string * int * (Bytecode.value list -> Bytecode.value)) list) =
  List.fold_left (fun st (fn_name, arity, impl) ->
    let qualified = mod_name ^ "." ^ fn_name in
    let idx = Dynarray.length st.global_names in
    Dynarray.add_last st.global_names qualified;
    Hashtbl.replace st.globals idx
      (Bytecode.VExternal { ext_name = qualified; ext_arity = arity; ext_fn = impl; ext_args = [] });
    st
  ) state fns

let register_module state mod_name (fns : (string * Types.ty * int * (Bytecode.value list -> Bytecode.value)) list) =
  let pub_vars = ref [] in
  let state = List.fold_left (fun st (fn_name, ty, arity, impl) ->
    let qualified = mod_name ^ "." ^ fn_name in
    let idx = Dynarray.length st.global_names in
    Dynarray.add_last st.global_names qualified;
    Hashtbl.replace st.globals idx
      (Bytecode.VExternal { ext_name = qualified; ext_arity = arity; ext_fn = impl; ext_args = [] });
    let max_gen = Typechecker.max_tgen_in_ty ty in
    let scheme = if max_gen >= 0 then { Types.quant = max_gen + 1; equant = 0; pvquant = 0; rquant = 0; constraints = []; record_evidences = []; body = ty }
                 else Types.mono ty in
    pub_vars := (fn_name, scheme) :: !pub_vars;
    { st with ctx = Typechecker.{ st.ctx with vars = (qualified, scheme) :: st.ctx.vars } }
  ) state fns in
  let minfo = Types.{
    mod_name; mod_pub_vars = !pub_vars; mod_pub_mutable_vars = [];
    mod_pub_types = [];
    mod_opaque_types = []; mod_pub_constructors = []; mod_instances = [];
    mod_submodules = []; mod_pub_classes = [];
  } in
  let type_env = { state.ctx.type_env with
    Types.modules = (mod_name, minfo) :: state.ctx.type_env.modules } in
  { state with ctx = Typechecker.{ state.ctx with type_env } }

let make_ext name arity fn =
  Bytecode.VExternal { ext_name = name; ext_arity = arity; ext_fn = fn; ext_args = [] }

let captured_setups : Bytecode.prototype list ref = ref []
let capture_mode : bool ref = ref false

let eval_setup state source =
  let tokens = Lexer.tokenize source in
  let program = Parser.parse_program tokens in
  let (ctx', typed_program) =
    Typechecker.check_program_in_ctx state.ctx program in
  let typed_program = Typechecker.transform_constraints ctx' typed_program in
  let compiled =
    Compiler.compile_program_with_globals
      ctx'.Typechecker.type_env state.global_names state.mutable_globals typed_program in
  let _ = Vm.execute_with_globals compiled state.globals in
  if !capture_mode then
    captured_setups := !captured_setups @ [compiled.main];
  { state with ctx = ctx' }

let setup_default_classes state stdlib_pub_vars =
  (* Option type: type 'a option = None | Some of 'a *)
  let type_env = { state.ctx.type_env with
    Types.variants = ("option", 1, [("None", None); ("Some", Some (Types.TGen 0))], false) :: state.ctx.type_env.variants;
    constructors =
      ("None", Types.{ ctor_type_name = "option"; ctor_arg_ty = None; ctor_num_params = 1;
                        ctor_return_ty_params = None; ctor_existentials = 0 }) ::
      ("Some", Types.{ ctor_type_name = "option"; ctor_arg_ty = Some (Types.TGen 0); ctor_num_params = 1;
                        ctor_return_ty_params = None; ctor_existentials = 0 }) ::
      state.ctx.type_env.constructors;
  } in
  let state = { state with ctx = Typechecker.{ state.ctx with type_env } } in
  (* ---- Num class: + - * / : 'a -> 'a -> 'a; neg : 'a -> 'a ---- *)
  let a2a = Types.TArrow (Types.TGen 0, Types.EffEmpty, Types.TArrow (Types.TGen 0, Types.EffEmpty, Types.TGen 0)) in
  let a_a = Types.TArrow (Types.TGen 0, Types.EffEmpty, Types.TGen 0) in
  let state = register_class state
    ~name:"Num" ~tyvars:["a"] ~fundeps:[]
    ~methods:[("+", a2a); ("-", a2a); ("*", a2a); ("/", a2a); ("neg", a_a)] in
  let state = register_instance state
    ~class_name:"Num" ~tys:[Types.TInt]
    ~methods:[
      ("+", make_ext "num_add_int" 2
        (fun args -> VInt (as_int (List.nth args 0) + as_int (List.nth args 1))));
      ("-", make_ext "num_sub_int" 2
        (fun args -> VInt (as_int (List.nth args 0) - as_int (List.nth args 1))));
      ("*", make_ext "num_mul_int" 2
        (fun args -> VInt (as_int (List.nth args 0) * as_int (List.nth args 1))));
      ("/", make_ext "num_div_int" 2
        (fun args ->
          let b = as_int (List.nth args 1) in
          if b = 0 then raise (Vm.Runtime_error "division by zero");
          VInt (as_int (List.nth args 0) / b)));
      ("neg", make_ext "num_neg_int" 1
        (fun args -> VInt (- as_int (List.nth args 0))));
    ] in
  let state = register_instance state
    ~class_name:"Num" ~tys:[Types.TFloat]
    ~methods:[
      ("+", make_ext "num_add_float" 2
        (fun args -> VFloat (as_float (List.nth args 0) +. as_float (List.nth args 1))));
      ("-", make_ext "num_sub_float" 2
        (fun args -> VFloat (as_float (List.nth args 0) -. as_float (List.nth args 1))));
      ("*", make_ext "num_mul_float" 2
        (fun args -> VFloat (as_float (List.nth args 0) *. as_float (List.nth args 1))));
      ("/", make_ext "num_div_float" 2
        (fun args -> VFloat (as_float (List.nth args 0) /. as_float (List.nth args 1))));
      ("neg", make_ext "num_neg_float" 1
        (fun args -> VFloat (-. (as_float (List.nth args 0)))));
    ] in
  (* ---- Eq class: (=) (<>) : 'a -> 'a -> bool ---- *)
  let a2bool = Types.TArrow (Types.TGen 0, Types.EffEmpty, Types.TArrow (Types.TGen 0, Types.EffEmpty, Types.TBool)) in
  let state = register_class state
    ~name:"Eq" ~tyvars:["a"] ~fundeps:[]
    ~methods:[("=", a2bool); ("<>", a2bool)] in
  let state = register_instance state
    ~class_name:"Eq" ~tys:[Types.TInt]
    ~methods:[
      ("=", make_ext "eq_int" 2
        (fun args -> VBool (as_int (List.nth args 0) = as_int (List.nth args 1))));
      ("<>", make_ext "neq_int" 2
        (fun args -> VBool (as_int (List.nth args 0) <> as_int (List.nth args 1))));
    ] in
  let state = register_instance state
    ~class_name:"Eq" ~tys:[Types.TFloat]
    ~methods:[
      ("=", make_ext "eq_float" 2
        (fun args -> VBool (as_float (List.nth args 0) = as_float (List.nth args 1))));
      ("<>", make_ext "neq_float" 2
        (fun args -> VBool (as_float (List.nth args 0) <> as_float (List.nth args 1))));
    ] in
  let state = register_instance state
    ~class_name:"Eq" ~tys:[Types.TString]
    ~methods:[
      ("=", make_ext "eq_string" 2
        (fun args -> VBool (as_string (List.nth args 0) = as_string (List.nth args 1))));
      ("<>", make_ext "neq_string" 2
        (fun args -> VBool (as_string (List.nth args 0) <> as_string (List.nth args 1))));
    ] in
  let state = register_instance state
    ~class_name:"Eq" ~tys:[Types.TBool]
    ~methods:[
      ("=", make_ext "eq_bool" 2
        (fun args -> VBool (as_bool (List.nth args 0) = as_bool (List.nth args 1))));
      ("<>", make_ext "neq_bool" 2
        (fun args -> VBool (as_bool (List.nth args 0) <> as_bool (List.nth args 1))));
    ] in
  let state = register_instance state
    ~class_name:"Eq" ~tys:[Types.TByte]
    ~methods:[
      ("=", make_ext "eq_byte" 2
        (fun args -> VBool (as_byte (List.nth args 0) = as_byte (List.nth args 1))));
      ("<>", make_ext "neq_byte" 2
        (fun args -> VBool (as_byte (List.nth args 0) <> as_byte (List.nth args 1))));
    ] in
  let state = register_instance state
    ~class_name:"Eq" ~tys:[Types.TRune]
    ~methods:[
      ("=", make_ext "eq_rune" 2
        (fun args -> VBool (as_rune (List.nth args 0) = as_rune (List.nth args 1))));
      ("<>", make_ext "neq_rune" 2
        (fun args -> VBool (as_rune (List.nth args 0) <> as_rune (List.nth args 1))));
    ] in
  (* ---- Ord class: (<) (>) (<=) (>=) : 'a -> 'a -> bool ---- *)
  let state = register_class state
    ~name:"Ord" ~tyvars:["a"] ~fundeps:[]
    ~methods:[("<", a2bool); (">", a2bool); ("<=", a2bool); (">=", a2bool)] in
  let state = register_instance state
    ~class_name:"Ord" ~tys:[Types.TInt]
    ~methods:[
      ("<", make_ext "lt_int" 2
        (fun args -> VBool (as_int (List.nth args 0) < as_int (List.nth args 1))));
      (">", make_ext "gt_int" 2
        (fun args -> VBool (as_int (List.nth args 0) > as_int (List.nth args 1))));
      ("<=", make_ext "le_int" 2
        (fun args -> VBool (as_int (List.nth args 0) <= as_int (List.nth args 1))));
      (">=", make_ext "ge_int" 2
        (fun args -> VBool (as_int (List.nth args 0) >= as_int (List.nth args 1))));
    ] in
  let state = register_instance state
    ~class_name:"Ord" ~tys:[Types.TFloat]
    ~methods:[
      ("<", make_ext "lt_float" 2
        (fun args -> VBool (as_float (List.nth args 0) < as_float (List.nth args 1))));
      (">", make_ext "gt_float" 2
        (fun args -> VBool (as_float (List.nth args 0) > as_float (List.nth args 1))));
      ("<=", make_ext "le_float" 2
        (fun args -> VBool (as_float (List.nth args 0) <= as_float (List.nth args 1))));
      (">=", make_ext "ge_float" 2
        (fun args -> VBool (as_float (List.nth args 0) >= as_float (List.nth args 1))));
    ] in
  let state = register_instance state
    ~class_name:"Ord" ~tys:[Types.TString]
    ~methods:[
      ("<", make_ext "lt_string" 2
        (fun args -> VBool (String.compare (as_string (List.nth args 0)) (as_string (List.nth args 1)) < 0)));
      (">", make_ext "gt_string" 2
        (fun args -> VBool (String.compare (as_string (List.nth args 0)) (as_string (List.nth args 1)) > 0)));
      ("<=", make_ext "le_string" 2
        (fun args -> VBool (String.compare (as_string (List.nth args 0)) (as_string (List.nth args 1)) <= 0)));
      (">=", make_ext "ge_string" 2
        (fun args -> VBool (String.compare (as_string (List.nth args 0)) (as_string (List.nth args 1)) >= 0)));
    ] in
  let state = register_instance state
    ~class_name:"Ord" ~tys:[Types.TByte]
    ~methods:[
      ("<", make_ext "lt_byte" 2
        (fun args -> VBool (as_byte (List.nth args 0) < as_byte (List.nth args 1))));
      (">", make_ext "gt_byte" 2
        (fun args -> VBool (as_byte (List.nth args 0) > as_byte (List.nth args 1))));
      ("<=", make_ext "le_byte" 2
        (fun args -> VBool (as_byte (List.nth args 0) <= as_byte (List.nth args 1))));
      (">=", make_ext "ge_byte" 2
        (fun args -> VBool (as_byte (List.nth args 0) >= as_byte (List.nth args 1))));
    ] in
  let state = register_instance state
    ~class_name:"Ord" ~tys:[Types.TRune]
    ~methods:[
      ("<", make_ext "lt_rune" 2
        (fun args -> VBool (as_rune (List.nth args 0) < as_rune (List.nth args 1))));
      (">", make_ext "gt_rune" 2
        (fun args -> VBool (as_rune (List.nth args 0) > as_rune (List.nth args 1))));
      ("<=", make_ext "le_rune" 2
        (fun args -> VBool (as_rune (List.nth args 0) <= as_rune (List.nth args 1))));
      (">=", make_ext "ge_rune" 2
        (fun args -> VBool (as_rune (List.nth args 0) >= as_rune (List.nth args 1))));
    ] in
  (* ---- Bitwise class: land lor lxor lsl lsr : 'a -> 'a -> 'a; lnot : 'a -> 'a ---- *)
  let state = register_class state
    ~name:"Bitwise" ~tyvars:["a"] ~fundeps:[]
    ~methods:[("land", a2a); ("lor", a2a); ("lxor", a2a);
              ("lsl", a2a); ("lsr", a2a); ("lnot", a_a)] in
  let state = register_instance state
    ~class_name:"Bitwise" ~tys:[Types.TInt]
    ~methods:[
      ("land", make_ext "band_int" 2
        (fun args -> VInt (as_int (List.nth args 0) land as_int (List.nth args 1))));
      ("lor", make_ext "bor_int" 2
        (fun args -> VInt (as_int (List.nth args 0) lor as_int (List.nth args 1))));
      ("lxor", make_ext "bxor_int" 2
        (fun args -> VInt (as_int (List.nth args 0) lxor as_int (List.nth args 1))));
      ("lsl", make_ext "bshl_int" 2
        (fun args -> VInt (as_int (List.nth args 0) lsl as_int (List.nth args 1))));
      ("lsr", make_ext "bshr_int" 2
        (fun args -> VInt (as_int (List.nth args 0) lsr as_int (List.nth args 1))));
      ("lnot", make_ext "bnot_int" 1
        (fun args -> VInt (lnot (as_int (List.nth args 0)))));
    ] in
  (* Show class: show : 'a -> string *)
  let state = register_class state
    ~name:"Show" ~tyvars:["a"] ~fundeps:[]
    ~methods:[("show", Types.TArrow (Types.TGen 0, Types.EffEmpty, Types.TString))] in
  let state = register_instance state
    ~class_name:"Show" ~tys:[Types.TInt]
    ~methods:[("show", make_ext "show_int" 1
      (fun args -> Bytecode.VString (string_of_int (as_int (List.nth args 0)))))] in
  let state = register_instance state
    ~class_name:"Show" ~tys:[Types.TFloat]
    ~methods:[("show", make_ext "show_float" 1
      (fun args -> Bytecode.VString (Printf.sprintf "%g" (as_float (List.nth args 0)))))] in
  let state = register_instance state
    ~class_name:"Show" ~tys:[Types.TBool]
    ~methods:[("show", make_ext "show_bool" 1
      (fun args -> Bytecode.VString
        (if as_bool (List.nth args 0) then "true" else "false")))] in
  let state = register_instance state
    ~class_name:"Show" ~tys:[Types.TString]
    ~methods:[("show", make_ext "show_string" 1
      (fun args -> List.nth args 0))] in
  let state = register_instance state
    ~class_name:"Show" ~tys:[Types.TUnit]
    ~methods:[("show", make_ext "show_unit" 1
      (fun _args -> Bytecode.VString "()"))] in
  let state = register_instance state
    ~class_name:"Show" ~tys:[Types.TByte]
    ~methods:[("show", make_ext "show_byte" 1
      (fun args -> Bytecode.VString (Printf.sprintf "#%02x" (as_byte (List.nth args 0)))))] in
  let state = register_instance state
    ~class_name:"Show" ~tys:[Types.TRune]
    ~methods:[("show", make_ext "show_rune" 1
      (fun args -> Bytecode.VString (Bytecode.pp_value (List.nth args 0))))] in
  (* Iter class: fold : ('c -> 'b -> 'c) -> 'c -> 'a -> 'c *)
  (* TGen 0='a (collection), TGen 1='b (element), TGen 2='c (accumulator) *)
  let fold_ty = Types.TArrow(
    Types.TArrow(Types.TGen 2, Types.EffEmpty, Types.TArrow(Types.TGen 1, Types.EffEmpty, Types.TGen 2)),
    Types.EffEmpty, Types.TArrow(Types.TGen 2, Types.EffEmpty, Types.TArrow(Types.TGen 0, Types.EffEmpty, Types.TGen 2))) in
  let state = register_class state
    ~name:"Iter" ~tyvars:["a"; "b"]
    ~fundeps:[Types.{ fd_from = [0]; fd_to = [1] }]
    ~methods:[("fold", fold_ty)] in
  let state = eval_setup state Stdlib_sources.iter in
  (*
  class Map 'm 'k 'v =
    of_list : ('k * 'v) list -> 'm
    get     : 'k -> 'm -> 'v option
    set     : 'k -> 'v -> 'm -> 'm
    has     : 'k -> 'm -> bool
    remove  : 'k -> 'm -> 'm
    size    : 'm -> int
    keys    : 'm -> 'k list
    values  : 'm -> 'v list
    to_list : 'm -> ('k * 'v) list
  *)
  (* TGen 0='m (collection), TGen 1='k (key), TGen 2='v (value) *)
  let option_v = Types.TVariant ("option", [Types.TGen 2]) in
  let of_list_ty = Types.TArrow(
    Types.TList (Types.TTuple [Types.TGen 1; Types.TGen 2]),
    Types.EffEmpty, Types.TGen 0) in
  let get_ty =
    Types.TArrow(Types.TGen 1, Types.EffEmpty, Types.TArrow(Types.TGen 0, Types.EffEmpty, option_v)) in
  let set_ty =
    Types.TArrow(Types.TGen 1, Types.EffEmpty, Types.TArrow(Types.TGen 2, Types.EffEmpty,
      Types.TArrow(Types.TGen 0, Types.EffEmpty, Types.TGen 0))) in
  let has_ty =
    Types.TArrow(Types.TGen 1, Types.EffEmpty, Types.TArrow(Types.TGen 0, Types.EffEmpty, Types.TBool)) in
  let remove_ty =
    Types.TArrow(Types.TGen 1, Types.EffEmpty, Types.TArrow(Types.TGen 0, Types.EffEmpty, Types.TGen 0)) in
  let size_ty =
    Types.TArrow(Types.TGen 0, Types.EffEmpty, Types.TInt) in
  let keys_ty =
    Types.TArrow(Types.TGen 0, Types.EffEmpty, Types.TList (Types.TGen 1)) in
  let values_ty =
    Types.TArrow(Types.TGen 0, Types.EffEmpty, Types.TList (Types.TGen 2)) in
  let to_list_ty = Types.TArrow(
    Types.TGen 0,
    Types.EffEmpty, Types.TList (Types.TTuple [Types.TGen 1; Types.TGen 2])) in
  let state = register_class state
    ~name:"Map" ~tyvars:["m"; "k"; "v"]
    ~fundeps:[Types.{ fd_from = [0]; fd_to = [1; 2] }]
    ~methods:[
      ("of_list", of_list_ty);
      ("get", get_ty);
      ("set", set_ty);
      ("has", has_ty);
      ("remove", remove_ty);
      ("size", size_ty);
      ("keys", keys_ty);
      ("values", values_ty);
      ("to_list", to_list_ty);
    ] in
  (* Native map instance: instance Map (('k, 'v) map) 'k 'v *)
  let state = register_instance state
    ~class_name:"Map"
    ~tys:[Types.TMap (Types.TGen 0, Types.TGen 1); Types.TGen 0; Types.TGen 1]
    ~methods:[
      ("of_list", make_ext "map_of_list" 1
        (fun args ->
          let lst = match List.nth args 0 with
            | Bytecode.VList l -> l
            | v -> raise (Vm.Runtime_error
                (Printf.sprintf "expected list, got %s" (Bytecode.pp_value v)))
          in
          let pairs = List.map (fun v ->
            match v with
            | Bytecode.VTuple [|k; v|] -> (k, v)
            | v -> raise (Vm.Runtime_error
                (Printf.sprintf "expected tuple pair, got %s" (Bytecode.pp_value v)))
          ) lst in
          Bytecode.VMap pairs));
      ("get", make_ext "map_get" 2
        (fun args ->
          let key = List.nth args 0 in
          let pairs = as_map (List.nth args 1) in
          match List.assoc_opt key pairs with
          | Some v -> Bytecode.VVariant (1, "Some", Some v)
          | None -> Bytecode.VVariant (0, "None", None)));
      ("set", make_ext "map_set" 3
        (fun args ->
          let k = List.nth args 0 in
          let v = List.nth args 1 in
          let pairs = as_map (List.nth args 2) in
          let updated = (k, v) :: List.filter (fun (k2, _) -> k2 <> k) pairs in
          Bytecode.VMap updated));
      ("has", make_ext "map_has" 2
        (fun args ->
          let key = List.nth args 0 in
          let pairs = as_map (List.nth args 1) in
          Bytecode.VBool (List.mem_assoc key pairs)));
      ("remove", make_ext "map_remove" 2
        (fun args ->
          let key = List.nth args 0 in
          let pairs = as_map (List.nth args 1) in
          Bytecode.VMap (List.filter (fun (k, _) -> k <> key) pairs)));
      ("size", make_ext "map_size" 1
        (fun args ->
          let pairs = as_map (List.nth args 0) in
          Bytecode.VInt (List.length pairs)));
      ("keys", make_ext "map_keys" 1
        (fun args ->
          let pairs = as_map (List.nth args 0) in
          Bytecode.VList (List.map fst pairs)));
      ("values", make_ext "map_values" 1
        (fun args ->
          let pairs = as_map (List.nth args 0) in
          Bytecode.VList (List.map snd pairs)));
      ("to_list", make_ext "map_to_list" 1
        (fun args ->
          let pairs = as_map (List.nth args 0) in
          Bytecode.VList (List.map (fun (k, v) -> Bytecode.VTuple [|k; v|]) pairs)));
    ] in
  (* ---- Index class: at : 'k -> 'c -> 'v ---- *)
  (* TGen 0='c (container), TGen 1='k (key/index), TGen 2='v (value/element) *)
  let at_ty = Types.TArrow(Types.TGen 1, Types.EffEmpty,
    Types.TArrow(Types.TGen 0, Types.EffEmpty, Types.TGen 2)) in
  let state = register_class state
    ~name:"Index" ~tyvars:["c"; "k"; "v"]
    ~fundeps:[Types.{ fd_from = [0]; fd_to = [1; 2] }]
    ~methods:[("at", at_ty)] in
  let state = register_instance state
    ~class_name:"Index" ~tys:[Types.TArray (Types.TGen 0); Types.TInt; Types.TGen 0]
    ~methods:[
      ("at", make_ext "index_at_array" 2
        (fun args ->
          let idx = as_int (List.nth args 0) in
          let arr = as_array (List.nth args 1) in
          if idx < 0 || idx >= Array.length arr then
            raise (Vm.Runtime_error (Printf.sprintf
              "[line 0] array index out of bounds: %d (length %d)" idx (Array.length arr)));
          arr.(idx)));
    ] in
  let state = register_instance state
    ~class_name:"Index" ~tys:[Types.TString; Types.TInt; Types.TByte]
    ~methods:[
      ("at", make_ext "index_at_string" 2
        (fun args ->
          let idx = as_int (List.nth args 0) in
          let s = as_string (List.nth args 1) in
          if idx < 0 || idx >= String.length s then
            raise (Vm.Runtime_error (Printf.sprintf
              "[line 0] string index out of bounds: %d (length %d)" idx (String.length s)));
          Bytecode.VByte (Char.code s.[idx])));
    ] in
  let state = register_instance state
    ~class_name:"Index" ~tys:[Types.TMap (Types.TGen 0, Types.TGen 1); Types.TGen 0; Types.TGen 1]
    ~methods:[
      ("at", make_ext "index_at_map" 2
        (fun args ->
          let key = List.nth args 0 in
          let pairs = as_map (List.nth args 1) in
          match List.assoc_opt key pairs with
          | Some v -> v
          | None -> raise (Vm.Runtime_error
              (Printf.sprintf "[line 0] key not found: %s" (Bytecode.pp_value key)))));
    ] in
  let state = eval_setup state Stdlib_sources.show in
  let state = eval_setup state Stdlib_sources.iter_map in
  (* Map/Set show as native instances — multi-param class methods don't
     propagate type info between params during inference, so we use pp_value *)
  let state = register_instance state
    ~class_name:"Show" ~tys:[Types.TMap (Types.TGen 0, Types.TGen 1)]
    ~methods:[("show", make_ext "show_map" 1
      (fun args -> Bytecode.VString (Bytecode.pp_value (List.nth args 0))))] in
  (* Build Stdlib module_info and register it *)
  let stdlib_constructors =
    List.filter_map (fun (name, info) ->
      if name = "None" || name = "Some" then
        Some (name, info)
      else None
    ) state.ctx.type_env.Types.constructors
  in
  let stdlib_info = Types.{
    mod_name = "Stdlib";
    mod_pub_vars = stdlib_pub_vars;
    mod_pub_mutable_vars = [];
    mod_pub_types = [];
    mod_opaque_types = [];
    mod_pub_constructors = stdlib_constructors;
    mod_instances = [];
    mod_submodules = [];
    mod_pub_classes = [];
  } in
  let type_env = { state.ctx.type_env with
    Types.modules = ("Stdlib", stdlib_info) :: state.ctx.type_env.Types.modules;
  } in
  { state with ctx = Typechecker.{ state.ctx with type_env } }

(* ---- Non-REPL execution ---- *)

(* ---- Dynamic eval support ---- *)

let eval_state : repl_state option ref = ref None
(* Flag set when eval_source updates eval_state during VM execution *)
let eval_state_changed = ref false

let eval_source source =
  match !eval_state with
  | None -> raise (Error "eval: interpreter state not initialized")
  | Some state ->
    let tokens = Lexer.tokenize source in
    let program = Parser.parse_program tokens in
    let (ctx', typed_program) =
      Typechecker.check_program_in_ctx state.ctx program in
    let typed_program = Typechecker.transform_constraints ctx' typed_program in
    let compiled =
      Compiler.compile_program_with_globals
        ctx'.Typechecker.type_env state.global_names state.mutable_globals typed_program in
    let result = Vm.execute_with_globals compiled state.globals in
    eval_state := Some { state with ctx = ctx' };
    eval_state_changed := true;
    result

let run_string_in_state state source =
  let tokens = Lexer.tokenize source in
  let program = Parser.parse_program tokens in
  let (ctx', typed_program) =
    Typechecker.check_program_in_ctx state.ctx program
  in
  let typed_program = Typechecker.transform_constraints ctx' typed_program in
  let compiled =
    Compiler.compile_program_with_globals
      ctx'.Typechecker.type_env state.global_names state.mutable_globals typed_program
  in
  (* Update eval state so Runtime.eval can see all bindings *)
  eval_state := Some { state with ctx = ctx' };
  eval_state_changed := false;
  let result = Vm.execute_with_globals compiled state.globals in
  (* If Runtime.eval/eval_file ran during execution, pick up their context *)
  if !eval_state_changed then
    (match !eval_state with
     | Some es -> eval_state := Some { state with ctx = es.ctx }
     | None -> ());
  result

let run_string source =
  wrap_errors (fun () ->
    let (ctx, global_names, globals, stdlib_pub_vars) = setup_builtins () in
    let mutable_globals = Hashtbl.create 8 in
    let state = { ctx; global_names; mutable_globals; globals } in
    let state = setup_default_classes state stdlib_pub_vars in
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
  with
  | Error msg -> Printf.eprintf "%s\n" msg

let run_file_show filename =
  try
    let _ = run_file filename in
    ()
  with
  | Error msg ->
    Printf.eprintf "%s\n" msg;
    exit 1

(* ---- REPL with persistent state ---- *)

let repl_state_init () =
  let (ctx, global_names, globals, stdlib_pub_vars) = setup_builtins () in
  let mutable_globals = Hashtbl.create 8 in
  let state = { ctx; global_names; mutable_globals; globals } in
  setup_default_classes state stdlib_pub_vars

let start_capture () =
  captured_setups := [];
  capture_mode := true

let stop_capture () =
  capture_mode := false

let emit_json_bundle state ?source () =
  let main_proto = match source with
    | None ->
      Bytecode.{
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
      let (ctx', typed_program) =
        Typechecker.check_program_in_ctx state.ctx program in
      let typed_program = Typechecker.transform_constraints ctx' typed_program in
      let compiled =
        Compiler.compile_program_with_globals
          ctx'.Typechecker.type_env state.global_names state.mutable_globals typed_program in
      compiled.main
  in
  let native_globals_json = Serialize.build_native_globals_json state.globals in
  Serialize.serialize_bundle
    ~global_names:state.global_names
    ~native_globals_json
    ~setup_protos:!captured_setups
    ~main_proto

let emit_binary_bundle state ?source () =
  let main_proto = match source with
    | None ->
      Bytecode.{
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
      let (ctx', typed_program) =
        Typechecker.check_program_in_ctx state.ctx program in
      let typed_program = Typechecker.transform_constraints ctx' typed_program in
      let compiled =
        Compiler.compile_program_with_globals
          ctx'.Typechecker.type_env state.global_names state.mutable_globals typed_program in
      compiled.main
  in
  Serialize_bin.serialize_bundle
    ~global_names:state.global_names
    ~globals:state.globals
    ~setup_protos:!captured_setups
    ~main_proto

let eval_repl state source =
  wrap_errors (fun () ->
    let tokens = Lexer.tokenize source in
    let program = Parser.parse_program tokens in
    let (ctx', typed_program) =
      Typechecker.check_program_in_ctx state.ctx program
    in
    let typed_program = Typechecker.transform_constraints ctx' typed_program in
    let type_info = match List.rev typed_program with
      | Typechecker.TDExpr te :: _ -> Some (None, Types.pp_ty te.ty)
      | Typechecker.TDLet (name, te) :: _ -> Some (Some name, Types.pp_ty te.ty)
      | Typechecker.TDLetMut (name, te) :: _ -> Some (Some name, Types.pp_ty te.ty)
      | Typechecker.TDLetRec (name, te) :: _ -> Some (Some name, Types.pp_ty te.ty)
      | _ -> None
    in
    let compiled =
      Compiler.compile_program_with_globals
        ctx'.Typechecker.type_env state.global_names state.mutable_globals typed_program
    in
    eval_state_changed := false;
    let result = Vm.execute_with_globals compiled state.globals in
    (* If Runtime.eval/eval_file ran during execution, they updated eval_state
       with new modules/bindings. Use that ctx instead of our local ctx'. *)
    let final_ctx = if !eval_state_changed then
      match !eval_state with
      | Some es -> es.ctx
      | None -> ctx'
    else ctx' in
    ({ state with ctx = final_ctx }, result, type_info))

let typeof_source state source =
  wrap_errors (fun () ->
    let tokens = Lexer.tokenize source in
    let program = Parser.parse_program tokens in
    let (_, typed_program) =
      Typechecker.check_program_in_ctx state.ctx program
    in
    match List.rev typed_program with
    | Typechecker.TDExpr te :: _ -> Types.pp_ty te.ty
    | Typechecker.TDLet (name, te) :: _ ->
      name ^ " : " ^ Types.pp_ty te.ty
    | Typechecker.TDLetMut (name, te) :: _ ->
      name ^ " : " ^ Types.pp_ty te.ty ^ " (mutable)"
    | Typechecker.TDLetRec (name, te) :: _ ->
      name ^ " : " ^ Types.pp_ty te.ty
    | Typechecker.TDExtern (name, scheme) :: _ ->
      "extern " ^ name ^ " : " ^ Types.pp_ty scheme.Types.body
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
  let matching = List.filter_map (fun (name, info) ->
    if String.equal name mod_name then Some info else None
  ) modules in
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
      else "(" ^ String.concat ", " (List.init num_params (fun i ->
        "'" ^ String.make 1 (Char.chr (Char.code 'a' + i))
      )) ^ ") "
    in
    (* Collect module-scoped type synonyms for pretty-printing *)
    let module_synonyms = ref [] in
    (* Standard library synonyms: name matches lowercase module name (e.g. "set" for Set) *)
    let low_mod = String.lowercase_ascii mod_name in
    List.iter (fun (syn_name, num_params, expanded_ty) ->
      if String.equal syn_name low_mod then begin
        module_synonyms := (syn_name, num_params, expanded_ty) :: !module_synonyms;
        Buffer.add_string buf (Printf.sprintf "  type %s%s = %s\n"
          (format_params num_params) syn_name (Types.pp_ty_raw expanded_ty))
      end
    ) all_synonyms;
    List.iter (fun (minfo : Types.module_info) ->
      (* Types: look up synonym expansion for type aliases *)
      let mod_prefix = mod_name ^ "." in
      let shown_variant_types = Hashtbl.create 8 in
      List.iter (fun tname ->
        let short = match String.rindex_opt tname '.' with
          | Some i -> String.sub tname (i + 1) (String.length tname - i - 1)
          | None -> tname
        in
        match List.find_opt (fun (n, _, _) -> String.equal n tname) all_synonyms with
        | Some (_, num_params, expanded_ty) ->
          module_synonyms := (short, num_params, expanded_ty) :: !module_synonyms;
          Buffer.add_string buf (Printf.sprintf "  type %s%s = %s\n"
            (format_params num_params) short (Types.pp_ty_raw expanded_ty))
        | None ->
          let display = if String.length tname > String.length mod_prefix
            && String.sub tname 0 (String.length mod_prefix) = mod_prefix
            then short else tname in
          let is_opaque = List.mem tname minfo.mod_opaque_types in
          if is_opaque then
            Buffer.add_string buf (Printf.sprintf "  type %s\n" display)
          else begin
            (* Show full definition for public types *)
            match List.find_opt (fun (n, _, _, _) -> String.equal n tname) all_variants with
            | Some (_, num_params, ctors, _) ->
              Hashtbl.replace shown_variant_types tname true;
              Buffer.add_string buf (Printf.sprintf "  type %s%s =\n"
                (format_params num_params) display);
              List.iter (fun (cname, arg_ty) ->
                let ctor_short = match String.rindex_opt cname '.' with
                  | Some i -> String.sub cname (i + 1) (String.length cname - i - 1)
                  | None -> cname
                in
                match arg_ty with
                | None -> Buffer.add_string buf (Printf.sprintf "    | %s\n" ctor_short)
                | Some t -> Buffer.add_string buf (Printf.sprintf "    | %s of %s\n" ctor_short (Types.pp_ty t))
              ) ctors
            | None ->
              match List.find_opt (fun (n, _) -> String.equal n tname) all_records with
              | Some (_, fields) ->
                Buffer.add_string buf (Printf.sprintf "  type %s%s = {\n"
                  (format_params 0) display);
                List.iter (fun (fname, fty) ->
                  let mut_prefix = if List.mem fname type_env.Types.mutable_fields then "mut " else "" in
                  Buffer.add_string buf (Printf.sprintf "    %s%s: %s;\n" mut_prefix fname (Types.pp_ty fty))
                ) fields;
                Buffer.add_string buf "  }\n"
              | None ->
                Buffer.add_string buf (Printf.sprintf "  type %s\n" display)
          end
      ) minfo.mod_pub_types;
      (* Constructors — only show those not already shown in type definitions *)
      List.iter (fun (cname, cinfo) ->
        if not (Hashtbl.mem shown_variant_types cinfo.Types.ctor_type_name) then begin
          let ty_str = match cinfo.Types.ctor_arg_ty with
            | None -> cname
            | Some t -> cname ^ " of " ^ Types.pp_ty t
          in
          Buffer.add_string buf (Printf.sprintf "  %s\n" ty_str)
        end
      ) minfo.mod_pub_constructors;
      (* Classes *)
      List.iter (fun qname ->
        let short = match String.rindex_opt qname '.' with
          | Some i -> String.sub qname (i + 1) (String.length qname - i - 1)
          | None -> qname
        in
        match List.find_opt (fun (c : Types.class_def) ->
          String.equal c.class_name qname
        ) state.ctx.Typechecker.type_env.Types.classes with
        | Some cls ->
          Buffer.add_string buf (Printf.sprintf "  class %s %s =\n"
            short (String.concat " " (List.map (fun p -> "'" ^ p) cls.class_params)));
          List.iter (fun (mname, mty) ->
            Buffer.add_string buf (Printf.sprintf "    val %s : %s\n" mname (Types.pp_ty mty))
          ) cls.class_methods
        | None -> ()
      ) minfo.mod_pub_classes;
      (* Temporarily add module synonyms for function signature printing *)
      let saved_synonyms = !Types.pp_synonyms in
      Types.pp_synonyms := !module_synonyms @ saved_synonyms;
      (* Functions/values — skip duplicates *)
      List.iter (fun (name, scheme) ->
        if not (Hashtbl.mem seen_vars name) then begin
          Hashtbl.replace seen_vars name true;
          Buffer.add_string buf (Printf.sprintf "  val %s : %s\n" name (Types.pp_ty scheme.Types.body))
        end
      ) (List.rev minfo.mod_pub_vars);
      Types.pp_synonyms := saved_synonyms;
      (* Submodules *)
      List.iter (fun (subname, _) ->
        Buffer.add_string buf (Printf.sprintf "  module %s\n" subname)
      ) minfo.mod_submodules;
    ) matching;
    String.trim (Buffer.contents buf)

let list_classes state =
  let type_env = state.ctx.Typechecker.type_env in
  let classes = type_env.Types.classes in
  let instances = type_env.Types.instances in
  if classes = [] then "No type classes defined."
  else begin
    let buf = Buffer.create 256 in
    List.iter (fun (cls : Types.class_def) ->
      Buffer.add_string buf (Printf.sprintf "class %s %s:\n"
        cls.class_name
        (String.concat " " (List.map (fun p -> "'" ^ p) cls.class_params)));
      List.iter (fun (meth_name, meth_ty) ->
        Buffer.add_string buf (Printf.sprintf "  val %s : %s\n" meth_name (Types.pp_ty meth_ty))
      ) cls.class_methods;
      (* Show instances for this class *)
      let cls_instances = List.filter (fun (inst : Types.instance_def) ->
        String.equal inst.inst_class cls.class_name
      ) instances in
      List.iter (fun (inst : Types.instance_def) ->
        let tys_str = String.concat " " (List.map Types.pp_ty inst.inst_tys) in
        let constraints_str = match inst.inst_constraints with
          | [] -> ""
          | cs -> " where " ^ String.concat ", " (List.map (fun (c : Types.class_constraint) ->
              c.cc_class ^ " " ^ String.concat " " (List.map (fun (ca : Types.class_arg) ->
                match ca with
                | CATGen i -> "'" ^ String.make 1 (Char.chr (Char.code 'a' + i))
                | CATy ty -> Types.pp_ty ty
                | CAWild -> "_"
                | CAPhantom _ -> "_"
              ) c.cc_args)
            ) cs)
        in
        Buffer.add_string buf (Printf.sprintf "  instance %s%s\n" tys_str constraints_str)
      ) cls_instances;
    ) classes;
    String.trim (Buffer.contents buf)
  end

let list_externs state =
  (* Collect all VExternal entries from globals, grouped by module *)
  let toplevel = Buffer.create 256 in
  let modules : (string, (string * string) list ref) Hashtbl.t = Hashtbl.create 16 in
  Dynarray.iteri (fun idx name ->
    match Hashtbl.find_opt state.globals idx with
    | Some (Bytecode.VExternal _) ->
      (* Skip Stdlib. duplicates *)
      if not (String.length name > 7 && String.sub name 0 7 = "Stdlib.") then begin
        let ty_str = match List.assoc_opt name state.ctx.Typechecker.vars with
          | Some scheme -> Types.pp_ty scheme.Types.body
          | None -> "?"
        in
        match String.index_opt name '.' with
        | Some i ->
          let mod_name = String.sub name 0 i in
          let fn_name = String.sub name (i + 1) (String.length name - i - 1) in
          let entries = match Hashtbl.find_opt modules mod_name with
            | Some r -> r
            | None -> let r = ref [] in Hashtbl.replace modules mod_name r; r
          in
          entries := (fn_name, ty_str) :: !entries
        | None ->
          Buffer.add_string toplevel (Printf.sprintf "  %s : %s\n" name ty_str)
      end
    | _ -> ()
  ) state.global_names;
  let buf = Buffer.create 512 in
  if Buffer.length toplevel > 0 then begin
    Buffer.add_string buf "Builtins:\n";
    Buffer.add_buffer buf toplevel
  end;
  let mod_names = Hashtbl.fold (fun k _ acc -> k :: acc) modules [] in
  let mod_names = List.sort String.compare mod_names in
  List.iter (fun mod_name ->
    Buffer.add_string buf (Printf.sprintf "%s:\n" mod_name);
    let entries = List.rev !(Hashtbl.find modules mod_name) in
    List.iter (fun (fn_name, ty_str) ->
      Buffer.add_string buf (Printf.sprintf "  %s : %s\n" fn_name ty_str)
    ) entries
  ) mod_names;
  let s = Buffer.contents buf in
  if String.length s = 0 then "No externals registered."
  else String.trim s

let eval_repl_show state source =
  try
    let (state', result, type_info) = eval_repl state source in
    (match result, type_info with
     | Bytecode.VUnit, Some (Some name, ty_str) ->
       Printf.printf "%s : %s\n%!" name ty_str
     | Bytecode.VUnit, _ -> ()
     | v, Some (Some name, ty_str) ->
       Printf.printf "%s : %s = %s\n%!" name ty_str (Bytecode.pp_value v)
     | v, Some (None, ty_str) ->
       Printf.printf "%s : %s\n%!" (Bytecode.pp_value v) ty_str
     | v, None ->
       print_endline (Bytecode.pp_value v));
    state'
  with
  | Error msg ->
    Printf.eprintf "%s\n%!" msg;
    state
