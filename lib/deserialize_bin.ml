(* Binary deserialization for bytecode bundles *)
(* Matches the wire format produced by Serialize_bin *)

(* --- Reader --- *)

type reader = {
  data: string;
  mutable pos: int;
}

let make_reader data = { data; pos = 0 }

let read_u8 r =
  let b = Char.code r.data.[r.pos] in
  r.pos <- r.pos + 1;
  b

let read_u32 r =
  let b0 = Char.code r.data.[r.pos] in
  let b1 = Char.code r.data.[r.pos + 1] in
  let b2 = Char.code r.data.[r.pos + 2] in
  let b3 = Char.code r.data.[r.pos + 3] in
  r.pos <- r.pos + 4;
  b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)

let read_i64 r =
  let n = ref 0L in
  for i = 0 to 7 do
    n := Int64.logor !n
      (Int64.shift_left (Int64.of_int (Char.code r.data.[r.pos + i])) (i * 8))
  done;
  r.pos <- r.pos + 8;
  Int64.to_int !n

let read_f64 r =
  let bits = ref 0L in
  for i = 0 to 7 do
    bits := Int64.logor !bits
      (Int64.shift_left (Int64.of_int (Char.code r.data.[r.pos + i])) (i * 8))
  done;
  r.pos <- r.pos + 8;
  Int64.float_of_bits !bits

(* --- Read helpers --- *)

let read_string_table r =
  let count = read_u32 r in
  Array.init count (fun _ ->
    let len = read_u32 r in
    let s = String.sub r.data r.pos len in
    r.pos <- r.pos + len;
    s)

let read_captures r =
  let count = read_u32 r in
  List.init count (fun _ ->
    let kind = read_u8 r in
    let idx = read_u32 r in
    if kind = 0 then Bytecode.CaptureLocal idx
    else Bytecode.CaptureUpvalue idx)

let read_opcode r strs =
  let tag = read_u8 r in
  match tag with
  | 0 -> Bytecode.CONST (read_u32 r)
  | 1 -> Bytecode.POP
  | 2 -> Bytecode.DUP
  | 3 -> Bytecode.GET_LOCAL (read_u32 r)
  | 4 -> Bytecode.SET_LOCAL (read_u32 r)
  | 5 -> Bytecode.GET_UPVALUE (read_u32 r)
  | 6 -> Bytecode.SET_UPVALUE (read_u32 r)
  | 7 -> Bytecode.MAKE_REF
  | 8 -> Bytecode.DEREF
  | 9 -> Bytecode.SET_REF
  | 10 -> Bytecode.GET_GLOBAL (read_u32 r)
  | 11 -> Bytecode.SET_GLOBAL (read_u32 r)
  | 12 -> Bytecode.DEF_GLOBAL (read_u32 r)
  | 13 -> Bytecode.ADD
  | 14 -> Bytecode.SUB
  | 15 -> Bytecode.MUL
  | 16 -> Bytecode.DIV
  | 17 -> Bytecode.MOD
  | 18 -> Bytecode.NEG
  | 19 -> Bytecode.FADD
  | 20 -> Bytecode.FSUB
  | 21 -> Bytecode.FMUL
  | 22 -> Bytecode.FDIV
  | 23 -> Bytecode.FNEG
  | 24 -> Bytecode.EQ
  | 25 -> Bytecode.NEQ
  | 26 -> Bytecode.LT
  | 27 -> Bytecode.GT
  | 28 -> Bytecode.LE
  | 29 -> Bytecode.GE
  | 30 -> Bytecode.NOT
  | 31 -> Bytecode.BAND
  | 32 -> Bytecode.BOR
  | 33 -> Bytecode.BXOR
  | 34 -> Bytecode.BNOT
  | 35 -> Bytecode.BSHL
  | 36 -> Bytecode.BSHR
  | 37 -> Bytecode.JUMP (read_u32 r)
  | 38 -> Bytecode.JUMP_IF_FALSE (read_u32 r)
  | 39 -> Bytecode.JUMP_IF_TRUE (read_u32 r)
  | 40 ->
    let proto_idx = read_u32 r in
    let caps = read_captures r in
    Bytecode.CLOSURE (proto_idx, caps)
  | 41 ->
    let proto_idx = read_u32 r in
    let caps = read_captures r in
    let self = read_u32 r in
    Bytecode.CLOSURE_REC (proto_idx, caps, self)
  | 42 -> Bytecode.CALL (read_u32 r)
  | 43 -> Bytecode.TAIL_CALL (read_u32 r)
  | 44 -> Bytecode.RETURN
  | 45 -> Bytecode.FUNC_RETURN
  | 46 -> Bytecode.ENTER_FUNC
  | 47 -> Bytecode.EXIT_FUNC
  | 48 -> Bytecode.MAKE_TUPLE (read_u32 r)
  | 49 -> Bytecode.TUPLE_GET (read_u32 r)
  | 50 ->
    let count = read_u32 r in
    let fields = List.init count (fun _ -> strs.(read_u32 r)) in
    Bytecode.MAKE_RECORD fields
  | 51 -> Bytecode.FIELD strs.(read_u32 r)
  | 52 -> Bytecode.SET_FIELD strs.(read_u32 r)
  | 53 ->
    let tag_val = read_u32 r in
    let name = strs.(read_u32 r) in
    let has_payload = read_u8 r <> 0 in
    Bytecode.MAKE_VARIANT (tag_val, name, has_payload)
  | 54 -> Bytecode.CONS
  | 55 -> Bytecode.NIL
  | 56 -> Bytecode.TAG_EQ (read_u32 r)
  | 57 -> Bytecode.IS_NIL
  | 58 -> Bytecode.IS_CONS
  | 59 -> Bytecode.HEAD
  | 60 -> Bytecode.TAIL
  | 61 -> Bytecode.VARIANT_PAYLOAD
  | 62 -> Bytecode.MATCH_FAIL strs.(read_u32 r)
  | 63 -> Bytecode.PERFORM strs.(read_u32 r)
  | 64 -> Bytecode.HANDLE (read_u32 r)
  | 65 -> Bytecode.RESUME
  | 66 -> Bytecode.ENTER_LOOP (read_u32 r)
  | 67 -> Bytecode.EXIT_LOOP
  | 68 -> Bytecode.LOOP_BREAK
  | 69 -> Bytecode.LOOP_CONTINUE (read_u32 r)
  | 70 -> Bytecode.FOLD_CONTINUE (read_u32 r)
  | 71 -> Bytecode.MAKE_MAP (read_u32 r)
  | 72 -> Bytecode.MAKE_ARRAY (read_u32 r)
  | 73 -> Bytecode.INDEX
  | 74 -> Bytecode.HALT
  | 75 ->
    let count = read_u32 r in
    let fields = List.init count (fun _ -> strs.(read_u32 r)) in
    Bytecode.RECORD_UPDATE fields
  | 76 -> Bytecode.RECORD_UPDATE_DYN (read_u32 r)
  | 77 ->
    let slot = read_u32 r in
    let arity = read_u32 r in
    Bytecode.GET_LOCAL_CALL (slot, arity)
  | 78 ->
    let slot = read_u32 r in
    let idx = read_u32 r in
    Bytecode.GET_LOCAL_TUPLE_GET (slot, idx)
  | 79 ->
    let slot = read_u32 r in
    let name = strs.(read_u32 r) in
    Bytecode.GET_LOCAL_FIELD (slot, name)
  | 80 ->
    let min_tag = read_u32 r in
    let table_size = read_u32 r in
    let targets = Array.init table_size (fun _ -> read_u32 r) in
    let default = read_u32 r in
    Bytecode.JUMP_TABLE (min_tag, targets, default)
  | n -> failwith (Printf.sprintf "unknown opcode tag: %d" n)

let rec read_value r strs =
  let tag = read_u8 r in
  match tag with
  | 0 -> Bytecode.VInt (read_i64 r)
  | 1 -> Bytecode.VFloat (read_f64 r)
  | 2 -> Bytecode.VBool (read_u8 r <> 0)
  | 3 -> Bytecode.VString strs.(read_u32 r)
  | 4 -> Bytecode.VByte (read_u8 r)
  | 5 -> Bytecode.VRune (read_u32 r)
  | 6 -> Bytecode.VUnit
  | 7 -> Bytecode.VProto (read_prototype r strs)
  | 8 ->
    let count = read_u32 r in
    Bytecode.VTuple (Array.init count (fun _ -> read_value r strs))
  | 9 ->
    let count = read_u32 r in
    Bytecode.VList (List.init count (fun _ -> read_value r strs))
  | 10 ->
    let tag_val = read_u32 r in
    let name = strs.(read_u32 r) in
    let has_payload = read_u8 r <> 0 in
    let payload = if has_payload then Some (read_value r strs) else None in
    Bytecode.VVariant (tag_val, name, payload)
  | n -> failwith (Printf.sprintf "unknown value tag: %d" n)

and read_prototype r strs =
  let name = strs.(read_u32 r) in
  let arity = read_u32 r in
  let num_locals = read_u32 r in
  let code_len = read_u32 r in
  let code = Array.init code_len (fun _ -> read_opcode r strs) in
  let const_len = read_u32 r in
  let constants = Array.init const_len (fun _ -> read_value r strs) in
  let lines_len = read_u32 r in
  let line_table = Array.init lines_len (fun _ -> read_u32 r) in
  Bytecode.{ name; arity; num_locals; code; constants; line_table }

(* --- Bundle loading --- *)

let read_header r =
  let magic = String.sub r.data r.pos 4 in
  r.pos <- r.pos + 4;
  if magic <> "MMLB" then
    failwith (Printf.sprintf "invalid binary bundle magic: %S (expected MMLB)" magic);
  let version = read_u32 r in
  if version <> 1 then
    failwith (Printf.sprintf "unsupported binary bundle version: %d" version)

let run_prototype globals proto =
  let program = Bytecode.{
    main = proto;
    global_names = [||];
  } in
  Vm.execute_with_globals program globals

let resolve_native_globals r strs (builtins : Deserialize.builtin_table) globals =
  let count = read_u32 r in
  for _ = 1 to count do
    let idx = read_u32 r in
    let kind = read_u8 r in
    match kind with
    | 0 -> (* external *)
      let name = strs.(read_u32 r) in
      let arity = read_u32 r in
      let impl = match Hashtbl.find_opt builtins name with
        | Some b -> b.impl
        | None -> (fun _ -> raise (Vm.Runtime_error
            (Printf.sprintf "unregistered builtin: %s" name)))
      in
      Hashtbl.replace globals idx (Bytecode.VExternal {
        ext_name = name;
        ext_arity = arity;
        ext_fn = impl;
        ext_args = [];
      })
    | 1 -> (* dict *)
      let num_fields = read_u32 r in
      let methods = List.init num_fields (fun _ ->
        let field_name = strs.(read_u32 r) in
        let ext_name = strs.(read_u32 r) in
        let ext_arity = read_u32 r in
        let impl = match Hashtbl.find_opt builtins ext_name with
          | Some b -> b.impl
          | None -> (fun _ -> raise (Vm.Runtime_error
              (Printf.sprintf "unregistered builtin: %s" ext_name)))
        in
        (field_name, Bytecode.VExternal {
          ext_name;
          ext_arity;
          ext_fn = impl;
          ext_args = [];
        })
      ) in
      let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) methods in
      let field_names = List.map fst sorted in
      let values = Array.of_list (List.map snd sorted) in
      Hashtbl.replace globals idx (Bytecode.make_record field_names values)
    | n -> failwith (Printf.sprintf "unknown native global kind: %d" n)
  done

let load_bundle_binary data (builtins : Deserialize.builtin_table) =
  let r = make_reader data in
  read_header r;
  let strs = read_string_table r in
  (* Global names *)
  let num_names = read_u32 r in
  let _global_names = Array.init num_names (fun _ -> strs.(read_u32 r)) in
  let globals : (int, Bytecode.value) Hashtbl.t = Hashtbl.create num_names in
  (* Native globals *)
  resolve_native_globals r strs builtins globals;
  (* Setup protos *)
  let num_setup = read_u32 r in
  for _ = 1 to num_setup do
    let proto = read_prototype r strs in
    ignore (run_prototype globals proto)
  done;
  (* Main proto *)
  let main_proto = read_prototype r strs in
  run_prototype globals main_proto

let prepare_bundle_binary data (builtins : Deserialize.builtin_table) =
  let r = make_reader data in
  read_header r;
  let strs = read_string_table r in
  (* Global names *)
  let num_names = read_u32 r in
  let _global_names = Array.init num_names (fun _ -> strs.(read_u32 r)) in
  let globals : (int, Bytecode.value) Hashtbl.t = Hashtbl.create num_names in
  (* Native globals *)
  resolve_native_globals r strs builtins globals;
  (* Setup protos *)
  let num_setup = read_u32 r in
  for _ = 1 to num_setup do
    let proto = read_prototype r strs in
    ignore (run_prototype globals proto)
  done;
  (* Main proto *)
  let main_proto = read_prototype r strs in
  Deserialize.{ prepared_globals = globals; prepared_main = main_proto }
