(* JSON serialization for bytecode *)

let json_escape_string s =
  let buf = Buffer.create (String.length s + 8) in
  Buffer.add_char buf '"';
  String.iter (fun c ->
    match c with
    | '"' -> Buffer.add_string buf {|\"|}
    | '\\' -> Buffer.add_string buf {|\\|}
    | '\n' -> Buffer.add_string buf {|\n|}
    | '\r' -> Buffer.add_string buf {|\r|}
    | '\t' -> Buffer.add_string buf {|\t|}
    | c when Char.code c < 0x20 ->
      Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let serialize_capture = function
  | Bytecode.CaptureLocal i -> Printf.sprintf "[\"local\",%d]" i
  | Bytecode.CaptureUpvalue i -> Printf.sprintf "[\"upvalue\",%d]" i

let serialize_captures caps =
  "[" ^ String.concat "," (List.map serialize_capture caps) ^ "]"

let serialize_opcode = function
  | Bytecode.CONST i -> Printf.sprintf "[\"CONST\",%d]" i
  | POP -> "[\"POP\"]"
  | DUP -> "[\"DUP\"]"
  | GET_LOCAL i -> Printf.sprintf "[\"GET_LOCAL\",%d]" i
  | SET_LOCAL i -> Printf.sprintf "[\"SET_LOCAL\",%d]" i
  | GET_UPVALUE i -> Printf.sprintf "[\"GET_UPVALUE\",%d]" i
  | SET_UPVALUE i -> Printf.sprintf "[\"SET_UPVALUE\",%d]" i
  | MAKE_REF -> "[\"MAKE_REF\"]"
  | DEREF -> "[\"DEREF\"]"
  | SET_REF -> "[\"SET_REF\"]"
  | GET_GLOBAL i -> Printf.sprintf "[\"GET_GLOBAL\",%d]" i
  | SET_GLOBAL i -> Printf.sprintf "[\"SET_GLOBAL\",%d]" i
  | DEF_GLOBAL i -> Printf.sprintf "[\"DEF_GLOBAL\",%d]" i
  | ADD -> "[\"ADD\"]"
  | SUB -> "[\"SUB\"]"
  | MUL -> "[\"MUL\"]"
  | DIV -> "[\"DIV\"]"
  | MOD -> "[\"MOD\"]"
  | NEG -> "[\"NEG\"]"
  | FADD -> "[\"FADD\"]"
  | FSUB -> "[\"FSUB\"]"
  | FMUL -> "[\"FMUL\"]"
  | FDIV -> "[\"FDIV\"]"
  | FNEG -> "[\"FNEG\"]"
  | EQ -> "[\"EQ\"]"
  | NEQ -> "[\"NEQ\"]"
  | LT -> "[\"LT\"]"
  | GT -> "[\"GT\"]"
  | LE -> "[\"LE\"]"
  | GE -> "[\"GE\"]"
  | NOT -> "[\"NOT\"]"
  | BAND -> "[\"BAND\"]"
  | BOR -> "[\"BOR\"]"
  | BXOR -> "[\"BXOR\"]"
  | BNOT -> "[\"BNOT\"]"
  | BSHL -> "[\"BSHL\"]"
  | BSHR -> "[\"BSHR\"]"
  | JUMP off -> Printf.sprintf "[\"JUMP\",%d]" off
  | JUMP_IF_FALSE off -> Printf.sprintf "[\"JUMP_IF_FALSE\",%d]" off
  | JUMP_IF_TRUE off -> Printf.sprintf "[\"JUMP_IF_TRUE\",%d]" off
  | CLOSURE (i, caps) ->
    Printf.sprintf "[\"CLOSURE\",%d,%s]" i (serialize_captures caps)
  | CLOSURE_REC (i, caps, self) ->
    Printf.sprintf "[\"CLOSURE_REC\",%d,%s,%d]" i (serialize_captures caps) self
  | CALL n -> Printf.sprintf "[\"CALL\",%d]" n
  | TAIL_CALL n -> Printf.sprintf "[\"TAIL_CALL\",%d]" n
  | RETURN -> "[\"RETURN\"]"
  | FUNC_RETURN -> "[\"FUNC_RETURN\"]"
  | ENTER_FUNC -> "[\"ENTER_FUNC\"]"
  | EXIT_FUNC -> "[\"EXIT_FUNC\"]"
  | MAKE_TUPLE n -> Printf.sprintf "[\"MAKE_TUPLE\",%d]" n
  | TUPLE_GET i -> Printf.sprintf "[\"TUPLE_GET\",%d]" i
  | MAKE_RECORD fields ->
    Printf.sprintf "[\"MAKE_RECORD\",[%s]]"
      (String.concat "," (List.map json_escape_string fields))
  | FIELD name -> Printf.sprintf "[\"FIELD\",%s]" (json_escape_string name)
  | SET_FIELD name -> Printf.sprintf "[\"SET_FIELD\",%s]" (json_escape_string name)
  | RECORD_UPDATE fields ->
    Printf.sprintf "[\"RECORD_UPDATE\",[%s]]"
      (String.concat "," (List.map json_escape_string fields))
  | RECORD_UPDATE_DYN n -> Printf.sprintf "[\"RECORD_UPDATE_DYN\",%d]" n
  | MAKE_VARIANT (tag, name, has_payload) ->
    Printf.sprintf "[\"MAKE_VARIANT\",%d,%s,%b]" tag (json_escape_string name) has_payload
  | CONS -> "[\"CONS\"]"
  | NIL -> "[\"NIL\"]"
  | TAG_EQ tag -> Printf.sprintf "[\"TAG_EQ\",%d]" tag
  | IS_NIL -> "[\"IS_NIL\"]"
  | IS_CONS -> "[\"IS_CONS\"]"
  | HEAD -> "[\"HEAD\"]"
  | TAIL -> "[\"TAIL\"]"
  | VARIANT_PAYLOAD -> "[\"VARIANT_PAYLOAD\"]"
  | MATCH_FAIL loc -> Printf.sprintf "[\"MATCH_FAIL\",%s]" (json_escape_string loc)
  | PERFORM op -> Printf.sprintf "[\"PERFORM\",%s]" (json_escape_string op)
  | HANDLE n -> Printf.sprintf "[\"HANDLE\",%d]" n
  | RESUME -> "[\"RESUME\"]"
  | ENTER_LOOP n -> Printf.sprintf "[\"ENTER_LOOP\",%d]" n
  | EXIT_LOOP -> "[\"EXIT_LOOP\"]"
  | LOOP_BREAK -> "[\"LOOP_BREAK\"]"
  | LOOP_CONTINUE n -> Printf.sprintf "[\"LOOP_CONTINUE\",%d]" n
  | FOLD_CONTINUE n -> Printf.sprintf "[\"FOLD_CONTINUE\",%d]" n
  | MAKE_ARRAY n -> Printf.sprintf "[\"MAKE_ARRAY\",%d]" n
  | INDEX -> "[\"INDEX\"]"
  | HALT -> "[\"HALT\"]"
  | GET_LOCAL_CALL (slot, arity) -> Printf.sprintf "[\"GET_LOCAL_CALL\",%d,%d]" slot arity
  | GET_LOCAL_TUPLE_GET (slot, idx) -> Printf.sprintf "[\"GET_LOCAL_TUPLE_GET\",%d,%d]" slot idx
  | GET_LOCAL_FIELD (slot, name) -> Printf.sprintf "[\"GET_LOCAL_FIELD\",%d,%s]" slot (json_escape_string name)
  | GET_GLOBAL_CALL (idx, arity) -> Printf.sprintf "[\"GET_GLOBAL_CALL\",%d,%d]" idx arity
  | GET_GLOBAL_FIELD (idx, name) -> Printf.sprintf "[\"GET_GLOBAL_FIELD\",%d,%s]" idx (json_escape_string name)
  | JUMP_TABLE (min_tag, targets, default) ->
    Printf.sprintf "[\"JUMP_TABLE\",%d,[%s],%d]" min_tag
      (String.concat "," (Array.to_list (Array.map string_of_int targets))) default
  | CALL_N n -> Printf.sprintf "[\"CALL_N\",%d]" n
  | TAIL_CALL_N n -> Printf.sprintf "[\"TAIL_CALL_N\",%d]" n
  | UPDATE_REC -> "[\"UPDATE_REC\"]"

let rec serialize_value = function
  | Bytecode.VInt n -> Printf.sprintf "{\"t\":\"i\",\"v\":%d}" n
  | VFloat f ->
    let s = Printf.sprintf "%.17g" f in
    let len = String.length s in
    let s = if len > 0 && s.[len - 1] = '.' then String.sub s 0 (len - 1) else s in
    Printf.sprintf "{\"t\":\"f\",\"v\":%s}" s
  | VBool b -> Printf.sprintf "{\"t\":\"b\",\"v\":%b}" b
  | VString s -> Printf.sprintf "{\"t\":\"s\",\"v\":%s}" (json_escape_string s)
  | VByte n -> Printf.sprintf "{\"t\":\"y\",\"v\":%d}" n
  | VRune n -> Printf.sprintf "{\"t\":\"r\",\"v\":%d}" n
  | VUnit -> "{\"t\":\"u\"}"
  | VProto p -> Printf.sprintf "{\"t\":\"p\",\"v\":%s}" (serialize_prototype p)
  | VTuple vs ->
    Printf.sprintf "{\"t\":\"T\",\"v\":[%s]}"
      (String.concat "," (List.map serialize_value (Array.to_list vs)))
  | VList vs ->
    Printf.sprintf "{\"t\":\"L\",\"v\":[%s]}"
      (String.concat "," (List.map serialize_value vs))
  | VVariant (tag, name, payload) ->
    let payload_str = match payload with
      | None -> "null"
      | Some v -> serialize_value v
    in
    Printf.sprintf "{\"t\":\"V\",\"tag\":%d,\"name\":%s,\"payload\":%s}"
      tag (json_escape_string name) payload_str
  | _ -> failwith "serialize_value: unsupported value type in constants"

and serialize_prototype proto =
  let code_strs = Array.to_list (Array.map serialize_opcode proto.Bytecode.code) in
  let const_strs = Array.to_list (Array.map serialize_value proto.Bytecode.constants) in
  let line_strs = Array.to_list (Array.map string_of_int proto.Bytecode.line_table) in
  Printf.sprintf "{\"name\":%s,\"arity\":%d,\"num_locals\":%d,\"code\":[%s],\"constants\":[%s],\"line_table\":[%s]}"
    (json_escape_string proto.name)
    proto.arity
    proto.num_locals
    (String.concat "," code_strs)
    (String.concat "," const_strs)
    (String.concat "," line_strs)

let serialize_native_global idx value =
  match value with
  | Bytecode.VExternal ext ->
    Some (Printf.sprintf "%s:{\"type\":\"external\",\"name\":%s,\"arity\":%d}"
      (json_escape_string (string_of_int idx))
      (json_escape_string ext.ext_name)
      ext.ext_arity)
  | Bytecode.VRecord (shape, values) ->
    (* Check if it's a typeclass dictionary (all fields are VExternal) *)
    let all_external = Array.for_all (fun v ->
      match v with Bytecode.VExternal _ -> true | _ -> false
    ) values in
    if all_external && Array.length values > 0 then
      let field_strs = List.init (Array.length shape.rs_fields) (fun i ->
        let name = shape.rs_fields.(i) in
        match values.(i) with
        | Bytecode.VExternal ext ->
          Printf.sprintf "%s:{\"name\":%s,\"arity\":%d}"
            (json_escape_string name)
            (json_escape_string ext.ext_name)
            ext.ext_arity
        | _ -> assert false
      ) in
      Some (Printf.sprintf "%s:{\"type\":\"dict\",\"fields\":{%s}}"
        (json_escape_string (string_of_int idx))
        (String.concat "," field_strs))
    else None
  | _ -> None

let serialize_native_ext idx name arity =
  Printf.sprintf "%s:{\"type\":\"external\",\"name\":%s,\"arity\":%d}"
    (json_escape_string (string_of_int idx))
    (json_escape_string name)
    arity

let serialize_native_dict idx fields =
  let field_strs = List.map (fun (name, ext_name, arity) ->
    Printf.sprintf "%s:{\"name\":%s,\"arity\":%d}"
      (json_escape_string name)
      (json_escape_string ext_name)
      arity
  ) fields in
  Printf.sprintf "%s:{\"type\":\"dict\",\"fields\":{%s}}"
    (json_escape_string (string_of_int idx))
    (String.concat "," field_strs)

let build_native_globals_json globals =
  let parts = ref [] in
  Hashtbl.iter (fun idx value ->
    match serialize_native_global idx value with
    | Some s -> parts := s :: !parts
    | None -> ()
  ) globals;
  String.concat "," !parts

let serialize_bundle ~global_names ~native_globals_json ~setup_protos ~main_proto =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "{\"version\":1";
  (* global_names *)
  let names = Dynarray.to_array global_names in
  Buffer.add_string buf ",\"global_names\":[";
  Array.iteri (fun i name ->
    if i > 0 then Buffer.add_char buf ',';
    Buffer.add_string buf (json_escape_string name)
  ) names;
  Buffer.add_string buf "]";
  (* native_globals *)
  Buffer.add_string buf ",\"native_globals\":{";
  Buffer.add_string buf native_globals_json;
  Buffer.add_string buf "}";
  (* setup prototypes *)
  Buffer.add_string buf ",\"setup\":[";
  List.iteri (fun i proto ->
    if i > 0 then Buffer.add_char buf ',';
    Buffer.add_string buf (serialize_prototype proto)
  ) setup_protos;
  Buffer.add_string buf "]";
  (* main prototype *)
  Buffer.add_string buf ",\"main\":";
  Buffer.add_string buf (serialize_prototype main_proto);
  Buffer.add_string buf "}";
  Buffer.contents buf
