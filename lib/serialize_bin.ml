(* Binary serialization for bytecode bundles *)
(* Wire format: little-endian, "MMLB" magic + u32 version, then sections *)

(* --- String table builder --- *)

type string_table = {
  tbl: (string, int) Hashtbl.t;
  mutable count: int;
}

let strtab_create () = { tbl = Hashtbl.create 256; count = 0 }

let strtab_intern st s =
  match Hashtbl.find_opt st.tbl s with
  | Some idx -> idx
  | None ->
    let idx = st.count in
    Hashtbl.replace st.tbl s idx;
    st.count <- st.count + 1;
    idx

let strtab_to_array st =
  let arr = Array.make st.count "" in
  Hashtbl.iter (fun s idx -> arr.(idx) <- s) st.tbl;
  arr

(* --- Write helpers --- *)

let write_u8 buf n =
  Buffer.add_char buf (Char.chr (n land 0xFF))

let write_u32 buf n =
  Buffer.add_char buf (Char.chr (n land 0xFF));
  Buffer.add_char buf (Char.chr ((n lsr 8) land 0xFF));
  Buffer.add_char buf (Char.chr ((n lsr 16) land 0xFF));
  Buffer.add_char buf (Char.chr ((n lsr 24) land 0xFF))

let write_i64 buf n =
  let n64 = Int64.of_int n in
  for i = 0 to 7 do
    Buffer.add_char buf (Char.chr (Int64.to_int
      (Int64.logand (Int64.shift_right_logical n64 (i * 8)) 0xFFL)))
  done

let write_f64 buf f =
  let bits = Int64.bits_of_float f in
  for i = 0 to 7 do
    Buffer.add_char buf (Char.chr (Int64.to_int
      (Int64.logand (Int64.shift_right_logical bits (i * 8)) 0xFFL)))
  done

(* --- Collect strings pass --- *)

let collect_opcode_strings st = function
  | Bytecode.MAKE_RECORD fields | Bytecode.RECORD_UPDATE fields ->
    List.iter (fun f -> ignore (strtab_intern st f)) fields
  | Bytecode.RECORD_UPDATE_DYN _ -> ()
  | Bytecode.FIELD name | Bytecode.SET_FIELD name | Bytecode.PERFORM name
  | Bytecode.MATCH_FAIL name ->
    ignore (strtab_intern st name)
  | Bytecode.MAKE_VARIANT (_, name, _) -> ignore (strtab_intern st name)
  | Bytecode.GET_LOCAL_FIELD (_, name) -> ignore (strtab_intern st name)
  | _ -> ()

let rec collect_value_strings st = function
  | Bytecode.VString s -> ignore (strtab_intern st s)
  | Bytecode.VProto p -> collect_proto_strings st p
  | Bytecode.VTuple vs -> Array.iter (collect_value_strings st) vs
  | Bytecode.VList vs -> List.iter (collect_value_strings st) vs
  | Bytecode.VVariant (_, name, payload) ->
    ignore (strtab_intern st name);
    (match payload with Some v -> collect_value_strings st v | None -> ())
  | _ -> ()

and collect_proto_strings st proto =
  ignore (strtab_intern st proto.Bytecode.name);
  Array.iter (collect_opcode_strings st) proto.Bytecode.code;
  Array.iter (collect_value_strings st) proto.Bytecode.constants

let collect_native_strings st globals =
  Hashtbl.iter (fun _idx value ->
    match value with
    | Bytecode.VExternal ext -> ignore (strtab_intern st ext.ext_name)
    | Bytecode.VRecord (shape, values) ->
      let all_external = Array.for_all (fun v ->
        match v with Bytecode.VExternal _ -> true | _ -> false
      ) values in
      if all_external && Array.length values > 0 then
        Array.iteri (fun i v ->
          ignore (strtab_intern st shape.rs_fields.(i));
          match v with
          | Bytecode.VExternal ext -> ignore (strtab_intern st ext.ext_name)
          | _ -> ()
        ) values
    | _ -> ()
  ) globals

(* --- Write sections --- *)

let write_string_table buf st =
  let arr = strtab_to_array st in
  write_u32 buf (Array.length arr);
  Array.iter (fun s ->
    write_u32 buf (String.length s);
    Buffer.add_string buf s
  ) arr

let write_captures buf caps =
  write_u32 buf (List.length caps);
  List.iter (fun cap ->
    match cap with
    | Bytecode.CaptureLocal i -> write_u8 buf 0; write_u32 buf i
    | Bytecode.CaptureUpvalue i -> write_u8 buf 1; write_u32 buf i
  ) caps

let write_opcode buf st = function
  | Bytecode.CONST i -> write_u8 buf 0; write_u32 buf i
  | Bytecode.POP -> write_u8 buf 1
  | Bytecode.DUP -> write_u8 buf 2
  | Bytecode.GET_LOCAL i -> write_u8 buf 3; write_u32 buf i
  | Bytecode.SET_LOCAL i -> write_u8 buf 4; write_u32 buf i
  | Bytecode.GET_UPVALUE i -> write_u8 buf 5; write_u32 buf i
  | Bytecode.SET_UPVALUE i -> write_u8 buf 6; write_u32 buf i
  | Bytecode.MAKE_REF -> write_u8 buf 7
  | Bytecode.DEREF -> write_u8 buf 8
  | Bytecode.SET_REF -> write_u8 buf 9
  | Bytecode.GET_GLOBAL i -> write_u8 buf 10; write_u32 buf i
  | Bytecode.SET_GLOBAL i -> write_u8 buf 11; write_u32 buf i
  | Bytecode.DEF_GLOBAL i -> write_u8 buf 12; write_u32 buf i
  | Bytecode.ADD -> write_u8 buf 13
  | Bytecode.SUB -> write_u8 buf 14
  | Bytecode.MUL -> write_u8 buf 15
  | Bytecode.DIV -> write_u8 buf 16
  | Bytecode.MOD -> write_u8 buf 17
  | Bytecode.NEG -> write_u8 buf 18
  | Bytecode.FADD -> write_u8 buf 19
  | Bytecode.FSUB -> write_u8 buf 20
  | Bytecode.FMUL -> write_u8 buf 21
  | Bytecode.FDIV -> write_u8 buf 22
  | Bytecode.FNEG -> write_u8 buf 23
  | Bytecode.EQ -> write_u8 buf 24
  | Bytecode.NEQ -> write_u8 buf 25
  | Bytecode.LT -> write_u8 buf 26
  | Bytecode.GT -> write_u8 buf 27
  | Bytecode.LE -> write_u8 buf 28
  | Bytecode.GE -> write_u8 buf 29
  | Bytecode.NOT -> write_u8 buf 30
  | Bytecode.BAND -> write_u8 buf 31
  | Bytecode.BOR -> write_u8 buf 32
  | Bytecode.BXOR -> write_u8 buf 33
  | Bytecode.BNOT -> write_u8 buf 34
  | Bytecode.BSHL -> write_u8 buf 35
  | Bytecode.BSHR -> write_u8 buf 36
  | Bytecode.JUMP off -> write_u8 buf 37; write_u32 buf off
  | Bytecode.JUMP_IF_FALSE off -> write_u8 buf 38; write_u32 buf off
  | Bytecode.JUMP_IF_TRUE off -> write_u8 buf 39; write_u32 buf off
  | Bytecode.CLOSURE (i, caps) ->
    write_u8 buf 40; write_u32 buf i; write_captures buf caps
  | Bytecode.CLOSURE_REC (i, caps, self) ->
    write_u8 buf 41; write_u32 buf i; write_captures buf caps; write_u32 buf self
  | Bytecode.CALL n -> write_u8 buf 42; write_u32 buf n
  | Bytecode.TAIL_CALL n -> write_u8 buf 43; write_u32 buf n
  | Bytecode.RETURN -> write_u8 buf 44
  | Bytecode.FUNC_RETURN -> write_u8 buf 45
  | Bytecode.ENTER_FUNC -> write_u8 buf 46
  | Bytecode.EXIT_FUNC -> write_u8 buf 47
  | Bytecode.MAKE_TUPLE n -> write_u8 buf 48; write_u32 buf n
  | Bytecode.TUPLE_GET i -> write_u8 buf 49; write_u32 buf i
  | Bytecode.MAKE_RECORD fields ->
    write_u8 buf 50;
    write_u32 buf (List.length fields);
    List.iter (fun f -> write_u32 buf (strtab_intern st f)) fields
  | Bytecode.FIELD name ->
    write_u8 buf 51; write_u32 buf (strtab_intern st name)
  | Bytecode.SET_FIELD name ->
    write_u8 buf 52; write_u32 buf (strtab_intern st name)
  | Bytecode.MAKE_VARIANT (tag, name, has_payload) ->
    write_u8 buf 53;
    write_u32 buf tag;
    write_u32 buf (strtab_intern st name);
    write_u8 buf (if has_payload then 1 else 0)
  | Bytecode.CONS -> write_u8 buf 54
  | Bytecode.NIL -> write_u8 buf 55
  | Bytecode.TAG_EQ tag -> write_u8 buf 56; write_u32 buf tag
  | Bytecode.IS_NIL -> write_u8 buf 57
  | Bytecode.IS_CONS -> write_u8 buf 58
  | Bytecode.HEAD -> write_u8 buf 59
  | Bytecode.TAIL -> write_u8 buf 60
  | Bytecode.VARIANT_PAYLOAD -> write_u8 buf 61
  | Bytecode.MATCH_FAIL loc -> write_u8 buf 62; write_u32 buf (strtab_intern st loc)
  | Bytecode.PERFORM op ->
    write_u8 buf 63; write_u32 buf (strtab_intern st op)
  | Bytecode.HANDLE n -> write_u8 buf 64; write_u32 buf n
  | Bytecode.RESUME -> write_u8 buf 65
  | Bytecode.ENTER_LOOP n -> write_u8 buf 66; write_u32 buf n
  | Bytecode.EXIT_LOOP -> write_u8 buf 67
  | Bytecode.LOOP_BREAK -> write_u8 buf 68
  | Bytecode.LOOP_CONTINUE n -> write_u8 buf 69; write_u32 buf n
  | Bytecode.FOLD_CONTINUE n -> write_u8 buf 70; write_u32 buf n
  | Bytecode.MAKE_ARRAY n -> write_u8 buf 72; write_u32 buf n
  | Bytecode.INDEX -> write_u8 buf 73
  | Bytecode.HALT -> write_u8 buf 74
  | Bytecode.RECORD_UPDATE fields ->
    write_u8 buf 75;
    write_u32 buf (List.length fields);
    List.iter (fun f -> write_u32 buf (strtab_intern st f)) fields
  | Bytecode.RECORD_UPDATE_DYN n ->
    write_u8 buf 76; write_u32 buf n
  | Bytecode.GET_LOCAL_CALL (slot, arity) ->
    write_u8 buf 77; write_u32 buf slot; write_u32 buf arity
  | Bytecode.GET_LOCAL_TUPLE_GET (slot, idx) ->
    write_u8 buf 78; write_u32 buf slot; write_u32 buf idx
  | Bytecode.GET_LOCAL_FIELD (slot, name) ->
    write_u8 buf 79; write_u32 buf slot; write_u32 buf (strtab_intern st name)
  | Bytecode.GET_GLOBAL_CALL (idx, arity) ->
    write_u8 buf 81; write_u32 buf idx; write_u32 buf arity
  | Bytecode.GET_GLOBAL_FIELD (idx, name) ->
    write_u8 buf 82; write_u32 buf idx; write_u32 buf (strtab_intern st name)
  | Bytecode.JUMP_TABLE (min_tag, targets, default) ->
    write_u8 buf 80;
    write_u32 buf min_tag;
    write_u32 buf (Array.length targets);
    Array.iter (fun t -> write_u32 buf t) targets;
    write_u32 buf default
  | Bytecode.CALL_N n -> write_u8 buf 83; write_u32 buf n
  | Bytecode.TAIL_CALL_N n -> write_u8 buf 84; write_u32 buf n
  | Bytecode.UPDATE_REC -> write_u8 buf 85

let rec write_value buf st = function
  | Bytecode.VInt n -> write_u8 buf 0; write_i64 buf n
  | Bytecode.VFloat f -> write_u8 buf 1; write_f64 buf f
  | Bytecode.VBool b -> write_u8 buf 2; write_u8 buf (if b then 1 else 0)
  | Bytecode.VString s -> write_u8 buf 3; write_u32 buf (strtab_intern st s)
  | Bytecode.VByte n -> write_u8 buf 4; write_u8 buf n
  | Bytecode.VRune n -> write_u8 buf 5; write_u32 buf n
  | Bytecode.VUnit -> write_u8 buf 6
  | Bytecode.VProto p -> write_u8 buf 7; write_prototype buf st p
  | Bytecode.VTuple vs ->
    write_u8 buf 8;
    write_u32 buf (Array.length vs);
    Array.iter (write_value buf st) vs
  | Bytecode.VList vs ->
    write_u8 buf 9;
    write_u32 buf (List.length vs);
    List.iter (write_value buf st) vs
  | Bytecode.VVariant (tag, name, payload) ->
    write_u8 buf 10;
    write_u32 buf tag;
    write_u32 buf (strtab_intern st name);
    (match payload with
     | None -> write_u8 buf 0
     | Some v -> write_u8 buf 1; write_value buf st v)
  | _ -> failwith "serialize_value: unsupported value type in constants"

and write_prototype buf st proto =
  write_u32 buf (strtab_intern st proto.Bytecode.name);
  write_u32 buf proto.Bytecode.arity;
  write_u32 buf proto.Bytecode.num_locals;
  write_u32 buf (Array.length proto.Bytecode.code);
  Array.iter (write_opcode buf st) proto.Bytecode.code;
  write_u32 buf (Array.length proto.Bytecode.constants);
  Array.iter (write_value buf st) proto.Bytecode.constants;
  write_u32 buf (Array.length proto.Bytecode.line_table);
  Array.iter (fun n -> write_u32 buf n) proto.Bytecode.line_table

let write_native_globals buf st globals =
  let entries = ref [] in
  Hashtbl.iter (fun idx value ->
    match value with
    | Bytecode.VExternal ext ->
      entries := (idx, `External (ext.ext_name, ext.ext_arity)) :: !entries
    | Bytecode.VRecord (shape, values) ->
      let all_external = Array.for_all (fun v ->
        match v with Bytecode.VExternal _ -> true | _ -> false
      ) values in
      if all_external && Array.length values > 0 then begin
        let fs = List.init (Array.length shape.rs_fields) (fun i ->
          match values.(i) with
          | Bytecode.VExternal ext -> (shape.rs_fields.(i), ext.ext_name, ext.ext_arity)
          | _ -> assert false
        ) in
        entries := (idx, `Dict fs) :: !entries
      end
    | _ -> ()
  ) globals;
  let entries = !entries in
  write_u32 buf (List.length entries);
  List.iter (fun (idx, entry) ->
    write_u32 buf idx;
    match entry with
    | `External (name, arity) ->
      write_u8 buf 0;
      write_u32 buf (strtab_intern st name);
      write_u32 buf arity
    | `Dict fields ->
      write_u8 buf 1;
      write_u32 buf (List.length fields);
      List.iter (fun (field_name, ext_name, ext_arity) ->
        write_u32 buf (strtab_intern st field_name);
        write_u32 buf (strtab_intern st ext_name);
        write_u32 buf ext_arity
      ) fields
  ) entries

(* --- JSON to binary conversion --- *)

let convert_json_to_binary json_str =
  let open Deserialize in
  let root = parse_json json_str in
  let st = strtab_create () in
  (* Extract global names *)
  let global_name_list = List.map json_string (json_list (json_field "global_names" root)) in
  let global_names_arr = Array.of_list global_name_list in
  (* Extract and deserialize prototypes *)
  let setup_json = json_list (json_field "setup" root) in
  let setup_protos = List.map deserialize_prototype setup_json in
  let main_proto = deserialize_prototype (json_field "main" root) in
  (* Extract native globals as raw data *)
  let native_entries = ref [] in
  (match json_field "native_globals" root with
   | JObject fields ->
     List.iter (fun (idx_str, entry) ->
       let idx = int_of_string idx_str in
       let ty = json_string (json_field "type" entry) in
       match ty with
       | "external" ->
         let name = json_string (json_field "name" entry) in
         let arity = json_int (json_field "arity" entry) in
         native_entries := (idx, `External (name, arity)) :: !native_entries
       | "dict" ->
         let field_list = match json_field "fields" entry with
           | JObject fl -> fl | _ -> [] in
         let fs = List.map (fun (field_name, field_entry) ->
           let name = json_string (json_field "name" field_entry) in
           let arity = json_int (json_field "arity" field_entry) in
           (field_name, name, arity)
         ) field_list in
         native_entries := (idx, `Dict fs) :: !native_entries
       | _ -> ()
     ) fields
   | _ -> ());
  let native_entries = !native_entries in
  (* Pass 1: collect all strings *)
  Array.iter (fun name -> ignore (strtab_intern st name)) global_names_arr;
  List.iter (fun (_, entry) ->
    match entry with
    | `External (name, _) -> ignore (strtab_intern st name)
    | `Dict fs -> List.iter (fun (fn, en, _) ->
        ignore (strtab_intern st fn); ignore (strtab_intern st en)) fs
  ) native_entries;
  List.iter (collect_proto_strings st) setup_protos;
  collect_proto_strings st main_proto;
  (* Pass 2: write binary *)
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "MMLB";
  write_u32 buf 1;
  write_string_table buf st;
  (* Global names *)
  write_u32 buf (Array.length global_names_arr);
  Array.iter (fun name -> write_u32 buf (strtab_intern st name)) global_names_arr;
  (* Native globals *)
  write_u32 buf (List.length native_entries);
  List.iter (fun (idx, entry) ->
    write_u32 buf idx;
    match entry with
    | `External (name, arity) ->
      write_u8 buf 0;
      write_u32 buf (strtab_intern st name);
      write_u32 buf arity
    | `Dict fields ->
      write_u8 buf 1;
      write_u32 buf (List.length fields);
      List.iter (fun (field_name, ext_name, ext_arity) ->
        write_u32 buf (strtab_intern st field_name);
        write_u32 buf (strtab_intern st ext_name);
        write_u32 buf ext_arity
      ) fields
  ) native_entries;
  (* Setup protos *)
  write_u32 buf (List.length setup_protos);
  List.iter (write_prototype buf st) setup_protos;
  (* Main proto *)
  write_prototype buf st main_proto;
  Buffer.contents buf

(* --- Main entry point --- *)

let serialize_bundle ~global_names ~globals ~setup_protos ~main_proto =
  let st = strtab_create () in
  (* Pass 1: collect all strings *)
  let names = Dynarray.to_array global_names in
  Array.iter (fun name -> ignore (strtab_intern st name)) names;
  collect_native_strings st globals;
  List.iter (collect_proto_strings st) setup_protos;
  collect_proto_strings st main_proto;
  (* Pass 2: write binary *)
  let buf = Buffer.create 4096 in
  (* Header *)
  Buffer.add_string buf "MMLB";
  write_u32 buf 1;
  (* String table *)
  write_string_table buf st;
  (* Global names *)
  write_u32 buf (Array.length names);
  Array.iter (fun name -> write_u32 buf (strtab_intern st name)) names;
  (* Native globals *)
  write_native_globals buf st globals;
  (* Setup protos *)
  write_u32 buf (List.length setup_protos);
  List.iter (write_prototype buf st) setup_protos;
  (* Main proto *)
  write_prototype buf st main_proto;
  Buffer.contents buf
