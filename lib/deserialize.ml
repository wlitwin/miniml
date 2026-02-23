(* JSON deserializer for bytecode bundles *)

type json =
  | JNull
  | JBool of bool
  | JInt of int
  | JFloat of float
  | JString of string
  | JArray of json list
  | JObject of (string * json) list

(* --- JSON tokenizer --- *)

type token =
  | TLBrace | TRBrace | TLBrack | TRBrack | TColon | TComma
  | TString of string | TNumber of string | TTrue | TFalse | TNull

let tokenize src =
  let len = String.length src in
  let i = ref 0 in
  let tokens = ref [] in
  while !i < len do
    let c = src.[!i] in
    match c with
    | ' ' | '\t' | '\n' | '\r' -> incr i
    | '{' -> tokens := TLBrace :: !tokens; incr i
    | '}' -> tokens := TRBrace :: !tokens; incr i
    | '[' -> tokens := TLBrack :: !tokens; incr i
    | ']' -> tokens := TRBrack :: !tokens; incr i
    | ':' -> tokens := TColon :: !tokens; incr i
    | ',' -> tokens := TComma :: !tokens; incr i
    | '"' ->
      let buf = Buffer.create 64 in
      incr i;
      while !i < len && src.[!i] <> '"' do
        if src.[!i] = '\\' then begin
          incr i;
          if !i < len then begin
            (match src.[!i] with
             | '"' -> Buffer.add_char buf '"'
             | '\\' -> Buffer.add_char buf '\\'
             | '/' -> Buffer.add_char buf '/'
             | 'n' -> Buffer.add_char buf '\n'
             | 'r' -> Buffer.add_char buf '\r'
             | 't' -> Buffer.add_char buf '\t'
             | 'u' ->
               let hex = String.sub src (!i + 1) 4 in
               let cp = int_of_string ("0x" ^ hex) in
               if cp < 0x80 then
                 Buffer.add_char buf (Char.chr cp)
               else if cp < 0x800 then begin
                 Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6)));
                 Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
               end else begin
                 Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12)));
                 Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
                 Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
               end;
               i := !i + 4
             | c -> Buffer.add_char buf c);
            incr i
          end
        end else begin
          Buffer.add_char buf src.[!i];
          incr i
        end
      done;
      if !i < len then incr i; (* skip closing quote *)
      tokens := TString (Buffer.contents buf) :: !tokens
    | 't' -> (* true *)
      tokens := TTrue :: !tokens; i := !i + 4
    | 'f' -> (* false *)
      tokens := TFalse :: !tokens; i := !i + 5
    | 'n' -> (* null *)
      tokens := TNull :: !tokens; i := !i + 4
    | '-' | '0'..'9' ->
      let start = !i in
      if src.[!i] = '-' then incr i;
      while !i < len && src.[!i] >= '0' && src.[!i] <= '9' do incr i done;
      let is_float = ref false in
      if !i < len && src.[!i] = '.' then begin
        is_float := true;
        incr i;
        while !i < len && src.[!i] >= '0' && src.[!i] <= '9' do incr i done
      end;
      if !i < len && (src.[!i] = 'e' || src.[!i] = 'E') then begin
        is_float := true;
        incr i;
        if !i < len && (src.[!i] = '+' || src.[!i] = '-') then incr i;
        while !i < len && src.[!i] >= '0' && src.[!i] <= '9' do incr i done
      end;
      ignore !is_float;
      tokens := TNumber (String.sub src start (!i - start)) :: !tokens
    | _ -> incr i (* skip unexpected chars *)
  done;
  List.rev !tokens

(* --- JSON parser --- *)

let parse_json src =
  let tokens = ref (tokenize src) in
  let peek () = match !tokens with t :: _ -> Some t | [] -> None in
  let advance () = match !tokens with _ :: rest -> tokens := rest | [] -> () in
  let expect_advance () =
    match !tokens with
    | t :: rest -> tokens := rest; t
    | [] -> failwith "unexpected end of JSON"
  in
  let rec parse_value () =
    match peek () with
    | Some TNull -> advance (); JNull
    | Some TTrue -> advance (); JBool true
    | Some TFalse -> advance (); JBool false
    | Some (TString s) -> advance (); JString s
    | Some (TNumber s) ->
      advance ();
      if String.contains s '.' || String.contains s 'e' || String.contains s 'E'
      then JFloat (float_of_string s)
      else JInt (int_of_string s)
    | Some TLBrack -> parse_array ()
    | Some TLBrace -> parse_object ()
    | Some _ -> failwith "unexpected token in JSON"
    | None -> failwith "unexpected end of JSON"
  and parse_array () =
    advance (); (* skip [ *)
    match peek () with
    | Some TRBrack -> advance (); JArray []
    | _ ->
      let items = ref [] in
      items := parse_value () :: !items;
      while (match peek () with Some TComma -> true | _ -> false) do
        advance (); (* skip , *)
        items := parse_value () :: !items
      done;
      (match expect_advance () with TRBrack -> () | _ -> failwith "expected ]");
      JArray (List.rev !items)
  and parse_object () =
    advance (); (* skip { *)
    match peek () with
    | Some TRBrace -> advance (); JObject []
    | _ ->
      let fields = ref [] in
      let parse_field () =
        let key = match expect_advance () with
          | TString s -> s
          | _ -> failwith "expected string key"
        in
        (match expect_advance () with TColon -> () | _ -> failwith "expected :");
        let value = parse_value () in
        fields := (key, value) :: !fields
      in
      parse_field ();
      while (match peek () with Some TComma -> true | _ -> false) do
        advance (); (* skip , *)
        parse_field ()
      done;
      (match expect_advance () with TRBrace -> () | _ -> failwith "expected }");
      JObject (List.rev !fields)
  in
  parse_value ()

(* --- JSON accessors --- *)

let json_field key = function
  | JObject fields -> (match List.assoc_opt key fields with Some v -> v | None -> JNull)
  | _ -> JNull

let json_string = function JString s -> s | _ -> ""
let json_int = function JInt n -> n | JFloat f -> int_of_float f | _ -> 0
let json_float = function JFloat f -> f | JInt n -> float_of_int n | _ -> 0.0
let json_bool = function JBool b -> b | _ -> false
let json_list = function JArray l -> l | _ -> []

(* --- Bytecode deserialization --- *)

let deserialize_capture json =
  match json with
  | JArray [JString "local"; JInt i] -> Bytecode.CaptureLocal i
  | JArray [JString "upvalue"; JInt i] -> Bytecode.CaptureUpvalue i
  | _ -> failwith "invalid capture"

let deserialize_opcode json =
  match json with
  | JArray (JString op :: args) ->
    (match op, args with
     | "CONST", [JInt i] -> Bytecode.CONST i
     | "POP", [] -> POP
     | "DUP", [] -> DUP
     | "GET_LOCAL", [JInt i] -> GET_LOCAL i
     | "SET_LOCAL", [JInt i] -> SET_LOCAL i
     | "GET_UPVALUE", [JInt i] -> GET_UPVALUE i
     | "SET_UPVALUE", [JInt i] -> SET_UPVALUE i
     | "MAKE_REF", [] -> MAKE_REF
     | "DEREF", [] -> DEREF
     | "SET_REF", [] -> SET_REF
     | "GET_GLOBAL", [JInt i] -> GET_GLOBAL i
     | "SET_GLOBAL", [JInt i] -> SET_GLOBAL i
     | "DEF_GLOBAL", [JInt i] -> DEF_GLOBAL i
     | "ADD", [] -> ADD
     | "SUB", [] -> SUB
     | "MUL", [] -> MUL
     | "DIV", [] -> DIV
     | "MOD", [] -> MOD
     | "NEG", [] -> NEG
     | "FADD", [] -> FADD
     | "FSUB", [] -> FSUB
     | "FMUL", [] -> FMUL
     | "FDIV", [] -> FDIV
     | "FNEG", [] -> FNEG
     | "EQ", [] -> EQ
     | "NEQ", [] -> NEQ
     | "LT", [] -> LT
     | "GT", [] -> GT
     | "LE", [] -> LE
     | "GE", [] -> GE
     | "NOT", [] -> NOT
     | "BAND", [] -> BAND
     | "BOR", [] -> BOR
     | "BXOR", [] -> BXOR
     | "BNOT", [] -> BNOT
     | "BSHL", [] -> BSHL
     | "BSHR", [] -> BSHR
     | "JUMP", [JInt off] -> JUMP off
     | "JUMP_IF_FALSE", [JInt off] -> JUMP_IF_FALSE off
     | "JUMP_IF_TRUE", [JInt off] -> JUMP_IF_TRUE off
     | "CLOSURE", [JInt i; JArray caps] ->
       CLOSURE (i, List.map deserialize_capture caps)
     | "CLOSURE_REC", [JInt i; JArray caps; JInt self] ->
       CLOSURE_REC (i, List.map deserialize_capture caps, self)
     | "CALL", [JInt n] -> CALL n
     | "TAIL_CALL", [JInt n] -> TAIL_CALL n
     | "RETURN", [] -> RETURN
     | "FUNC_RETURN", [] -> FUNC_RETURN
     | "ENTER_FUNC", [] -> ENTER_FUNC
     | "EXIT_FUNC", [] -> EXIT_FUNC
     | "MAKE_TUPLE", [JInt n] -> MAKE_TUPLE n
     | "TUPLE_GET", [JInt i] -> TUPLE_GET i
     | "MAKE_RECORD", [JArray fields] ->
       MAKE_RECORD (List.map json_string fields)
     | "FIELD", [JString name] -> FIELD name
     | "SET_FIELD", [JString name] -> SET_FIELD name
     | "RECORD_UPDATE", [JArray fields] ->
       RECORD_UPDATE (List.map json_string fields)
     | "RECORD_UPDATE_DYN", [JInt n] -> RECORD_UPDATE_DYN n
     | "MAKE_VARIANT", [JInt tag; JString name; JBool has_payload] ->
       MAKE_VARIANT (tag, name, has_payload)
     | "CONS", [] -> CONS
     | "NIL", [] -> NIL
     | "TAG_EQ", [JInt tag] -> TAG_EQ tag
     | "IS_NIL", [] -> IS_NIL
     | "IS_CONS", [] -> IS_CONS
     | "HEAD", [] -> HEAD
     | "TAIL", [] -> TAIL
     | "VARIANT_PAYLOAD", [] -> VARIANT_PAYLOAD
     | "MATCH_FAIL", [JString loc] -> MATCH_FAIL loc
     | "PERFORM", [JString op] -> PERFORM op
     | "HANDLE", [JInt n] -> HANDLE n
     | "RESUME", [] -> RESUME
     | "ENTER_LOOP", [JInt n] -> ENTER_LOOP n
     | "EXIT_LOOP", [] -> EXIT_LOOP
     | "LOOP_BREAK", [] -> LOOP_BREAK
     | "LOOP_CONTINUE", [JInt n] -> LOOP_CONTINUE n
     | "FOLD_CONTINUE", [JInt n] -> FOLD_CONTINUE n
     | "MAKE_MAP", [JInt n] -> MAKE_MAP n
     | "MAKE_ARRAY", [JInt n] -> MAKE_ARRAY n
     | "INDEX", [] -> INDEX
     | "HALT", [] -> HALT
     | "GET_LOCAL_CALL", [JInt slot; JInt arity] -> GET_LOCAL_CALL (slot, arity)
     | "GET_LOCAL_TUPLE_GET", [JInt slot; JInt idx] -> GET_LOCAL_TUPLE_GET (slot, idx)
     | "GET_LOCAL_FIELD", [JInt slot; JString name] -> GET_LOCAL_FIELD (slot, name)
     | "JUMP_TABLE", [JInt min_tag; JArray targets; JInt default] ->
       JUMP_TABLE (min_tag, Array.of_list (List.map json_int targets), default)
     | _ -> failwith (Printf.sprintf "unknown opcode: %s" op))
  | _ -> failwith "invalid opcode format"

let rec deserialize_value json =
  let t = json_string (json_field "t" json) in
  match t with
  | "i" -> Bytecode.VInt (json_int (json_field "v" json))
  | "f" -> Bytecode.VFloat (json_float (json_field "v" json))
  | "b" -> Bytecode.VBool (json_bool (json_field "v" json))
  | "s" -> Bytecode.VString (json_string (json_field "v" json))
  | "y" -> Bytecode.VByte (json_int (json_field "v" json))
  | "r" -> Bytecode.VRune (json_int (json_field "v" json))
  | "u" -> Bytecode.VUnit
  | "p" -> Bytecode.VProto (deserialize_prototype (json_field "v" json))
  | "T" -> Bytecode.VTuple (Array.of_list (List.map deserialize_value (json_list (json_field "v" json))))
  | "L" -> Bytecode.VList (List.map deserialize_value (json_list (json_field "v" json)))
  | "V" ->
    let tag = json_int (json_field "tag" json) in
    let name = json_string (json_field "name" json) in
    let payload = match json_field "payload" json with
      | JNull -> None
      | v -> Some (deserialize_value v)
    in
    Bytecode.VVariant (tag, name, payload)
  | _ -> failwith (Printf.sprintf "unknown value type: %s" t)

and deserialize_prototype json =
  let name = json_string (json_field "name" json) in
  let arity = json_int (json_field "arity" json) in
  let num_locals = json_int (json_field "num_locals" json) in
  let code = Array.of_list (List.map deserialize_opcode (json_list (json_field "code" json))) in
  let constants = Array.of_list (List.map deserialize_value (json_list (json_field "constants" json))) in
  let line_table = Array.of_list (List.map json_int (json_list (json_field "line_table" json))) in
  Bytecode.{ name; arity; num_locals; code; constants; line_table }

(* --- Builtin table type --- *)

type builtin_entry = {
  arity: int;
  impl: Bytecode.value list -> Bytecode.value;
}

type builtin_table = (string, builtin_entry) Hashtbl.t

(* --- Bundle loading --- *)

let run_prototype globals proto =
  let program = Bytecode.{
    main = proto;
    global_names = [||];
  } in
  Vm.execute_with_globals program globals

let load_bundle_parsed root (builtins : builtin_table) =
  (* Extract global_names *)
  let global_names = Array.of_list
    (List.map json_string (json_list (json_field "global_names" root))) in
  let num_globals = Array.length global_names in
  let globals : (int, Bytecode.value) Hashtbl.t = Hashtbl.create num_globals in
  (* Process native_globals *)
  let native_globals = json_field "native_globals" root in
  (match native_globals with
   | JObject fields ->
     List.iter (fun (idx_str, entry) ->
       let idx = int_of_string idx_str in
       let ty = json_string (json_field "type" entry) in
       match ty with
       | "external" ->
         let name = json_string (json_field "name" entry) in
         let arity = json_int (json_field "arity" entry) in
         (match Hashtbl.find_opt builtins name with
          | Some b ->
            Hashtbl.replace globals idx (Bytecode.VExternal {
              ext_name = name;
              ext_arity = arity;
              ext_fn = b.impl;
              ext_args = [];
            })
          | None ->
            (* Not found — register a stub that errors *)
            Hashtbl.replace globals idx (Bytecode.VExternal {
              ext_name = name;
              ext_arity = arity;
              ext_fn = (fun _ -> raise (Vm.Runtime_error
                (Printf.sprintf "unregistered builtin: %s" name)));
              ext_args = [];
            }))
       | "dict" ->
         let fields_json = json_field "fields" entry in
         let field_list = match fields_json with
           | JObject fl -> fl
           | _ -> []
         in
         let methods = List.map (fun (field_name, field_entry) ->
           let name = json_string (json_field "name" field_entry) in
           let arity = json_int (json_field "arity" field_entry) in
           let impl = match Hashtbl.find_opt builtins name with
             | Some b -> b.impl
             | None -> (fun _ -> raise (Vm.Runtime_error
                 (Printf.sprintf "unregistered builtin: %s" name)))
           in
           (field_name, Bytecode.VExternal {
             ext_name = name;
             ext_arity = arity;
             ext_fn = impl;
             ext_args = [];
           })
         ) field_list in
         let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) methods in
         let field_names = List.map fst sorted in
         let values = Array.of_list (List.map snd sorted) in
         Hashtbl.replace globals idx (Bytecode.make_record field_names values)
       | _ -> ()
     ) fields
   | _ -> ());
  (* Run setup prototypes *)
  let setup_protos = json_list (json_field "setup" root) in
  List.iter (fun proto_json ->
    let proto = deserialize_prototype proto_json in
    let _ = run_prototype globals proto in
    ()
  ) setup_protos;
  (* Run main prototype *)
  let main_json = json_field "main" root in
  let main_proto = deserialize_prototype main_json in
  run_prototype globals main_proto

let load_bundle json_str builtins =
  let root = parse_json json_str in
  load_bundle_parsed root builtins

(* --- Prepared bundle for repeated execution --- *)

type prepared_bundle = {
  prepared_globals : (int, Bytecode.value) Hashtbl.t;
  prepared_main : Bytecode.prototype;
}

let prepare_bundle root (builtins : builtin_table) =
  (* Same as load_bundle_parsed but stops after setup, before running main *)
  let global_names = Array.of_list
    (List.map json_string (json_list (json_field "global_names" root))) in
  let num_globals = Array.length global_names in
  let globals : (int, Bytecode.value) Hashtbl.t = Hashtbl.create num_globals in
  let native_globals = json_field "native_globals" root in
  (match native_globals with
   | JObject fields ->
     List.iter (fun (idx_str, entry) ->
       let idx = int_of_string idx_str in
       let ty = json_string (json_field "type" entry) in
       match ty with
       | "external" ->
         let name = json_string (json_field "name" entry) in
         let arity = json_int (json_field "arity" entry) in
         (match Hashtbl.find_opt builtins name with
          | Some b ->
            Hashtbl.replace globals idx (Bytecode.VExternal {
              ext_name = name;
              ext_arity = arity;
              ext_fn = b.impl;
              ext_args = [];
            })
          | None ->
            Hashtbl.replace globals idx (Bytecode.VExternal {
              ext_name = name;
              ext_arity = arity;
              ext_fn = (fun _ -> raise (Vm.Runtime_error
                (Printf.sprintf "unregistered builtin: %s" name)));
              ext_args = [];
            }))
       | "dict" ->
         let fields_json = json_field "fields" entry in
         let field_list = match fields_json with
           | JObject fl -> fl
           | _ -> []
         in
         let methods = List.map (fun (field_name, field_entry) ->
           let name = json_string (json_field "name" field_entry) in
           let arity = json_int (json_field "arity" field_entry) in
           let impl = match Hashtbl.find_opt builtins name with
             | Some b -> b.impl
             | None -> (fun _ -> raise (Vm.Runtime_error
                 (Printf.sprintf "unregistered builtin: %s" name)))
           in
           (field_name, Bytecode.VExternal {
             ext_name = name;
             ext_arity = arity;
             ext_fn = impl;
             ext_args = [];
           })
         ) field_list in
         let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) methods in
         let field_names = List.map fst sorted in
         let values = Array.of_list (List.map snd sorted) in
         Hashtbl.replace globals idx (Bytecode.make_record field_names values)
       | _ -> ()
     ) fields
   | _ -> ());
  let setup_protos = json_list (json_field "setup" root) in
  List.iter (fun proto_json ->
    let proto = deserialize_prototype proto_json in
    let _ = run_prototype globals proto in
    ()
  ) setup_protos;
  let main_json = json_field "main" root in
  let main_proto = deserialize_prototype main_json in
  { prepared_globals = globals; prepared_main = main_proto }

let run_prepared (pb : prepared_bundle) =
  (* Clone the globals so the prepared state is not mutated *)
  let globals = Hashtbl.copy pb.prepared_globals in
  run_prototype globals pb.prepared_main
