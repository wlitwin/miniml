exception Runtime_error of string

let error msg = raise (Runtime_error msg)

type handler_entry = {
  he_return: Bytecode.value;
  he_ops: (string * Bytecode.value) list;
  he_body_fiber: Bytecode.fiber;
  he_parent_fiber: Bytecode.fiber;
}

type control_entry = {
  ce_break_ip: int;
  ce_fiber: Bytecode.fiber;
  ce_frame_depth: int;
  ce_stack_depth: int;
}

type return_entry = {
  ret_fiber: Bytecode.fiber;
  ret_frame_depth: int;
}

type t = {
  mutable current_fiber: Bytecode.fiber;
  mutable handler_stack: handler_entry list;
  mutable control_stack: control_entry list;
  mutable return_stack: return_entry list;
  globals: (int, Bytecode.value) Hashtbl.t;
  global_names: string array;
}

let fiber_stack_size = 65536

let make_fiber () : Bytecode.fiber = {
  fiber_stack = Array.make fiber_stack_size Bytecode.VUnit;
  fiber_sp = 0;
  fiber_frames = [];
}

let push vm v =
  let f = vm.current_fiber in
  if f.fiber_sp >= fiber_stack_size then error "stack overflow";
  f.fiber_stack.(f.fiber_sp) <- v;
  f.fiber_sp <- f.fiber_sp + 1

let pop vm =
  let f = vm.current_fiber in
  if f.fiber_sp <= 0 then error "stack underflow";
  f.fiber_sp <- f.fiber_sp - 1;
  f.fiber_stack.(f.fiber_sp)

let peek vm =
  let f = vm.current_fiber in
  if f.fiber_sp <= 0 then error "stack underflow";
  f.fiber_stack.(f.fiber_sp - 1)

let frame vm = List.hd vm.current_fiber.fiber_frames
let proto vm = (frame vm).Bytecode.frame_closure.Bytecode.fn_proto

let read_op vm =
  let f = frame vm in
  let op = (proto vm).code.(f.frame_ip) in
  f.frame_ip <- f.frame_ip + 1;
  op

let as_int = function
  | Bytecode.VInt n -> n
  | v -> error (Printf.sprintf "expected int, got %s" (Bytecode.pp_value v))

let as_float = function
  | Bytecode.VFloat f -> f
  | v -> error (Printf.sprintf "expected float, got %s" (Bytecode.pp_value v))

let as_bool = function
  | Bytecode.VBool b -> b
  | v -> error (Printf.sprintf "expected bool, got %s" (Bytecode.pp_value v))

let as_string_vm vm = function
  | Bytecode.VString s -> s
  | v ->
    let trace = Buffer.create 256 in
    List.iteri (fun i fr ->
      let fip = fr.Bytecode.frame_ip in
      let fname = fr.frame_closure.fn_proto.name in
      let line = if fip > 0 && fip - 1 < Array.length fr.frame_closure.fn_proto.line_table
        then fr.frame_closure.fn_proto.line_table.(fip - 1) else 0 in
      Buffer.add_string trace (Printf.sprintf "\n  frame[%d]: %s ip=%d line=%d" i fname fip line)
    ) vm.current_fiber.fiber_frames;
    error (Printf.sprintf "expected string, got %s\nStack:%s" (Bytecode.pp_value v) (Buffer.contents trace))
let as_string = function
  | Bytecode.VString s -> s
  | v -> error (Printf.sprintf "expected string, got %s" (Bytecode.pp_value v))

let as_closure = function
  | Bytecode.VClosure c -> c
  | v -> error (Printf.sprintf "expected function, got %s" (Bytecode.pp_value v))

let as_tuple = function
  | Bytecode.VTuple t -> t
  | v -> error (Printf.sprintf "expected tuple, got %s" (Bytecode.pp_value v))

let as_record_vm vm = function
  | Bytecode.VRecord (shape, values) -> (shape, values)
  | v ->
    let trace = Buffer.create 256 in
    List.iteri (fun i fr ->
      let fip = fr.Bytecode.frame_ip in
      let fname = fr.frame_closure.fn_proto.name in
      let line = if fip > 0 && fip - 1 < Array.length fr.frame_closure.fn_proto.line_table
        then fr.frame_closure.fn_proto.line_table.(fip - 1) else 0 in
      Buffer.add_string trace (Printf.sprintf "\n  frame[%d]: %s ip=%d line=%d" i fname fip line)
    ) vm.current_fiber.fiber_frames;
    error (Printf.sprintf "expected record, got %s\nStack:%s" (Bytecode.pp_value v) (Buffer.contents trace))
let as_record = function
  | Bytecode.VRecord (shape, values) -> (shape, values)
  | v -> error (Printf.sprintf "expected record, got %s" (Bytecode.pp_value v))

let as_list = function
  | Bytecode.VList l -> l
  | v -> error (Printf.sprintf "expected list, got %s" (Bytecode.pp_value v))

let as_variant = function
  | Bytecode.VVariant (tag, name, payload) -> (tag, name, payload)
  | v -> error (Printf.sprintf "expected variant, got %s" (Bytecode.pp_value v))

let as_continuation = function
  | Bytecode.VContinuation cd -> cd
  | v -> error (Printf.sprintf "expected continuation, got %s" (Bytecode.pp_value v))

let values_equal a b =
  let rec eq a b =
    match a, b with
    | Bytecode.VInt a, Bytecode.VInt b -> a = b
    | Bytecode.VFloat a, Bytecode.VFloat b -> Float.equal a b
    | Bytecode.VBool a, Bytecode.VBool b -> a = b
    | Bytecode.VString a, Bytecode.VString b -> String.equal a b
    | Bytecode.VByte a, Bytecode.VByte b -> a = b
    | Bytecode.VRune a, Bytecode.VRune b -> a = b
    | Bytecode.VUnit, Bytecode.VUnit -> true
    | Bytecode.VTuple a, Bytecode.VTuple b ->
      Array.length a = Array.length b &&
      Array.for_all2 eq a b
    | Bytecode.VList a, Bytecode.VList b ->
      List.length a = List.length b &&
      List.for_all2 eq a b
    | Bytecode.VVariant (t1, _, p1), Bytecode.VVariant (t2, _, p2) ->
      t1 = t2 && (match p1, p2 with
        | None, None -> true
        | Some a, Some b -> eq a b
        | _ -> false)
    | Bytecode.VRecord (sa, va), Bytecode.VRecord (sb, vb) ->
      Array.length va = Array.length vb &&
      sa.rs_fields = sb.rs_fields &&
      Array.for_all2 eq va vb
    | Bytecode.VArray a, Bytecode.VArray b ->
      Array.length a = Array.length b &&
      Array.for_all2 eq a b
    | _ -> false
  in
  eq a b

let resolve_capture vm (cap : Bytecode.capture) : Bytecode.value =
  let f = frame vm in
  match cap with
  | Bytecode.CaptureLocal slot -> f.frame_locals.(slot)
  | Bytecode.CaptureUpvalue idx -> f.frame_closure.upvalues.(idx)

(* Set up a call frame for a closure on the current fiber *)
let internal_call vm (cls : Bytecode.closure) arg =
  let new_locals = Array.make cls.fn_proto.num_locals Bytecode.VUnit in
  new_locals.(0) <- arg;
  let new_frame = Bytecode.{
    frame_closure = cls;
    frame_ip = 0;
    frame_locals = new_locals;
    frame_base_sp = vm.current_fiber.fiber_sp;
  } in
  vm.current_fiber.fiber_frames <- new_frame :: vm.current_fiber.fiber_frames

(* Find a handler entry whose body fiber matches the given fiber *)
let find_handler_for_fiber vm fiber =
  List.find_opt (fun he -> he.he_body_fiber == fiber) vm.handler_stack

let remove_handler vm he =
  vm.handler_stack <- List.filter (fun h -> h != he) vm.handler_stack

let run vm =
  let rec loop () =
    let op = read_op vm in
    match op with
    | Bytecode.CONST i ->
      push vm (proto vm).constants.(i);
      loop ()
    | Bytecode.POP ->
      ignore (pop vm);
      loop ()
    | Bytecode.DUP ->
      push vm (peek vm);
      loop ()
    | Bytecode.GET_LOCAL i ->
      push vm (frame vm).frame_locals.(i);
      loop ()
    | Bytecode.SET_LOCAL i ->
      (frame vm).frame_locals.(i) <- pop vm;
      loop ()
    | Bytecode.GET_UPVALUE i ->
      push vm (frame vm).frame_closure.upvalues.(i);
      loop ()
    | Bytecode.SET_UPVALUE i ->
      (frame vm).frame_closure.upvalues.(i) <- pop vm;
      loop ()
    | Bytecode.MAKE_REF ->
      let v = pop vm in
      push vm (Bytecode.VRef (ref v));
      loop ()
    | Bytecode.DEREF ->
      (match pop vm with
       | Bytecode.VRef r -> push vm !r; loop ()
       | _ -> error "DEREF on non-ref value")
    | Bytecode.SET_REF ->
      (* stack: [... ref new_value] -> pop new_value, pop ref, set ref *)
      let r = pop vm in
      let v = pop vm in
      (match r with
       | Bytecode.VRef cell -> cell := v; loop ()
       | _ -> error "SET_REF on non-ref value")
    | Bytecode.GET_GLOBAL i ->
      (match Hashtbl.find_opt vm.globals i with
       | Some v ->
         push vm v; loop ()
       | None ->
         let name = if i < Array.length vm.global_names then vm.global_names.(i) else "?" in
         error (Printf.sprintf "undefined global: %s" name))
    | Bytecode.SET_GLOBAL i ->
      Hashtbl.replace vm.globals i (pop vm);
      loop ()
    | Bytecode.DEF_GLOBAL i ->
      let v = pop vm in
      Hashtbl.replace vm.globals i v;
      loop ()
    | Bytecode.ADD ->
      let b = pop vm in let a = pop vm in
      push vm (Bytecode.VInt (as_int a + as_int b));
      loop ()
    | Bytecode.SUB ->
      let b = pop vm in let a = pop vm in
      push vm (Bytecode.VInt (as_int a - as_int b));
      loop ()
    | Bytecode.MUL ->
      let b = pop vm in let a = pop vm in
      push vm (Bytecode.VInt (as_int a * as_int b));
      loop ()
    | Bytecode.DIV ->
      let b = pop vm in let a = pop vm in
      let bv = as_int b in
      if bv = 0 then error "division by zero";
      push vm (Bytecode.VInt (as_int a / bv));
      loop ()
    | Bytecode.MOD ->
      let b = pop vm in let a = pop vm in
      let bv = as_int b in
      if bv = 0 then error "modulo by zero";
      push vm (Bytecode.VInt (as_int a mod bv));
      loop ()
    | Bytecode.NEG ->
      let v = pop vm in
      push vm (Bytecode.VInt (- (as_int v)));
      loop ()
    | Bytecode.FADD ->
      let b = pop vm in let a = pop vm in
      push vm (Bytecode.VFloat (as_float a +. as_float b));
      loop ()
    | Bytecode.FSUB ->
      let b = pop vm in let a = pop vm in
      push vm (Bytecode.VFloat (as_float a -. as_float b));
      loop ()
    | Bytecode.FMUL ->
      let b = pop vm in let a = pop vm in
      push vm (Bytecode.VFloat (as_float a *. as_float b));
      loop ()
    | Bytecode.FDIV ->
      let b = pop vm in let a = pop vm in
      push vm (Bytecode.VFloat (as_float a /. as_float b));
      loop ()
    | Bytecode.FNEG ->
      let v = pop vm in
      push vm (Bytecode.VFloat (-. (as_float v)));
      loop ()
    | Bytecode.EQ ->
      let b = pop vm in let a = pop vm in
      push vm (Bytecode.VBool (values_equal a b));
      loop ()
    | Bytecode.NEQ ->
      let b = pop vm in let a = pop vm in
      push vm (Bytecode.VBool (not (values_equal a b)));
      loop ()
    | Bytecode.LT ->
      let b = pop vm in let a = pop vm in
      let result = match a, b with
        | Bytecode.VFloat fa, Bytecode.VFloat fb -> fa < fb
        | Bytecode.VString sa, Bytecode.VString sb -> String.compare sa sb < 0
        | Bytecode.VByte a, Bytecode.VByte b -> a < b
        | Bytecode.VRune a, Bytecode.VRune b -> a < b
        | _ -> as_int a < as_int b
      in
      push vm (Bytecode.VBool result);
      loop ()
    | Bytecode.GT ->
      let b = pop vm in let a = pop vm in
      let result = match a, b with
        | Bytecode.VFloat fa, Bytecode.VFloat fb -> fa > fb
        | Bytecode.VString sa, Bytecode.VString sb -> String.compare sa sb > 0
        | Bytecode.VByte a, Bytecode.VByte b -> a > b
        | Bytecode.VRune a, Bytecode.VRune b -> a > b
        | _ -> as_int a > as_int b
      in
      push vm (Bytecode.VBool result);
      loop ()
    | Bytecode.LE ->
      let b = pop vm in let a = pop vm in
      let result = match a, b with
        | Bytecode.VFloat fa, Bytecode.VFloat fb -> fa <= fb
        | Bytecode.VString sa, Bytecode.VString sb -> String.compare sa sb <= 0
        | Bytecode.VByte a, Bytecode.VByte b -> a <= b
        | Bytecode.VRune a, Bytecode.VRune b -> a <= b
        | _ -> as_int a <= as_int b
      in
      push vm (Bytecode.VBool result);
      loop ()
    | Bytecode.GE ->
      let b = pop vm in let a = pop vm in
      let result = match a, b with
        | Bytecode.VFloat fa, Bytecode.VFloat fb -> fa >= fb
        | Bytecode.VString sa, Bytecode.VString sb -> String.compare sa sb >= 0
        | Bytecode.VByte a, Bytecode.VByte b -> a >= b
        | Bytecode.VRune a, Bytecode.VRune b -> a >= b
        | _ -> as_int a >= as_int b
      in
      push vm (Bytecode.VBool result);
      loop ()
    | Bytecode.NOT ->
      let v = pop vm in
      push vm (Bytecode.VBool (not (as_bool v)));
      loop ()
    | Bytecode.BAND ->
      let b = pop vm in let a = pop vm in
      push vm (Bytecode.VInt (as_int a land as_int b));
      loop ()
    | Bytecode.BOR ->
      let b = pop vm in let a = pop vm in
      push vm (Bytecode.VInt (as_int a lor as_int b));
      loop ()
    | Bytecode.BXOR ->
      let b = pop vm in let a = pop vm in
      push vm (Bytecode.VInt (as_int a lxor as_int b));
      loop ()
    | Bytecode.BNOT ->
      let v = pop vm in
      push vm (Bytecode.VInt (lnot (as_int v)));
      loop ()
    | Bytecode.BSHL ->
      let b = pop vm in let a = pop vm in
      push vm (Bytecode.VInt (as_int a lsl as_int b));
      loop ()
    | Bytecode.BSHR ->
      let b = pop vm in let a = pop vm in
      push vm (Bytecode.VInt (as_int a lsr as_int b));
      loop ()
    | Bytecode.JUMP target ->
      (frame vm).frame_ip <- target;
      loop ()
    | Bytecode.JUMP_IF_FALSE target ->
      let v = pop vm in
      if not (as_bool v) then (frame vm).frame_ip <- target;
      loop ()
    | Bytecode.JUMP_IF_TRUE target ->
      let v = pop vm in
      if as_bool v then (frame vm).frame_ip <- target;
      loop ()
    | Bytecode.CLOSURE (proto_idx, captures) ->
      let fn_proto = get_proto vm proto_idx in
      let upvalues = Array.of_list (List.map (resolve_capture vm) captures) in
      push vm (Bytecode.VClosure { fn_proto; upvalues });
      loop ()
    | Bytecode.CLOSURE_REC (proto_idx, captures, self_idx) ->
      let fn_proto = get_proto vm proto_idx in
      let upvalues = Array.of_list (List.map (resolve_capture vm) captures) in
      let cls = Bytecode.{ fn_proto; upvalues } in
      cls.upvalues.(self_idx) <- Bytecode.VClosure cls;
      push vm (Bytecode.VClosure cls);
      loop ()
    | Bytecode.CALL _ ->
      let arg = pop vm in
      let fn_val = pop vm in
      (match fn_val with
       | Bytecode.VClosure cls ->
         let new_locals = Array.make cls.fn_proto.num_locals Bytecode.VUnit in
         new_locals.(0) <- arg;
         let new_frame = Bytecode.{
           frame_closure = cls;
           frame_ip = 0;
           frame_locals = new_locals;
           frame_base_sp = vm.current_fiber.fiber_sp;
         } in
         vm.current_fiber.fiber_frames <- new_frame :: vm.current_fiber.fiber_frames;
         loop ()
       | Bytecode.VExternal ext ->
         let new_args = ext.ext_args @ [arg] in
         if List.length new_args = ext.ext_arity then begin
           let result = ext.ext_fn new_args in
           push vm result;
           loop ()
         end else begin
           push vm (Bytecode.VExternal { ext with ext_args = new_args });
           loop ()
         end
       | _ -> error (Printf.sprintf "expected function, got %s" (Bytecode.pp_value fn_val)))
    | Bytecode.TAIL_CALL _ ->
      let arg = pop vm in
      let fn_val = pop vm in
      (match fn_val with
       | Bytecode.VClosure cls ->
         (* Replace current frame with new one — same stack depth *)
         let base_sp = (frame vm).Bytecode.frame_base_sp in
         let new_locals = Array.make cls.fn_proto.num_locals Bytecode.VUnit in
         new_locals.(0) <- arg;
         let new_frame = Bytecode.{
           frame_closure = cls;
           frame_ip = 0;
           frame_locals = new_locals;
           frame_base_sp = base_sp;
         } in
         vm.current_fiber.fiber_frames <- new_frame :: List.tl vm.current_fiber.fiber_frames;
         loop ()
       | Bytecode.VExternal ext ->
         (* For externals, do a proper tail return *)
         let new_args = ext.ext_args @ [arg] in
         let result =
           if List.length new_args = ext.ext_arity then
             ext.ext_fn new_args
           else
             Bytecode.VExternal { ext with ext_args = new_args }
         in
         vm.current_fiber.fiber_sp <- (frame vm).Bytecode.frame_base_sp;
         vm.current_fiber.fiber_frames <- List.tl vm.current_fiber.fiber_frames;
         if vm.current_fiber.fiber_frames = [] then begin
           match find_handler_for_fiber vm vm.current_fiber with
           | Some he ->
             remove_handler vm he;
             vm.current_fiber <- he.he_parent_fiber;
             internal_call vm (as_closure he.he_return) result;
             loop ()
           | None ->
             result
         end else begin
           push vm result;
           loop ()
         end
       | _ -> error (Printf.sprintf "expected function, got %s" (Bytecode.pp_value fn_val)))
    | Bytecode.RETURN ->
      let result = pop vm in
      vm.current_fiber.fiber_frames <- List.tl vm.current_fiber.fiber_frames;
      if vm.current_fiber.fiber_frames = [] then begin
        (* Last frame on fiber — check for handler *)
        match find_handler_for_fiber vm vm.current_fiber with
        | Some he ->
          remove_handler vm he;
          vm.current_fiber <- he.he_parent_fiber;
          internal_call vm (as_closure he.he_return) result;
          loop ()
        | None ->
          (* Main fiber done *)
          result
      end else begin
        push vm result;
        loop ()
      end
    | Bytecode.MAKE_TUPLE n ->
      let values = Array.init n (fun _ -> Bytecode.VUnit) in
      for i = n - 1 downto 0 do
        values.(i) <- pop vm
      done;
      push vm (Bytecode.VTuple values);
      loop ()
    | Bytecode.TUPLE_GET i ->
      let tup = pop vm in
      let arr = as_tuple tup in
      if i >= Array.length arr then error "tuple index out of bounds";
      push vm arr.(i);
      loop ()
    | Bytecode.MAKE_RECORD field_names ->
      let n = List.length field_names in
      let values = Array.init n (fun _ -> Bytecode.VUnit) in
      for i = n - 1 downto 0 do
        values.(i) <- pop vm
      done;
      let shape = Bytecode.make_record_shape field_names in
      push vm (Bytecode.VRecord (shape, values));
      loop ()
    | Bytecode.FIELD name ->
      let record = pop vm in
      (match record with
       | Bytecode.VRecord (shape, values) ->
         (match Hashtbl.find_opt shape.rs_index name with
          | Some idx -> push vm values.(idx); loop ()
          | None -> error (Printf.sprintf "record has no field: %s" name))
       | _ ->
         let trace = Buffer.create 256 in
         List.iteri (fun i fr ->
           let fip = fr.Bytecode.frame_ip in
           let fname = fr.frame_closure.fn_proto.name in
           let line = if fip > 0 && fip - 1 < Array.length fr.frame_closure.fn_proto.line_table
             then fr.frame_closure.fn_proto.line_table.(fip - 1) else 0 in
           Buffer.add_string trace (Printf.sprintf "\n  frame[%d]: %s ip=%d line=%d" i fname fip line)
         ) vm.current_fiber.fiber_frames;
         error (Printf.sprintf "FIELD '%s': expected record, got %s\nStack:%s" name (Bytecode.pp_value record) (Buffer.contents trace)))
    | Bytecode.SET_FIELD name ->
      let new_val = pop vm in
      let record = pop vm in
      let (shape, values) = as_record_vm vm record in
      (match Hashtbl.find_opt shape.rs_index name with
       | Some idx -> values.(idx) <- new_val; loop ()
       | None -> error (Printf.sprintf "record has no field: %s" name))
    | Bytecode.RECORD_UPDATE field_names ->
      let n = List.length field_names in
      let new_vals = Array.init n (fun _ -> Bytecode.VUnit) in
      for i = n - 1 downto 0 do
        new_vals.(i) <- pop vm
      done;
      let (shape, base_values) = as_record_vm vm (pop vm) in
      let copy = Array.copy base_values in
      List.iteri (fun i name ->
        match Hashtbl.find_opt shape.rs_index name with
        | Some idx -> copy.(idx) <- new_vals.(i)
        | None -> error (Printf.sprintf "record has no field: %s" name)
      ) field_names;
      push vm (Bytecode.VRecord (shape, copy));
      loop ()
    | Bytecode.RECORD_UPDATE_DYN n ->
      (* Stack: base, idx1, val1, ..., idxN, valN *)
      let pairs = Array.init n (fun _ -> (0, Bytecode.VUnit)) in
      for i = n - 1 downto 0 do
        let v = pop vm in
        let idx = match pop vm with
          | Bytecode.VInt i -> i
          | _ -> error "RECORD_UPDATE_DYN: expected int index"
        in
        pairs.(i) <- (idx, v)
      done;
      let (shape, base_values) = as_record_vm vm (pop vm) in
      let copy = Array.copy base_values in
      Array.iter (fun (idx, v) ->
        if idx >= 0 && idx < Array.length copy then
          copy.(idx) <- v
        else
          error (Printf.sprintf "RECORD_UPDATE_DYN: index %d out of bounds (record has %d fields)" idx (Array.length copy))
      ) pairs;
      push vm (Bytecode.VRecord (shape, copy));
      loop ()
    | Bytecode.MAKE_VARIANT (tag, ctor_name, has_payload) ->
      if has_payload then begin
        let payload = pop vm in
        push vm (Bytecode.VVariant (tag, ctor_name, Some payload))
      end else
        push vm (Bytecode.VVariant (tag, ctor_name, None));
      loop ()
    | Bytecode.CONS ->
      let tl = pop vm in
      let hd = pop vm in
      let tl_list = as_list tl in
      push vm (Bytecode.VList (hd :: tl_list));
      loop ()
    | Bytecode.NIL ->
      push vm (Bytecode.VList []);
      loop ()
    | Bytecode.TAG_EQ tag ->
      let v = pop vm in
      let (vtag, _, _) = as_variant v in
      push vm (Bytecode.VBool (vtag = tag));
      loop ()
    | Bytecode.IS_NIL ->
      let v = pop vm in
      let l = as_list v in
      push vm (Bytecode.VBool (l = []));
      loop ()
    | Bytecode.IS_CONS ->
      let v = pop vm in
      let l = as_list v in
      push vm (Bytecode.VBool (l <> []));
      loop ()
    | Bytecode.HEAD ->
      let v = pop vm in
      let l = as_list v in
      (match l with
       | hd :: _ -> push vm hd; loop ()
       | [] -> error "head of empty list")
    | Bytecode.TAIL ->
      let v = pop vm in
      let l = as_list v in
      (match l with
       | _ :: tl -> push vm (Bytecode.VList tl); loop ()
       | [] -> error "tail of empty list")
    | Bytecode.VARIANT_PAYLOAD ->
      let v = pop vm in
      let (_, _, payload) = as_variant v in
      (match payload with
       | Some p -> push vm p; loop ()
       | None -> error "variant has no payload")
    | Bytecode.MATCH_FAIL loc ->
      error (Printf.sprintf "non-exhaustive match at %s" loc)
    | Bytecode.PERFORM op_name ->
      let arg = pop vm in
      (* Find handler for this operation *)
      let he = match List.find_opt (fun he ->
        List.exists (fun (name, _) -> String.equal name op_name) he.he_ops
      ) vm.handler_stack with
        | Some he -> he
        | None -> error (Printf.sprintf "unhandled effect operation: %s" op_name)
      in
      let handler_fn = List.assoc op_name he.he_ops in
      (* Create continuation from current fiber *)
      let cont = Bytecode.{
        cd_fiber = vm.current_fiber;
        cd_return_handler = he.he_return;
        cd_op_handlers = he.he_ops;
        cd_used = false;
      } in
      (* Build (arg, k) tuple *)
      let pair = Bytecode.VTuple [| arg; VContinuation cont |] in
      (* Remove handler entry *)
      remove_handler vm he;
      (* Switch to parent fiber and call handler *)
      vm.current_fiber <- he.he_parent_fiber;
      internal_call vm (as_closure handler_fn) pair;
      loop ()
    | Bytecode.HANDLE n_ops ->
      (* Stack layout: body_thunk, return_handler, (handler1, name1), (handler2, name2), ... *)
      (* Pop op handlers in reverse order *)
      let ops = List.init n_ops (fun _ ->
        let handler_closure = pop vm in
        let op_name = as_string (pop vm) in
        (op_name, handler_closure)
      ) in
      let return_handler = pop vm in
      let body_thunk = pop vm in
      (* Create body fiber *)
      let body_fiber = make_fiber () in
      (* Install handler *)
      let he = {
        he_return = return_handler;
        he_ops = ops;
        he_body_fiber = body_fiber;
        he_parent_fiber = vm.current_fiber;
      } in
      vm.handler_stack <- he :: vm.handler_stack;
      (* Switch to body fiber and call thunk *)
      vm.current_fiber <- body_fiber;
      internal_call vm (as_closure body_thunk) Bytecode.VUnit;
      loop ()
    | Bytecode.RESUME ->
      let v = pop vm in
      let cont = as_continuation (pop vm) in
      if cont.cd_used then error "continuation already resumed";
      cont.cd_used <- true;
      (* Push value onto body fiber's stack (result of perform) *)
      let body_fiber = cont.cd_fiber in
      body_fiber.fiber_stack.(body_fiber.fiber_sp) <- v;
      body_fiber.fiber_sp <- body_fiber.fiber_sp + 1;
      (* Reinstall handler for deep handling *)
      let he = {
        he_return = cont.cd_return_handler;
        he_ops = cont.cd_op_handlers;
        he_body_fiber = body_fiber;
        he_parent_fiber = vm.current_fiber;
      } in
      vm.handler_stack <- he :: vm.handler_stack;
      (* Switch to body fiber *)
      vm.current_fiber <- body_fiber;
      loop ()
    | Bytecode.ENTER_LOOP break_target ->
      let fiber = vm.current_fiber in
      let ce = {
        ce_break_ip = break_target;
        ce_fiber = fiber;
        ce_frame_depth = List.length fiber.fiber_frames;
        ce_stack_depth = fiber.fiber_sp;
      } in
      vm.control_stack <- ce :: vm.control_stack;
      loop ()
    | Bytecode.EXIT_LOOP ->
      (match vm.control_stack with
       | _ :: rest -> vm.control_stack <- rest; loop ()
       | [] -> error "EXIT_LOOP: no control entry")
    | Bytecode.LOOP_BREAK ->
      let break_value = pop vm in
      (match vm.control_stack with
       | ce :: rest ->
         vm.control_stack <- rest;
         let fiber = ce.ce_fiber in
         (* Unwind frames until we reach the loop entry depth *)
         while List.length fiber.fiber_frames > ce.ce_frame_depth do
           fiber.fiber_frames <- List.tl fiber.fiber_frames
         done;
         (* Restore stack pointer and push break value *)
         fiber.fiber_sp <- ce.ce_stack_depth;
         vm.current_fiber <- fiber;
         push vm break_value;
         (* Jump to break target *)
         let f = frame vm in
         f.frame_ip <- ce.ce_break_ip;
         loop ()
       | [] -> error "LOOP_BREAK: no control entry")
    | Bytecode.ENTER_FUNC ->
      let fiber = vm.current_fiber in
      let re = {
        ret_fiber = fiber;
        ret_frame_depth = List.length fiber.fiber_frames;
      } in
      vm.return_stack <- re :: vm.return_stack;
      loop ()
    | Bytecode.EXIT_FUNC ->
      (match vm.return_stack with
       | _ :: rest -> vm.return_stack <- rest; loop ()
       | [] -> error "EXIT_FUNC: no return entry")
    | Bytecode.FUNC_RETURN ->
      let result = pop vm in
      let fiber = vm.current_fiber in
      (* Find and remove the return entry for this fiber *)
      let rec find_and_remove = function
        | [] -> error "FUNC_RETURN: no return entry"
        | re :: rest when re.ret_fiber == fiber ->
          vm.return_stack <- rest;
          re
        | re :: rest ->
          let entry = find_and_remove rest in
          vm.return_stack <- re :: vm.return_stack;
          entry
      in
      let re = find_and_remove vm.return_stack in
      let target_depth = re.ret_frame_depth in
      (* Unwind frames back to the target function *)
      while List.length fiber.fiber_frames > target_depth do
        fiber.fiber_frames <- List.tl fiber.fiber_frames
      done;
      (* Clean up control_stack entries above target depth *)
      vm.control_stack <- List.filter (fun ce ->
        not (ce.ce_fiber == fiber && ce.ce_frame_depth >= target_depth)
      ) vm.control_stack;
      (* Restore stack pointer and pop the target function's frame *)
      let base_sp = (frame vm).Bytecode.frame_base_sp in
      fiber.fiber_sp <- base_sp;
      fiber.fiber_frames <- List.tl fiber.fiber_frames;
      if fiber.fiber_frames = [] then begin
        match find_handler_for_fiber vm fiber with
        | Some he ->
          remove_handler vm he;
          vm.current_fiber <- he.he_parent_fiber;
          internal_call vm (as_closure he.he_return) result;
          loop ()
        | None ->
          result
      end else begin
        push vm result;
        loop ()
      end
    | Bytecode.LOOP_CONTINUE target ->
      (match vm.control_stack with
       | ce :: _ ->
         let fiber = ce.ce_fiber in
         fiber.fiber_sp <- ce.ce_stack_depth;
         let f = frame vm in
         f.frame_ip <- target;
         loop ()
       | [] -> error "LOOP_CONTINUE: no control entry")
    | Bytecode.FOLD_CONTINUE n_pops ->
      let continue_value = pop vm in
      let fiber = vm.current_fiber in
      for _ = 1 to n_pops do
        if fiber.fiber_sp > 0 then
          fiber.fiber_sp <- fiber.fiber_sp - 1
      done;
      push vm continue_value;
      (* Execute RETURN — return from fold callback *)
      let result = pop vm in
      (match vm.current_fiber.fiber_frames with
       | _ :: rest ->
         vm.current_fiber.fiber_frames <- rest;
         push vm result;
         loop ()
       | [] -> error "FOLD_CONTINUE: no frame to return from")
    | Bytecode.MAKE_MAP n ->
      let pairs = ref [] in
      for _ = 1 to n do
        let v = pop vm in
        let k = pop vm in
        pairs := (k, v) :: !pairs
      done;
      push vm (Bytecode.VMap !pairs);
      loop ()
    | Bytecode.MAKE_ARRAY n ->
      let elems = ref [] in
      for _ = 1 to n do
        elems := pop vm :: !elems
      done;
      push vm (Bytecode.VArray (Array.of_list !elems));
      loop ()
    | Bytecode.INDEX ->
      let idx = as_int (pop vm) in
      let base = pop vm in
      (match base with
       | Bytecode.VString s ->
         if idx < 0 || idx >= String.length s then
           error (Printf.sprintf "string index out of bounds: %d (length %d)" idx (String.length s));
         push vm (Bytecode.VByte (Char.code s.[idx]));
         loop ()
       | Bytecode.VArray a ->
         if idx < 0 || idx >= Array.length a then
           error (Printf.sprintf "array index out of bounds: %d (length %d)" idx (Array.length a));
         push vm a.(idx);
         loop ()
       | _ -> error "index operation requires string or array")
    | Bytecode.GET_LOCAL_CALL (slot, _arity) ->
      let fn_val = pop vm in
      let arg = (frame vm).frame_locals.(slot) in
      (match fn_val with
       | Bytecode.VClosure cls ->
         let new_locals = Array.make cls.fn_proto.num_locals Bytecode.VUnit in
         new_locals.(0) <- arg;
         let new_frame = Bytecode.{
           frame_closure = cls;
           frame_ip = 0;
           frame_locals = new_locals;
           frame_base_sp = vm.current_fiber.fiber_sp;
         } in
         vm.current_fiber.fiber_frames <- new_frame :: vm.current_fiber.fiber_frames;
         loop ()
       | Bytecode.VExternal ext ->
         let new_args = ext.ext_args @ [arg] in
         if List.length new_args = ext.ext_arity then begin
           let result = ext.ext_fn new_args in
           push vm result;
           loop ()
         end else begin
           push vm (Bytecode.VExternal { ext with ext_args = new_args });
           loop ()
         end
       | _ -> error (Printf.sprintf "expected function, got %s" (Bytecode.pp_value fn_val)))
    | Bytecode.GET_LOCAL_TUPLE_GET (slot, idx) ->
      let tup = (frame vm).frame_locals.(slot) in
      let arr = as_tuple tup in
      if idx >= Array.length arr then error "tuple index out of bounds";
      push vm arr.(idx);
      loop ()
    | Bytecode.GET_LOCAL_FIELD (slot, name) ->
      let record = (frame vm).frame_locals.(slot) in
      (match record with
       | Bytecode.VRecord (shape, values) ->
         (match Hashtbl.find_opt shape.rs_index name with
          | Some idx -> push vm values.(idx); loop ()
          | None -> error (Printf.sprintf "record has no field: %s" name))
       | _ -> error (Printf.sprintf "GET_LOCAL_FIELD '%s': expected record, got %s" name (Bytecode.pp_value record)))
    | Bytecode.JUMP_TABLE (min_tag, targets, default_target) ->
      let v = pop vm in
      let (vtag, _, _) = as_variant v in
      let idx = vtag - min_tag in
      if idx >= 0 && idx < Array.length targets then
        (frame vm).frame_ip <- targets.(idx)
      else
        (frame vm).frame_ip <- default_target;
      loop ()
    | Bytecode.HALT ->
      if vm.current_fiber.fiber_sp > 0 then peek vm
      else Bytecode.VUnit

  and get_proto vm proto_idx =
    let constants = (proto vm).constants in
    match constants.(proto_idx) with
    | Bytecode.VProto p -> p
    | _ -> error (Printf.sprintf "expected prototype at constant %d" proto_idx)

  in
  let lookup_line () =
    try
      let f = frame vm in
      let p = f.frame_closure.fn_proto in
      let ip = max 0 (f.frame_ip - 1) in
      if ip < Array.length p.line_table then p.line_table.(ip) else 0
    with _ -> 0
  in
  (try loop () with
   | Runtime_error msg ->
     let line = lookup_line () in
     let trace = Buffer.create 256 in
     List.iteri (fun i fr ->
       let fip = fr.Bytecode.frame_ip in
       let fname = fr.frame_closure.fn_proto.name in
       let fline = if fip > 0 && fip - 1 < Array.length fr.frame_closure.fn_proto.line_table
         then fr.frame_closure.fn_proto.line_table.(fip - 1) else 0 in
       Buffer.add_string trace (Printf.sprintf "\n  frame[%d]: %s ip=%d line=%d" i fname fip fline)
     ) vm.current_fiber.fiber_frames;
     let full_msg = if line > 0 then
       Printf.sprintf "[line %d] %s\nCall stack:%s" line msg (Buffer.contents trace)
     else Printf.sprintf "%s\nCall stack:%s" msg (Buffer.contents trace) in
     raise (Runtime_error full_msg))

let execute program =
  let main_fiber = make_fiber () in
  let main_closure = Bytecode.{
    fn_proto = program.Bytecode.main;
    upvalues = [||];
  } in
  let main_frame = Bytecode.{
    frame_closure = main_closure;
    frame_ip = 0;
    frame_locals = Array.make program.main.num_locals Bytecode.VUnit;
    frame_base_sp = 0;
  } in
  main_fiber.fiber_frames <- [main_frame];
  let vm = {
    current_fiber = main_fiber;
    handler_stack = [];
    control_stack = [];
    return_stack = [];
    globals = Hashtbl.create 64;
    global_names = program.global_names;
  } in
  run vm

let execute_with_globals program (globals : (int, Bytecode.value) Hashtbl.t) =
  let main_fiber = make_fiber () in
  let main_closure = Bytecode.{
    fn_proto = program.Bytecode.main;
    upvalues = [||];
  } in
  let main_frame = Bytecode.{
    frame_closure = main_closure;
    frame_ip = 0;
    frame_locals = Array.make program.main.num_locals Bytecode.VUnit;
    frame_base_sp = 0;
  } in
  main_fiber.fiber_frames <- [main_frame];
  let vm = {
    current_fiber = main_fiber;
    handler_stack = [];
    control_stack = [];
    return_stack = [];
    globals;
    global_names = program.global_names;
  } in
  run vm
