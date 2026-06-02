type capture = CaptureLocal of int | CaptureUpvalue of int

type opcode =
  | CONST of int
  | POP
  | DUP
  | GET_LOCAL of int
  | SET_LOCAL of int
  | GET_UPVALUE of int
  | MAKE_REF
  | DEREF
  | SET_REF
  | GET_GLOBAL of int
  | DEF_GLOBAL of int
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | NEG
  | FADD
  | FSUB
  | FMUL
  | FDIV
  | FNEG
  | EQ
  | NEQ
  | LT
  | GT
  | LE
  | GE
  | NOT
  | BAND
  | BOR
  | BXOR
  | BNOT
  | BSHL
  | BSHR
  | JUMP of int
  | JUMP_IF_FALSE of int
  | JUMP_IF_TRUE of int
  | CLOSURE of int * capture list
  | CLOSURE_REC of int * capture list * int
  | CALL of int
  | TAIL_CALL of int
  | RETURN
  | FUNC_RETURN
  | ENTER_FUNC
  | EXIT_FUNC
  | MAKE_TUPLE of int
  | TUPLE_GET of int
  | MAKE_RECORD of string list
  | FIELD of string
  | SET_FIELD of string
  | RECORD_UPDATE of string list
  | RECORD_UPDATE_DYN of
      int (* n fields; stack: base, idx1, val1, ..., idxN, valN *)
  | MAKE_VARIANT of int * string * bool
  | CONS
  | NIL
  | TAG_EQ of int
  | IS_NIL
  | IS_CONS
  | HEAD
  | TAIL
  | VARIANT_PAYLOAD
  | MATCH_FAIL of string
  | PERFORM of string
  | HANDLE of int
  | RESUME
  | ENTER_LOOP of int
  | EXIT_LOOP
  | LOOP_BREAK
  | LOOP_CONTINUE of int
  | FOLD_CONTINUE of int
  | MAKE_ARRAY of int
  | INDEX
  | HALT
  (* Superinstructions *)
  | GET_LOCAL_CALL of int * int
  | GET_LOCAL_TUPLE_GET of int * int
  | GET_LOCAL_FIELD of int * string
  | GET_GLOBAL_CALL of int * int
  | GET_GLOBAL_FIELD of int * string
  (* Jump table for match expressions *)
  | JUMP_TABLE of int * int array * int
  (* Multi-arity call *)
  | CALL_N of int
  | TAIL_CALL_N of int
  | UPDATE_REC
  (* No-fiber lowering of non-resumptive (THOpTry) handlers. TRY_BEGIN installs a
     lightweight handler marker on the current fiber carrying an op-name -> catch_ip
     table; the handled body runs inline (no thunk, no fiber). TRY_END pops the
     marker on normal completion. A matching PERFORM unwinds to the marker and jumps
     to the op's catch block. Appended at the end so existing opcode tags are stable. *)
  | TRY_BEGIN of (string * int) list
  | TRY_END
  (* No-fiber lowering of tail-resumptive (THOpProvide) handlers. PROVIDE n installs
     a lightweight handler marker carrying [n] op-name -> arm-closure pairs (each arm
     is [fun arg -> value], the resume value with the trailing [resume k] stripped);
     the handled body runs inline (no thunk, no fiber). A matching PERFORM calls the
     arm on the current fiber and continues the body with its result (tail-resumption
     is fiber-agnostic: we resume exactly once, immediately, so no continuation is
     reified). PROVIDE_END pops the marker on normal completion. Appended at the end
     so existing opcode tags are stable. *)
  | PROVIDE of int
  | PROVIDE_END

type record_shape = {
  rs_fields : string array;
  rs_index : (string, int) Hashtbl.t;
}

type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VString of string
  | VByte of int
  | VRune of int
  | VUnit
  | VTuple of value array
  | VList of value list
  | VRecord of record_shape * value array
  | VVariant of int * string * value option
  | VClosure of closure
  | VPartial of closure * value list
  | VProto of prototype
  | VExternal of external_fn
  | VContinuation of continuation_data
  | VRef of value ref
  | VArray of value array

and closure = { fn_proto : prototype; upvalues : value array }

and prototype = {
  name : string;
  arity : int;
  num_locals : int;
  code : opcode array;
  constants : value array;
  line_table : int array;
}

and external_fn = {
  ext_name : string;
  ext_arity : int;
  ext_fn : value list -> value;
  ext_args : value list;
}

and call_frame = {
  frame_closure : closure;
  mutable frame_ip : int;
  frame_base_sp : int;
}

and fiber = {
  mutable fiber_stack : value array;  (* grows on demand; see Vm.ensure_fiber_capacity *)
  mutable fiber_sp : int;
  mutable fiber_frames : call_frame list;
  mutable fiber_frame_depth : int;
  mutable fiber_extra_args : value list list;
  (* Loop-control and function-return markers are PER-FIBER control state:
     they are captured with the fiber by continuations and duplicated by
     copy_continuation, so multishot resume re-enters loops/functions with
     intact markers (the spec: control state is copied across resumes, the
     heap is shared). A VM-global stack breaks exactly that — the first
     resume consumes the marker and the second finds it gone. *)
  mutable fiber_control : control_entry list;
  mutable fiber_return : return_entry list;
}

(* A handler installed on the handler stack. [HFull] is the general fiber-based
   handler (multi-shot / tail-resumptive): the body runs on its own [hf_body_fiber]
   and PERFORM builds a continuation. [HTry] is the no-fiber lowering of a
   non-resumptive (THOpTry) handler: the body runs inline on [ht_fiber] (the fiber
   current when TRY_BEGIN ran) and a matching PERFORM unwinds to [ht_frame_depth]/
   [ht_stack_depth] and jumps to the op's catch ip. Both kinds live on the same
   handler stack so PERFORM resolves the nearest match in correct nesting order and
   try-markers are captured/reinstalled as continuation intermediates for free. *)
and handler_entry =
  | HFull of {
      hf_return : value;
      hf_ops : (string * value) list;
      hf_body_fiber : fiber;
      hf_parent_fiber : fiber;
    }
  | HTry of {
      ht_fiber : fiber;
      ht_frame_depth : int;
      ht_stack_depth : int;
      (* Loop-control and return stacks as they were when TRY_BEGIN ran. A
         non-resumptive op discards the entire handled body, so restoring these
         snapshots exactly drops every loop/function-return marker opened inside the
         body (correct for same-fiber, cross-fiber, and resumed-intermediate cases). *)
      ht_control : control_entry list;
      ht_return : return_entry list;
      ht_catch : (string * int) list;
    }
  (* No-fiber lowering of a tail-resumptive (THOpProvide) handler. The body runs
     inline on [hp_fiber] (the fiber current when PROVIDE ran). A matching PERFORM
     calls the op's arm closure on the current fiber and continues with its result
     (see the PROVIDE opcode comment). Carries only the op -> arm-closure table; the
     return arm runs inline via PROVIDE_END, so — unlike HFull — there is no body
     fiber and no fiber-drain return path. *)
  | HProvide of {
      hp_ops : (string * value) list;
      hp_fiber : fiber;
      (* Frame/stack depth on [hp_fiber] when PROVIDE ran. A break/continue/return
         that leaves the inline body skips PROVIDE_END, so the marker is dropped by
         depth like a try-marker. (No control/return snapshots: a provide resumes the
         body rather than discarding it, so those stacks stay valid.) *)
      hp_frame_depth : int;
      hp_stack_depth : int;
    }

(* A loop control marker (pushed by ENTER_LOOP, popped by EXIT_LOOP/LOOP_BREAK).
   Lives on the owning fiber's [fiber_control] stack, so no fiber field. *)
and control_entry = {
  ce_break_ip : int;
  ce_frame_depth : int;
  ce_stack_depth : int;
  ce_handler_depth : int;
      (* Handler-stack length when ENTER_LOOP ran. break/continue drop only the
         inline try/provide markers pushed AFTER this point (markers opened
         inside the loop body, whose TRY_END/PROVIDE_END they skip). Stack-depth
         comparison cannot express this: a handler installed immediately before
         the loop — its body being the loop — records the same sp as the loop
         entry, and would be wrongly dropped (BUG-10, BUG-12). *)
}

(* A function-return marker (pushed by ENTER_FUNC for functions that use `return`).
   Lives on the owning fiber's [fiber_return] stack. *)
and return_entry = { ret_frame_depth : int }

and continuation_data = {
  cd_fiber : fiber;
  cd_return_handler : value;
  cd_op_handlers : (string * value) list;
  cd_body_fiber : fiber;
  cd_intermediate_handlers : handler_entry list;
  mutable cd_used : bool;
}

type compiled_program = { main : prototype; global_names : string array }

let make_record_shape (field_names : string list) : record_shape =
  let fields = Array.of_list field_names in
  let index = Hashtbl.create (Array.length fields) in
  Array.iteri (fun i name -> Hashtbl.replace index name i) fields;
  { rs_fields = fields; rs_index = index }

let make_record (field_names : string list) (values : value array) : value =
  let shape = make_record_shape field_names in
  VRecord (shape, values)

let rec pp_value = function
  | VInt n -> string_of_int n
  | VFloat f ->
      (* Display spec: %g plus a trailing "." when the result could otherwise
         be read as an int ("3."). inf/nan are unambiguous, so no dot — same
         rule as the native runtime. string_of_float / show / interpolation
         use bare %g (no dot); only DISPLAY appends. *)
      let s = Printf.sprintf "%g" f in
      if
        String.contains s '.' || String.contains s 'e' || String.contains s 'n'
        || String.contains s 'i'
      then s
      else s ^ "."
  | VBool true -> "true"
  | VBool false -> "false"
  | VString s -> s
  | VByte n -> Printf.sprintf "#%02x" n
  | VRune cp ->
      if cp < 0x80 then Printf.sprintf "'%c'" (Char.chr cp)
      else
        let buf = Buffer.create 6 in
        Buffer.add_char buf '\'';
        if cp < 0x800 then begin
          Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6)));
          Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
        end
        else if cp < 0x10000 then begin
          Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12)));
          Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
          Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
        end
        else begin
          Buffer.add_char buf (Char.chr (0xF0 lor (cp lsr 18)));
          Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
          Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
          Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
        end;
        Buffer.add_char buf '\'';
        Buffer.contents buf
  | VUnit -> "()"
  | VTuple vs ->
      "(" ^ String.concat ", " (Array.to_list (Array.map pp_value vs)) ^ ")"
  | VList vs -> "[" ^ String.concat "; " (List.map pp_value vs) ^ "]"
  | VRecord (shape, values) ->
      "{ "
      ^ String.concat "; "
          (List.init (Array.length shape.rs_fields) (fun i ->
               shape.rs_fields.(i) ^ " = " ^ pp_value values.(i)))
      ^ " }"
  | VVariant (_, name, None) -> name
  | VVariant (_, name, Some v) -> (
      name ^ " "
      ^
      match v with
      | VTuple _ | VVariant (_, _, Some _) -> "(" ^ pp_value v ^ ")"
      | _ -> pp_value v)
  | VClosure _ -> "<fun>"
  | VPartial _ -> "<fun>"
  | VExternal ext ->
      if ext.ext_args = [] then Printf.sprintf "<external:%s>" ext.ext_name
      else "<fun>"
  | VProto p -> Printf.sprintf "<proto:%s>" p.name
  | VContinuation _ -> "<continuation>"
  | VRef r -> Printf.sprintf "ref(%s)" (pp_value !r)
  | VArray arr ->
      "#[" ^ String.concat "; " (Array.to_list (Array.map pp_value arr)) ^ "]"

let pp_capture = function
  | CaptureLocal i -> Printf.sprintf "local(%d)" i
  | CaptureUpvalue i -> Printf.sprintf "upvalue(%d)" i

let pp_opcode = function
  | CONST i -> Printf.sprintf "CONST %d" i
  | POP -> "POP"
  | DUP -> "DUP"
  | GET_LOCAL i -> Printf.sprintf "GET_LOCAL %d" i
  | SET_LOCAL i -> Printf.sprintf "SET_LOCAL %d" i
  | GET_UPVALUE i -> Printf.sprintf "GET_UPVALUE %d" i
  | MAKE_REF -> "MAKE_REF"
  | DEREF -> "DEREF"
  | SET_REF -> "SET_REF"
  | GET_GLOBAL i -> Printf.sprintf "GET_GLOBAL %d" i
  | DEF_GLOBAL i -> Printf.sprintf "DEF_GLOBAL %d" i
  | ADD -> "ADD"
  | SUB -> "SUB"
  | MUL -> "MUL"
  | DIV -> "DIV"
  | MOD -> "MOD"
  | NEG -> "NEG"
  | FADD -> "FADD"
  | FSUB -> "FSUB"
  | FMUL -> "FMUL"
  | FDIV -> "FDIV"
  | FNEG -> "FNEG"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | LT -> "LT"
  | GT -> "GT"
  | LE -> "LE"
  | GE -> "GE"
  | NOT -> "NOT"
  | BAND -> "BAND"
  | BOR -> "BOR"
  | BXOR -> "BXOR"
  | BNOT -> "BNOT"
  | BSHL -> "BSHL"
  | BSHR -> "BSHR"
  | JUMP off -> Printf.sprintf "JUMP %d" off
  | JUMP_IF_FALSE off -> Printf.sprintf "JUMP_IF_FALSE %d" off
  | JUMP_IF_TRUE off -> Printf.sprintf "JUMP_IF_TRUE %d" off
  | CLOSURE (i, caps) ->
      Printf.sprintf "CLOSURE %d [%s]" i
        (String.concat ", " (List.map pp_capture caps))
  | CLOSURE_REC (i, caps, self) ->
      Printf.sprintf "CLOSURE_REC %d [%s] self=%d" i
        (String.concat ", " (List.map pp_capture caps))
        self
  | CALL n -> Printf.sprintf "CALL %d" n
  | TAIL_CALL n -> Printf.sprintf "TAIL_CALL %d" n
  | RETURN -> "RETURN"
  | FUNC_RETURN -> "FUNC_RETURN"
  | ENTER_FUNC -> "ENTER_FUNC"
  | EXIT_FUNC -> "EXIT_FUNC"
  | MAKE_TUPLE n -> Printf.sprintf "MAKE_TUPLE %d" n
  | TUPLE_GET i -> Printf.sprintf "TUPLE_GET %d" i
  | MAKE_RECORD fields ->
      Printf.sprintf "MAKE_RECORD [%s]" (String.concat ", " fields)
  | FIELD name -> Printf.sprintf "FIELD %s" name
  | SET_FIELD name -> Printf.sprintf "SET_FIELD %s" name
  | RECORD_UPDATE fields ->
      Printf.sprintf "RECORD_UPDATE [%s]" (String.concat ", " fields)
  | RECORD_UPDATE_DYN n -> Printf.sprintf "RECORD_UPDATE_DYN %d" n
  | MAKE_VARIANT (tag, name, has_payload) ->
      Printf.sprintf "MAKE_VARIANT %d %s %b" tag name has_payload
  | CONS -> "CONS"
  | NIL -> "NIL"
  | TAG_EQ tag -> Printf.sprintf "TAG_EQ %d" tag
  | IS_NIL -> "IS_NIL"
  | IS_CONS -> "IS_CONS"
  | HEAD -> "HEAD"
  | TAIL -> "TAIL"
  | VARIANT_PAYLOAD -> "VARIANT_PAYLOAD"
  | MATCH_FAIL loc -> Printf.sprintf "MATCH_FAIL %s" loc
  | PERFORM op -> Printf.sprintf "PERFORM %s" op
  | HANDLE n -> Printf.sprintf "HANDLE %d" n
  | RESUME -> "RESUME"
  | ENTER_LOOP n -> Printf.sprintf "ENTER_LOOP %d" n
  | EXIT_LOOP -> "EXIT_LOOP"
  | LOOP_BREAK -> "LOOP_BREAK"
  | LOOP_CONTINUE n -> Printf.sprintf "LOOP_CONTINUE %d" n
  | FOLD_CONTINUE n -> Printf.sprintf "FOLD_CONTINUE %d" n
  | MAKE_ARRAY n -> Printf.sprintf "MAKE_ARRAY %d" n
  | INDEX -> "INDEX"
  | HALT -> "HALT"
  | GET_LOCAL_CALL (slot, arity) ->
      Printf.sprintf "GET_LOCAL_CALL %d %d" slot arity
  | GET_LOCAL_TUPLE_GET (slot, idx) ->
      Printf.sprintf "GET_LOCAL_TUPLE_GET %d %d" slot idx
  | GET_LOCAL_FIELD (slot, name) ->
      Printf.sprintf "GET_LOCAL_FIELD %d %s" slot name
  | GET_GLOBAL_CALL (idx, arity) ->
      Printf.sprintf "GET_GLOBAL_CALL %d %d" idx arity
  | GET_GLOBAL_FIELD (idx, name) ->
      Printf.sprintf "GET_GLOBAL_FIELD %d %s" idx name
  | JUMP_TABLE (min_tag, targets, default) ->
      Printf.sprintf "JUMP_TABLE min=%d [%s] default=%d" min_tag
        (String.concat "," (Array.to_list (Array.map string_of_int targets)))
        default
  | CALL_N n -> Printf.sprintf "CALL_N %d" n
  | TAIL_CALL_N n -> Printf.sprintf "TAIL_CALL_N %d" n
  | UPDATE_REC -> "UPDATE_REC"
  | TRY_BEGIN catch ->
      Printf.sprintf "TRY_BEGIN [%s]"
        (String.concat ", "
           (List.map (fun (op, ip) -> Printf.sprintf "%s->%d" op ip) catch))
  | TRY_END -> "TRY_END"
  | PROVIDE n -> Printf.sprintf "PROVIDE %d" n
  | PROVIDE_END -> "PROVIDE_END"

(* Op names a handler entry covers, used by PERFORM to find the nearest match. *)
let he_op_names = function
  | HFull { hf_ops; _ } -> List.map fst hf_ops
  | HTry { ht_catch; _ } -> List.map fst ht_catch
  | HProvide { hp_ops; _ } -> List.map fst hp_ops

let disassemble proto =
  let buf = Buffer.create 256 in
  Buffer.add_string buf
    (Printf.sprintf "=== %s (arity=%d, locals=%d) ===\n" proto.name proto.arity
       proto.num_locals);
  let prev_line = ref 0 in
  Array.iteri
    (fun i op ->
      let line =
        if i < Array.length proto.line_table then proto.line_table.(i) else 0
      in
      let line_str =
        if line > 0 && line <> !prev_line then (
          prev_line := line;
          Printf.sprintf "%4d" line)
        else "   |"
      in
      Buffer.add_string buf
        (Printf.sprintf "%s %04d  %s\n" line_str i (pp_opcode op)))
    proto.code;
  Buffer.contents buf
