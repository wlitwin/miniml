exception Runtime_error of string

let error msg = raise (Runtime_error msg)

type handler_entry = Bytecode.handler_entry =
  | HFull of {
      hf_return : Bytecode.value;
      hf_ops : (string * Bytecode.value) list;
      hf_body_fiber : Bytecode.fiber;
      hf_parent_fiber : Bytecode.fiber;
    }
  | HTry of {
      ht_fiber : Bytecode.fiber;
      ht_frame_depth : int;
      ht_stack_depth : int;
      ht_control : Bytecode.control_entry list;
      ht_return : Bytecode.return_entry list;
      ht_catch : (string * int) list;
    }
  | HProvide of {
      hp_ops : (string * Bytecode.value) list;
      hp_fiber : Bytecode.fiber;
      hp_frame_depth : int;
      hp_stack_depth : int;
    }

type control_entry = Bytecode.control_entry = {
  ce_break_ip : int;
  ce_fiber : Bytecode.fiber;
  ce_frame_depth : int;
  ce_stack_depth : int;
}

type return_entry = Bytecode.return_entry = {
  ret_fiber : Bytecode.fiber;
  ret_frame_depth : int;
}

type t = {
  mutable current_fiber : Bytecode.fiber;
  mutable handler_stack : handler_entry list;
  mutable control_stack : control_entry list;
  mutable return_stack : return_entry list;
  (* Pending no-fiber provide resumptions: while a PERFORM-matched HProvide arm runs
     on a fiber, an entry [(fiber, frame_depth, removed_handlers)] records that the
     arm was entered at [frame_depth] with [removed_handlers] (the matched HProvide
     plus inner intermediates) temporarily lifted off the handler stack. When the arm
     returns to that depth, the handlers are reinstalled and the body continues. *)
  mutable provide_resumes :
    (Bytecode.fiber * int * handler_entry list) list;
  globals : (int, Bytecode.value) Hashtbl.t;
  global_names : string array;
}

(* Fiber stacks grow on demand. Each [handle]/[try] allocates a fresh fiber, so
   starting small (instead of the old fixed 64K-slot array = ~512KB) makes
   handler installation dramatically cheaper; deep recursion grows the array by
   doubling. A generous cap turns runaway non-tail recursion into a clean
   "stack overflow" instead of an OOM. *)
let fiber_stack_init = 1024
let fiber_stack_max = 16 * 1024 * 1024

let make_fiber () : Bytecode.fiber =
  {
    fiber_stack = Array.make fiber_stack_init Bytecode.VUnit;
    fiber_sp = 0;
    fiber_frames = [];
    fiber_frame_depth = 0;
    fiber_extra_args = [];
  }

(* Ensure [f.fiber_stack] can hold at least [needed] slots, growing (doubling)
   if necessary. Safe because every fiber-stack access reads [f.fiber_stack]
   through the record field, so callers see the reallocated array. *)
let ensure_fiber_capacity (f : Bytecode.fiber) needed =
  let cap = Array.length f.fiber_stack in
  if needed > cap then begin
    if needed > fiber_stack_max then error "stack overflow";
    let new_cap = ref cap in
    while !new_cap < needed do
      new_cap := min (!new_cap * 2) fiber_stack_max
    done;
    let grown = Array.make !new_cap Bytecode.VUnit in
    Array.blit f.fiber_stack 0 grown 0 f.fiber_sp;
    f.fiber_stack <- grown
  end

let push vm v =
  let f = vm.current_fiber in
  ensure_fiber_capacity f (f.fiber_sp + 1);
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
      List.iteri
        (fun i fr ->
          let fip = fr.Bytecode.frame_ip in
          let fname = fr.frame_closure.fn_proto.name in
          let line =
            if
              fip > 0
              && fip - 1 < Array.length fr.frame_closure.fn_proto.line_table
            then fr.frame_closure.fn_proto.line_table.(fip - 1)
            else 0
          in
          Buffer.add_string trace
            (Printf.sprintf "\n  frame[%d]: %s ip=%d line=%d" i fname fip line))
        vm.current_fiber.fiber_frames;
      error
        (Printf.sprintf "expected string, got %s\nStack:%s"
           (Bytecode.pp_value v) (Buffer.contents trace))

let as_string = function
  | Bytecode.VString s -> s
  | v -> error (Printf.sprintf "expected string, got %s" (Bytecode.pp_value v))

let as_closure = function
  | Bytecode.VClosure c -> c
  | v ->
      error (Printf.sprintf "expected function, got %s" (Bytecode.pp_value v))

let as_tuple = function
  | Bytecode.VTuple t -> t
  | v -> error (Printf.sprintf "expected tuple, got %s" (Bytecode.pp_value v))

let as_record_vm vm = function
  | Bytecode.VRecord (shape, values) -> (shape, values)
  | v ->
      let trace = Buffer.create 256 in
      List.iteri
        (fun i fr ->
          let fip = fr.Bytecode.frame_ip in
          let fname = fr.frame_closure.fn_proto.name in
          let line =
            if
              fip > 0
              && fip - 1 < Array.length fr.frame_closure.fn_proto.line_table
            then fr.frame_closure.fn_proto.line_table.(fip - 1)
            else 0
          in
          Buffer.add_string trace
            (Printf.sprintf "\n  frame[%d]: %s ip=%d line=%d" i fname fip line))
        vm.current_fiber.fiber_frames;
      error
        (Printf.sprintf "expected record, got %s\nStack:%s"
           (Bytecode.pp_value v) (Buffer.contents trace))

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
  | v ->
      error
        (Printf.sprintf "expected continuation, got %s" (Bytecode.pp_value v))

let value_hash v =
  (* FNV-1a parameters for 63-bit OCaml int *)
  let fnv_offset = 0x811c9dc5 in
  let fnv_prime = 0x01000193 in
  let mix h x = h lxor x * fnv_prime in
  let mix_bytes h n =
    let h = mix h (n land 0xff) in
    let h = mix h ((n lsr 8) land 0xff) in
    let h = mix h ((n lsr 16) land 0xff) in
    let h = mix h ((n lsr 24) land 0xff) in
    let h = mix h ((n lsr 32) land 0xff) in
    let h = mix h ((n lsr 40) land 0xff) in
    let h = mix h ((n lsr 48) land 0xff) in
    let h = mix h ((n asr 56) land 0xff) in
    h
  in
  let rec hash h = function
    | Bytecode.VInt n -> mix_bytes h n
    | Bytecode.VFloat f ->
        let bits = Int64.to_int (Int64.bits_of_float f) in
        mix_bytes h bits
    | Bytecode.VBool true -> mix h 1
    | Bytecode.VBool false -> mix h 0
    | Bytecode.VString s ->
        let h = ref h in
        String.iter (fun c -> h := mix !h (Char.code c)) s;
        !h
    | Bytecode.VByte n -> mix_bytes h n
    | Bytecode.VRune n -> mix_bytes h n
    | Bytecode.VUnit -> mix h 0
    | Bytecode.VTuple vs -> Array.fold_left (fun h v -> hash h v) (mix h 1) vs
    | Bytecode.VList vs -> List.fold_left (fun h v -> hash h v) (mix h 2) vs
    | Bytecode.VVariant (tag, _, payload) -> (
        let h = mix_bytes h tag in
        match payload with None -> h | Some v -> hash h v)
    | Bytecode.VRecord (_shape, values) ->
        Array.fold_left (fun h v -> hash h v) (mix h 3) values
    | Bytecode.VArray arr -> Array.fold_left (fun h v -> hash h v) (mix h 4) arr
    | _ -> h (* closures, refs, etc. *)
  in
  hash fnv_offset v

let values_equal a b =
  let rec eq a b =
    match (a, b) with
    | Bytecode.VInt a, Bytecode.VInt b -> a = b
    | Bytecode.VFloat a, Bytecode.VFloat b -> Float.equal a b
    | Bytecode.VBool a, Bytecode.VBool b -> a = b
    | Bytecode.VString a, Bytecode.VString b -> String.equal a b
    | Bytecode.VByte a, Bytecode.VByte b -> a = b
    | Bytecode.VRune a, Bytecode.VRune b -> a = b
    | Bytecode.VUnit, Bytecode.VUnit -> true
    | Bytecode.VTuple a, Bytecode.VTuple b ->
        Array.length a = Array.length b && Array.for_all2 eq a b
    | Bytecode.VList a, Bytecode.VList b ->
        List.length a = List.length b && List.for_all2 eq a b
    | Bytecode.VVariant (t1, _, p1), Bytecode.VVariant (t2, _, p2) -> (
        t1 = t2
        &&
        match (p1, p2) with
        | None, None -> true
        | Some a, Some b -> eq a b
        | _ -> false)
    | Bytecode.VRecord (sa, va), Bytecode.VRecord (sb, vb) ->
        Array.length va = Array.length vb
        && sa.rs_fields = sb.rs_fields
        && Array.for_all2 eq va vb
    | Bytecode.VArray a, Bytecode.VArray b ->
        Array.length a = Array.length b && Array.for_all2 eq a b
    | _ -> false
  in
  eq a b

let values_compare a b =
  let rec cmp a b =
    match (a, b) with
    | Bytecode.VInt a, Bytecode.VInt b -> Int.compare a b
    | Bytecode.VFloat a, Bytecode.VFloat b -> Float.compare a b
    | Bytecode.VString a, Bytecode.VString b -> String.compare a b
    | Bytecode.VByte a, Bytecode.VByte b -> Int.compare a b
    | Bytecode.VRune a, Bytecode.VRune b -> Int.compare a b
    | Bytecode.VBool a, Bytecode.VBool b -> Bool.compare a b
    | Bytecode.VUnit, Bytecode.VUnit -> 0
    | Bytecode.VTuple a, Bytecode.VTuple b ->
        let n = min (Array.length a) (Array.length b) in
        let rec go i =
          if i >= n then Int.compare (Array.length a) (Array.length b)
          else
            let c = cmp a.(i) b.(i) in
            if c <> 0 then c else go (i + 1)
        in
        go 0
    | Bytecode.VList a, Bytecode.VList b ->
        let rec go a b =
          match (a, b) with
          | [], [] -> 0
          | [], _ -> -1
          | _, [] -> 1
          | x :: xs, y :: ys ->
              let c = cmp x y in
              if c <> 0 then c else go xs ys
        in
        go a b
    | Bytecode.VVariant (t1, _, p1), Bytecode.VVariant (t2, _, p2) -> (
        let tc = Int.compare t1 t2 in
        if tc <> 0 then tc
        else
          match (p1, p2) with
          | None, None -> 0
          | None, Some _ -> -1
          | Some _, None -> 1
          | Some a, Some b -> cmp a b)
    | _ -> 0
  in
  cmp a b

let resolve_capture vm (cap : Bytecode.capture) : Bytecode.value =
  let f = frame vm in
  match cap with
  | Bytecode.CaptureLocal slot ->
      vm.current_fiber.fiber_stack.(f.frame_base_sp + slot)
  | Bytecode.CaptureUpvalue idx -> f.frame_closure.upvalues.(idx)

(* Set up a call frame for a closure with a single argument *)
let enter_frame vm (cls : Bytecode.closure) arg is_tail =
  let fiber = vm.current_fiber in
  let base =
    if is_tail then (frame vm).Bytecode.frame_base_sp else fiber.fiber_sp
  in
  let num_locals = cls.fn_proto.num_locals in
  ensure_fiber_capacity fiber (base + num_locals);
  Array.fill fiber.fiber_stack base num_locals Bytecode.VUnit;
  fiber.fiber_stack.(base) <- arg;
  fiber.fiber_sp <- base + num_locals;
  let new_frame =
    Bytecode.{ frame_closure = cls; frame_ip = 0; frame_base_sp = base }
  in
  fiber.fiber_frames <-
    new_frame
    :: (if is_tail then List.tl fiber.fiber_frames else fiber.fiber_frames);
  if not is_tail then fiber.fiber_frame_depth <- fiber.fiber_frame_depth + 1

(* Set up a call frame for a closure with multiple arguments *)
let enter_frame_args vm (cls : Bytecode.closure) args is_tail =
  let fiber = vm.current_fiber in
  let base =
    if is_tail then (frame vm).Bytecode.frame_base_sp else fiber.fiber_sp
  in
  let num_locals = cls.fn_proto.num_locals in
  ensure_fiber_capacity fiber (base + num_locals);
  Array.fill fiber.fiber_stack base num_locals Bytecode.VUnit;
  List.iteri (fun i a -> fiber.fiber_stack.(base + i) <- a) args;
  fiber.fiber_sp <- base + num_locals;
  let new_frame =
    Bytecode.{ frame_closure = cls; frame_ip = 0; frame_base_sp = base }
  in
  fiber.fiber_frames <-
    new_frame
    :: (if is_tail then List.tl fiber.fiber_frames else fiber.fiber_frames);
  if not is_tail then fiber.fiber_frame_depth <- fiber.fiber_frame_depth + 1

let internal_call vm cls arg = enter_frame vm cls arg false

let rec split_at n lst =
  if n = 0 then ([], lst)
  else
    match lst with
    | [] -> ([], [])
    | x :: rest ->
        let first, second = split_at (n - 1) rest in
        (x :: first, second)

(* Apply n args to a function value. For CALL_N.
   Enters a frame (exact/over) or pushes VPartial (under).
   Returns true if a frame was entered, false if result was pushed. *)
let rec call_with_args vm fn_val args =
  match fn_val with
  | Bytecode.VClosure cls ->
      let arity = cls.fn_proto.arity in
      let n = List.length args in
      if n = arity then begin
        enter_frame_args vm cls args false;
        true
      end
      else if n < arity then begin
        push vm (Bytecode.VPartial (cls, args));
        false
      end
      else begin
        let use_args, extra = split_at arity args in
        vm.current_fiber.fiber_extra_args <-
          extra :: vm.current_fiber.fiber_extra_args;
        enter_frame_args vm cls use_args false;
        true
      end
  | Bytecode.VPartial (cls, existing) ->
      call_with_args vm (Bytecode.VClosure cls) (existing @ args)
  | Bytecode.VExternal ext ->
      let all = ext.ext_args @ args in
      if List.length all >= ext.ext_arity then begin
        let use_args, remaining = split_at ext.ext_arity all in
        let result = ext.ext_fn use_args in
        if remaining = [] then begin
          push vm result;
          false
        end
        else call_with_args vm result remaining
      end
      else begin
        push vm (Bytecode.VExternal { ext with ext_args = all });
        false
      end
  | _ ->
      error
        (Printf.sprintf "CALL_N: expected function, got %s"
           (Bytecode.pp_value fn_val))

(* Process pending over-application args after a frame return.
   Returns (result, entered_frame). *)
let rec process_extra_args vm result =
  match vm.current_fiber.fiber_extra_args with
  | [] -> (result, false)
  | pending :: rest -> (
      match result with
      | Bytecode.VClosure cls ->
          let arity = cls.fn_proto.arity in
          let n = List.length pending in
          if n >= arity then begin
            let use_args, remaining = split_at arity pending in
            vm.current_fiber.fiber_extra_args <-
              (if remaining = [] then rest else remaining :: rest);
            enter_frame_args vm cls use_args false;
            (Bytecode.VUnit, true)
          end
          else begin
            vm.current_fiber.fiber_extra_args <- rest;
            process_extra_args vm (Bytecode.VPartial (cls, pending))
          end
      | Bytecode.VPartial (cls, existing) ->
          vm.current_fiber.fiber_extra_args <- (existing @ pending) :: rest;
          process_extra_args vm (Bytecode.VClosure cls)
      | Bytecode.VExternal ext ->
          let all = ext.ext_args @ pending in
          if List.length all >= ext.ext_arity then begin
            let use_args, remaining = split_at ext.ext_arity all in
            vm.current_fiber.fiber_extra_args <-
              (if remaining = [] then rest else remaining :: rest);
            let ext_result = ext.ext_fn use_args in
            process_extra_args vm ext_result
          end
          else begin
            vm.current_fiber.fiber_extra_args <- rest;
            process_extra_args vm
              (Bytecode.VExternal { ext with ext_args = all })
          end
      | _ ->
          error
            (Printf.sprintf "expected function in over-application, got %s"
               (Bytecode.pp_value result)))

(* Find a full handler entry whose body fiber matches the given fiber. Only
   fiber-based handlers reach this (when a body fiber's frames drain); try-markers
   complete inline via TRY_END and never own a body fiber. *)
let find_handler_for_fiber vm fiber =
  List.find_opt
    (function
      | HFull { hf_body_fiber; _ } -> hf_body_fiber == fiber
      | HTry _ | HProvide _ -> false)
    vm.handler_stack

let remove_handler vm he =
  vm.handler_stack <- List.filter (fun h -> h != he) vm.handler_stack

(* Drop inline try/provide markers opened at or above [stack_depth] on [fiber]. Used
   when a non-local jump (break/continue) leaves an inline handler body without running
   its TRY_END/PROVIDE_END. The pending provide-resume bookkeeping is cleaned
   separately by [drop_provide_resumes_above] (keyed on frame depth). *)
let drop_try_markers_above vm fiber stack_depth =
  vm.handler_stack <-
    List.filter
      (function
        | HTry { ht_fiber; ht_stack_depth; _ } ->
            not (ht_fiber == fiber && ht_stack_depth >= stack_depth)
        | HProvide { hp_fiber; hp_stack_depth; _ } ->
            not (hp_fiber == fiber && hp_stack_depth >= stack_depth)
        | HFull _ -> true)
      vm.handler_stack

(* Drop pending provide-resume entries on [fiber] whose recorded frame depth is at or
   above [frame_depth]. Used when frames are unwound non-locally (a try-discard,
   break/continue, or return) past a point where a provide arm was mid-flight, so its
   reinstall would otherwise fire spuriously when the depth is later revisited. *)
let drop_provide_resumes_above vm fiber frame_depth =
  if vm.provide_resumes <> [] then
    vm.provide_resumes <-
      List.filter
        (fun (f, d, _) -> not (f == fiber && d >= frame_depth))
        vm.provide_resumes

type frame_action = FrameLoop | FrameReturn of Bytecode.value

let run vm =
  let complete_frame result =
    vm.current_fiber.fiber_sp <- (frame vm).Bytecode.frame_base_sp;
    vm.current_fiber.fiber_frames <- List.tl vm.current_fiber.fiber_frames;
    vm.current_fiber.fiber_frame_depth <- vm.current_fiber.fiber_frame_depth - 1;
    (* If a provide arm just returned (depth dropped back to where it was entered),
       reinstall the HProvide marker + intermediates that were lifted off for the arm,
       so the resumed body sees the handler again. [result] is the resume value, left
       on the stack below by the normal branches. *)
    (match vm.provide_resumes with
    | (f, d, removed) :: rest
      when f == vm.current_fiber && d = vm.current_fiber.fiber_frame_depth ->
        vm.handler_stack <- removed @ vm.handler_stack;
        vm.provide_resumes <- rest
    | _ -> ());
    if vm.current_fiber.fiber_extra_args <> [] then begin
      let result, entered = process_extra_args vm result in
      if entered then FrameLoop
      else if vm.current_fiber.fiber_frames = [] then begin
        match find_handler_for_fiber vm vm.current_fiber with
        | Some (HFull { hf_parent_fiber; hf_return; _ } as he) ->
            remove_handler vm he;
            vm.current_fiber <- hf_parent_fiber;
            internal_call vm (as_closure hf_return) result;
            FrameLoop
        | Some (HTry _ | HProvide _) | None -> FrameReturn result
      end
      else begin
        push vm result;
        FrameLoop
      end
    end
    else if vm.current_fiber.fiber_frames = [] then begin
      match find_handler_for_fiber vm vm.current_fiber with
      | Some (HFull { hf_parent_fiber; hf_return; _ } as he) ->
          remove_handler vm he;
          vm.current_fiber <- hf_parent_fiber;
          internal_call vm (as_closure hf_return) result;
          FrameLoop
      | Some (HTry _ | HProvide _) | None -> FrameReturn result
    end
    else begin
      push vm result;
      FrameLoop
    end
  in
  let rec loop () =
    let op = read_op vm in
    match op with
    (* --- Stack and variables --- *)
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
        push vm vm.current_fiber.fiber_stack.((frame vm).frame_base_sp + i);
        loop ()
    | Bytecode.SET_LOCAL i ->
        vm.current_fiber.fiber_stack.((frame vm).frame_base_sp + i) <- pop vm;
        loop ()
    | Bytecode.GET_UPVALUE i ->
        push vm (frame vm).frame_closure.upvalues.(i);
        loop ()
    | Bytecode.MAKE_REF ->
        let v = pop vm in
        push vm (Bytecode.VRef (ref v));
        loop ()
    | Bytecode.DEREF -> (
        match pop vm with
        | Bytecode.VRef r ->
            push vm !r;
            loop ()
        | _ -> error "DEREF on non-ref value")
    | Bytecode.SET_REF -> (
        (* stack: [... ref new_value] -> pop new_value, pop ref, set ref *)
        let r = pop vm in
        let v = pop vm in
        match r with
        | Bytecode.VRef cell ->
            cell := v;
            loop ()
        | _ -> error "SET_REF on non-ref value")
    | Bytecode.GET_GLOBAL i -> (
        match Hashtbl.find_opt vm.globals i with
        | Some v ->
            push vm v;
            loop ()
        | None ->
            let name =
              if i < Array.length vm.global_names then vm.global_names.(i)
              else "?"
            in
            error (Printf.sprintf "undefined global: %s" name))
    | Bytecode.DEF_GLOBAL i ->
        let v = pop vm in
        Hashtbl.replace vm.globals i v;
        loop ()
    (* --- Arithmetic --- *)
    | Bytecode.ADD ->
        let b = pop vm in
        let a = pop vm in
        push vm (Bytecode.VInt (as_int a + as_int b));
        loop ()
    | Bytecode.SUB ->
        let b = pop vm in
        let a = pop vm in
        push vm (Bytecode.VInt (as_int a - as_int b));
        loop ()
    | Bytecode.MUL ->
        let b = pop vm in
        let a = pop vm in
        push vm (Bytecode.VInt (as_int a * as_int b));
        loop ()
    | Bytecode.DIV ->
        let b = pop vm in
        let a = pop vm in
        let bv = as_int b in
        if bv = 0 then error "division by zero";
        push vm (Bytecode.VInt (as_int a / bv));
        loop ()
    | Bytecode.MOD ->
        let b = pop vm in
        let a = pop vm in
        let bv = as_int b in
        if bv = 0 then error "modulo by zero";
        push vm (Bytecode.VInt (as_int a mod bv));
        loop ()
    | Bytecode.NEG ->
        let v = pop vm in
        push vm (Bytecode.VInt (-as_int v));
        loop ()
    | Bytecode.FADD ->
        let b = pop vm in
        let a = pop vm in
        push vm (Bytecode.VFloat (as_float a +. as_float b));
        loop ()
    | Bytecode.FSUB ->
        let b = pop vm in
        let a = pop vm in
        push vm (Bytecode.VFloat (as_float a -. as_float b));
        loop ()
    | Bytecode.FMUL ->
        let b = pop vm in
        let a = pop vm in
        push vm (Bytecode.VFloat (as_float a *. as_float b));
        loop ()
    | Bytecode.FDIV ->
        let b = pop vm in
        let a = pop vm in
        push vm (Bytecode.VFloat (as_float a /. as_float b));
        loop ()
    | Bytecode.FNEG ->
        let v = pop vm in
        push vm (Bytecode.VFloat (-.as_float v));
        loop ()
    (* --- Comparisons and logic --- *)
    | Bytecode.EQ ->
        let b = pop vm in
        let a = pop vm in
        push vm (Bytecode.VBool (values_equal a b));
        loop ()
    | Bytecode.NEQ ->
        let b = pop vm in
        let a = pop vm in
        push vm (Bytecode.VBool (not (values_equal a b)));
        loop ()
    | Bytecode.LT ->
        let b = pop vm in
        let a = pop vm in
        let result =
          match (a, b) with
          | Bytecode.VFloat fa, Bytecode.VFloat fb -> fa < fb
          | Bytecode.VString sa, Bytecode.VString sb -> String.compare sa sb < 0
          | Bytecode.VByte a, Bytecode.VByte b -> a < b
          | Bytecode.VRune a, Bytecode.VRune b -> a < b
          | _ -> as_int a < as_int b
        in
        push vm (Bytecode.VBool result);
        loop ()
    | Bytecode.GT ->
        let b = pop vm in
        let a = pop vm in
        let result =
          match (a, b) with
          | Bytecode.VFloat fa, Bytecode.VFloat fb -> fa > fb
          | Bytecode.VString sa, Bytecode.VString sb -> String.compare sa sb > 0
          | Bytecode.VByte a, Bytecode.VByte b -> a > b
          | Bytecode.VRune a, Bytecode.VRune b -> a > b
          | _ -> as_int a > as_int b
        in
        push vm (Bytecode.VBool result);
        loop ()
    | Bytecode.LE ->
        let b = pop vm in
        let a = pop vm in
        let result =
          match (a, b) with
          | Bytecode.VFloat fa, Bytecode.VFloat fb -> fa <= fb
          | Bytecode.VString sa, Bytecode.VString sb ->
              String.compare sa sb <= 0
          | Bytecode.VByte a, Bytecode.VByte b -> a <= b
          | Bytecode.VRune a, Bytecode.VRune b -> a <= b
          | _ -> as_int a <= as_int b
        in
        push vm (Bytecode.VBool result);
        loop ()
    | Bytecode.GE ->
        let b = pop vm in
        let a = pop vm in
        let result =
          match (a, b) with
          | Bytecode.VFloat fa, Bytecode.VFloat fb -> fa >= fb
          | Bytecode.VString sa, Bytecode.VString sb ->
              String.compare sa sb >= 0
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
    (* --- Bitwise --- *)
    | Bytecode.BAND ->
        let b = pop vm in
        let a = pop vm in
        push vm (Bytecode.VInt (as_int a land as_int b));
        loop ()
    | Bytecode.BOR ->
        let b = pop vm in
        let a = pop vm in
        push vm (Bytecode.VInt (as_int a lor as_int b));
        loop ()
    | Bytecode.BXOR ->
        let b = pop vm in
        let a = pop vm in
        push vm (Bytecode.VInt (as_int a lxor as_int b));
        loop ()
    | Bytecode.BNOT ->
        let v = pop vm in
        push vm (Bytecode.VInt (lnot (as_int v)));
        loop ()
    | Bytecode.BSHL ->
        let b = pop vm in
        let a = pop vm in
        push vm (Bytecode.VInt (as_int a lsl as_int b));
        loop ()
    | Bytecode.BSHR ->
        let b = pop vm in
        let a = pop vm in
        push vm (Bytecode.VInt (as_int a lsr as_int b));
        loop ()
    (* --- Control flow --- *)
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
    (* --- Closures and function calls --- *)
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
        dispatch_call fn_val arg false
    | Bytecode.TAIL_CALL _ ->
        let arg = pop vm in
        let fn_val = pop vm in
        dispatch_call fn_val arg true
    | Bytecode.RETURN -> (
        let result = pop vm in
        match complete_frame result with
        | FrameLoop -> loop ()
        | FrameReturn v -> v)
    | Bytecode.CALL_N n ->
        let args = Array.init n (fun _ -> pop vm) in
        let args_list = List.rev (Array.to_list args) in
        let fn_val = pop vm in
        ignore (call_with_args vm fn_val args_list);
        loop ()
    | Bytecode.TAIL_CALL_N n ->
        let args = Array.init n (fun _ -> pop vm) in
        let args_list = List.rev (Array.to_list args) in
        let fn_val = pop vm in
        let entered = ref false in
        let result_ref = ref Bytecode.VUnit in
        let rec resolve fn remaining =
          match fn with
          | Bytecode.VClosure cls ->
              let arity = cls.fn_proto.arity in
              let n_args = List.length remaining in
              if n_args = arity then begin
                enter_frame_args vm cls remaining true;
                entered := true
              end
              else if n_args < arity then
                result_ref := Bytecode.VPartial (cls, remaining)
              else begin
                let use_args, extra = split_at arity remaining in
                vm.current_fiber.fiber_extra_args <-
                  extra :: vm.current_fiber.fiber_extra_args;
                enter_frame_args vm cls use_args true;
                entered := true
              end
          | Bytecode.VPartial (cls, existing) ->
              resolve (Bytecode.VClosure cls) (existing @ remaining)
          | Bytecode.VExternal ext ->
              let all = ext.ext_args @ remaining in
              if List.length all >= ext.ext_arity then begin
                let use_args, rest = split_at ext.ext_arity all in
                let r = ext.ext_fn use_args in
                if rest = [] then result_ref := r else resolve r rest
              end
              else result_ref := Bytecode.VExternal { ext with ext_args = all }
          | _ ->
              error
                (Printf.sprintf "TAIL_CALL_N: expected function, got %s"
                   (Bytecode.pp_value fn))
        in
        resolve fn_val args_list;
        if !entered then loop ()
        else begin
          let result = !result_ref in
          match complete_frame result with
          | FrameLoop -> loop ()
          | FrameReturn v -> v
        end
    (* --- Data construction and access --- *)
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
    | Bytecode.FIELD name -> (
        let record = pop vm in
        match record with
        | Bytecode.VRecord (shape, values) -> (
            match Hashtbl.find_opt shape.rs_index name with
            | Some idx ->
                push vm values.(idx);
                loop ()
            | None -> error (Printf.sprintf "record has no field: %s" name))
        | _ ->
            let trace = Buffer.create 256 in
            List.iteri
              (fun i fr ->
                let fip = fr.Bytecode.frame_ip in
                let fname = fr.frame_closure.fn_proto.name in
                let line =
                  if
                    fip > 0
                    && fip - 1
                       < Array.length fr.frame_closure.fn_proto.line_table
                  then fr.frame_closure.fn_proto.line_table.(fip - 1)
                  else 0
                in
                Buffer.add_string trace
                  (Printf.sprintf "\n  frame[%d]: %s ip=%d line=%d" i fname fip
                     line))
              vm.current_fiber.fiber_frames;
            error
              (Printf.sprintf "FIELD '%s': expected record, got %s\nStack:%s"
                 name (Bytecode.pp_value record) (Buffer.contents trace)))
    | Bytecode.SET_FIELD name -> (
        let new_val = pop vm in
        let record = pop vm in
        let shape, values = as_record_vm vm record in
        match Hashtbl.find_opt shape.rs_index name with
        | Some idx ->
            values.(idx) <- new_val;
            loop ()
        | None -> error (Printf.sprintf "record has no field: %s" name))
    | Bytecode.RECORD_UPDATE field_names ->
        let n = List.length field_names in
        let new_vals = Array.init n (fun _ -> Bytecode.VUnit) in
        for i = n - 1 downto 0 do
          new_vals.(i) <- pop vm
        done;
        let shape, base_values = as_record_vm vm (pop vm) in
        let copy = Array.copy base_values in
        List.iteri
          (fun i name ->
            match Hashtbl.find_opt shape.rs_index name with
            | Some idx -> copy.(idx) <- new_vals.(i)
            | None -> error (Printf.sprintf "record has no field: %s" name))
          field_names;
        push vm (Bytecode.VRecord (shape, copy));
        loop ()
    | Bytecode.RECORD_UPDATE_DYN n ->
        (* Stack: base, idx1, val1, ..., idxN, valN *)
        let pairs = Array.init n (fun _ -> (0, Bytecode.VUnit)) in
        for i = n - 1 downto 0 do
          let v = pop vm in
          let idx =
            match pop vm with
            | Bytecode.VInt i -> i
            | _ -> error "RECORD_UPDATE_DYN: expected int index"
          in
          pairs.(i) <- (idx, v)
        done;
        let shape, base_values = as_record_vm vm (pop vm) in
        let copy = Array.copy base_values in
        Array.iter
          (fun (idx, v) ->
            if idx >= 0 && idx < Array.length copy then copy.(idx) <- v
            else
              error
                (Printf.sprintf
                   "RECORD_UPDATE_DYN: index %d out of bounds (record has %d \
                    fields)"
                   idx (Array.length copy)))
          pairs;
        push vm (Bytecode.VRecord (shape, copy));
        loop ()
    | Bytecode.MAKE_VARIANT (tag, ctor_name, has_payload) ->
        if has_payload then begin
          let payload = pop vm in
          push vm (Bytecode.VVariant (tag, ctor_name, Some payload))
        end
        else push vm (Bytecode.VVariant (tag, ctor_name, None));
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
        let vtag, _, _ = as_variant v in
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
    | Bytecode.HEAD -> (
        let v = pop vm in
        let l = as_list v in
        match l with
        | hd :: _ ->
            push vm hd;
            loop ()
        | [] -> error "head of empty list")
    | Bytecode.TAIL -> (
        let v = pop vm in
        let l = as_list v in
        match l with
        | _ :: tl ->
            push vm (Bytecode.VList tl);
            loop ()
        | [] -> error "tail of empty list")
    | Bytecode.VARIANT_PAYLOAD -> (
        let v = pop vm in
        let _, _, payload = as_variant v in
        match payload with
        | Some p ->
            push vm p;
            loop ()
        | None -> error "variant has no payload")
    | Bytecode.MATCH_FAIL loc ->
        error (Printf.sprintf "non-exhaustive match at %s" loc)
    (* --- Effects --- *)
    | Bytecode.PERFORM op_name ->
        let arg = pop vm in
        (* Find the nearest handler (full or try) covering this op. *)
        let rec find_idx i = function
          | [] ->
              error (Printf.sprintf "unhandled effect operation: %s" op_name)
          | he :: rest ->
              if List.mem op_name (Bytecode.he_op_names he) then (i, he)
              else find_idx (i + 1) rest
        in
        let match_idx, he = find_idx 0 vm.handler_stack in
        (match he with
        | HTry
            {
              ht_fiber;
              ht_frame_depth;
              ht_stack_depth;
              ht_control;
              ht_return;
              ht_catch;
            } ->
            (* No-fiber, non-resumptive handler: unwind to the TRY_BEGIN marker
               (on [ht_fiber] — the current fiber, or an enclosing one if a full
               handle is nested between) and jump to the op's catch block. The whole
               handled body is discarded, so restore the control/return stacks to
               their TRY_BEGIN snapshots and drop the matched marker + everything
               above it from the handler stack. *)
            let catch_ip = List.assoc op_name ht_catch in
            vm.handler_stack <-
              List.filteri (fun i _ -> i > match_idx) vm.handler_stack;
            let fiber = ht_fiber in
            while fiber.fiber_frame_depth > ht_frame_depth do
              fiber.fiber_frames <- List.tl fiber.fiber_frames;
              fiber.fiber_frame_depth <- fiber.fiber_frame_depth - 1
            done;
            fiber.fiber_sp <- ht_stack_depth;
            fiber.fiber_extra_args <- [];
            vm.control_stack <- ht_control;
            vm.return_stack <- ht_return;
            vm.current_fiber <- fiber;
            push vm arg;
            (frame vm).frame_ip <- catch_ip;
            loop ()
        | HProvide { hp_ops; _ } ->
            (* No-fiber, tail-resumptive handler: call the op's arm closure
               [fun arg -> value] on the CURRENT fiber and continue the body with its
               result. Tail-resumption resumes exactly once, immediately, so no
               continuation is reified and the fiber the perform happened on is
               irrelevant. The matched handler + inner intermediates are lifted off the
               stack while the arm runs (deep semantics: the arm executes outside this
               handler) and reinstalled by [complete_frame] when the arm returns to the
               recorded depth. *)
            let arm = List.assoc op_name hp_ops in
            let removed =
              List.filteri (fun i _ -> i <= match_idx) vm.handler_stack
            in
            vm.handler_stack <-
              List.filteri (fun i _ -> i > match_idx) vm.handler_stack;
            vm.provide_resumes <-
              (vm.current_fiber, vm.current_fiber.fiber_frame_depth, removed)
              :: vm.provide_resumes;
            internal_call vm (as_closure arm) arg;
            loop ()
        | HFull
            { hf_return; hf_ops; hf_body_fiber; hf_parent_fiber } ->
            let handler_fn = List.assoc op_name hf_ops in
            (* Collect intermediate handlers (between stack top and matched handler) *)
            let intermediates =
              List.filteri (fun i _ -> i < match_idx) vm.handler_stack
            in
            (* Create continuation from current fiber *)
            let cont =
              Bytecode.
                {
                  cd_fiber = vm.current_fiber;
                  cd_return_handler = hf_return;
                  cd_op_handlers = hf_ops;
                  cd_body_fiber = hf_body_fiber;
                  cd_intermediate_handlers = intermediates;
                  cd_used = false;
                }
            in
            (* Build (arg, k) tuple *)
            let pair = Bytecode.VTuple [| arg; VContinuation cont |] in
            (* Remove matched handler AND all intermediates *)
            vm.handler_stack <-
              List.filteri (fun i _ -> i > match_idx) vm.handler_stack;
            (* Switch to parent fiber and call handler *)
            vm.current_fiber <- hf_parent_fiber;
            internal_call vm (as_closure handler_fn) pair;
            loop ())
    | Bytecode.HANDLE n_ops ->
        (* Stack layout: body_thunk, return_handler, (handler1, name1), (handler2, name2), ... *)
        (* Pop op handlers in reverse order *)
        let ops =
          List.init n_ops (fun _ ->
              let handler_closure = pop vm in
              let op_name = as_string (pop vm) in
              (op_name, handler_closure))
        in
        let return_handler = pop vm in
        let body_thunk = pop vm in
        (* Create body fiber *)
        let body_fiber = make_fiber () in
        (* Install handler *)
        let he =
          HFull
            {
              hf_return = return_handler;
              hf_ops = ops;
              hf_body_fiber = body_fiber;
              hf_parent_fiber = vm.current_fiber;
            }
        in
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
        ensure_fiber_capacity body_fiber (body_fiber.fiber_sp + 1);
        body_fiber.fiber_stack.(body_fiber.fiber_sp) <- v;
        body_fiber.fiber_sp <- body_fiber.fiber_sp + 1;
        (* Reinstall caught handler with original body fiber *)
        let he =
          HFull
            {
              hf_return = cont.cd_return_handler;
              hf_ops = cont.cd_op_handlers;
              hf_body_fiber = cont.cd_body_fiber;
              hf_parent_fiber = vm.current_fiber;
            }
        in
        (* Reinstall intermediates (innermost first) then caught handler *)
        vm.handler_stack <-
          cont.cd_intermediate_handlers @ (he :: vm.handler_stack);
        (* Switch to body fiber *)
        vm.current_fiber <- body_fiber;
        loop ()
    (* --- No-fiber (try) handlers --- *)
    | Bytecode.TRY_BEGIN catch ->
        let fiber = vm.current_fiber in
        let he =
          HTry
            {
              ht_fiber = fiber;
              ht_frame_depth = fiber.fiber_frame_depth;
              ht_stack_depth = fiber.fiber_sp;
              ht_control = vm.control_stack;
              ht_return = vm.return_stack;
              ht_catch = catch;
            }
        in
        vm.handler_stack <- he :: vm.handler_stack;
        loop ()
    | Bytecode.TRY_END ->
        (* Normal completion of a try body: pop the nearest HTry marker. *)
        let rec drop = function
          | HTry _ :: rest -> rest
          | ((HFull _ | HProvide _) as h) :: rest -> h :: drop rest
          | [] -> error "TRY_END: no try marker"
        in
        vm.handler_stack <- drop vm.handler_stack;
        loop ()
    | Bytecode.PROVIDE n ->
        (* Install a no-fiber tail-resumptive handler; the body runs inline. Pop the
           [n] (op_name, arm_closure) pairs pushed by the compiler. *)
        let fiber = vm.current_fiber in
        let ops =
          List.init n (fun _ ->
              let arm = pop vm in
              let op_name = as_string (pop vm) in
              (op_name, arm))
        in
        let he =
          HProvide
            {
              hp_ops = ops;
              hp_fiber = fiber;
              hp_frame_depth = fiber.fiber_frame_depth;
              hp_stack_depth = fiber.fiber_sp;
            }
        in
        vm.handler_stack <- he :: vm.handler_stack;
        loop ()
    | Bytecode.PROVIDE_END ->
        (* Normal completion of a provide body: pop the nearest HProvide marker. *)
        let rec drop = function
          | HProvide _ :: rest -> rest
          | ((HFull _ | HTry _) as h) :: rest -> h :: drop rest
          | [] -> error "PROVIDE_END: no provide marker"
        in
        vm.handler_stack <- drop vm.handler_stack;
        loop ()
    (* --- Loop and return control --- *)
    | Bytecode.ENTER_LOOP break_target ->
        let fiber = vm.current_fiber in
        let ce =
          {
            ce_break_ip = break_target;
            ce_fiber = fiber;
            ce_frame_depth = fiber.fiber_frame_depth;
            ce_stack_depth = fiber.fiber_sp;
          }
        in
        vm.control_stack <- ce :: vm.control_stack;
        loop ()
    | Bytecode.EXIT_LOOP -> (
        match vm.control_stack with
        | _ :: rest ->
            vm.control_stack <- rest;
            loop ()
        | [] -> error "EXIT_LOOP: no control entry")
    | Bytecode.LOOP_BREAK -> (
        let break_value = pop vm in
        match vm.control_stack with
        | ce :: rest ->
            vm.control_stack <- rest;
            let fiber = ce.ce_fiber in
            (* Unwind frames until we reach the loop entry depth *)
            while fiber.fiber_frame_depth > ce.ce_frame_depth do
              fiber.fiber_frames <- List.tl fiber.fiber_frames;
              fiber.fiber_frame_depth <- fiber.fiber_frame_depth - 1
            done;
            (* Drop inline try/provide markers opened inside the loop body (break skips
               TRY_END/PROVIDE_END). *)
            drop_try_markers_above vm fiber ce.ce_stack_depth;
            drop_provide_resumes_above vm fiber ce.ce_frame_depth;
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
        let re =
          { ret_fiber = fiber; ret_frame_depth = fiber.fiber_frame_depth }
        in
        vm.return_stack <- re :: vm.return_stack;
        loop ()
    | Bytecode.EXIT_FUNC -> (
        match vm.return_stack with
        | _ :: rest ->
            vm.return_stack <- rest;
            loop ()
        | [] -> error "EXIT_FUNC: no return entry")
    | Bytecode.FUNC_RETURN ->
        let result = pop vm in
        let fiber = vm.current_fiber in
        (* Find and remove the return entry for this fiber *)
        let rec find_and_remove acc = function
          | [] -> error "FUNC_RETURN: no return entry"
          | re :: rest when re.ret_fiber == fiber ->
              (re, List.rev_append acc rest)
          | re :: rest -> find_and_remove (re :: acc) rest
        in
        let re, remaining = find_and_remove [] vm.return_stack in
        vm.return_stack <- remaining;
        let target_depth = re.ret_frame_depth in
        (* Unwind frames back to the target function *)
        while fiber.fiber_frame_depth > target_depth do
          fiber.fiber_frames <- List.tl fiber.fiber_frames;
          fiber.fiber_frame_depth <- fiber.fiber_frame_depth - 1
        done;
        (* Clean up control_stack entries above target depth *)
        vm.control_stack <-
          List.filter
            (fun ce ->
              not (ce.ce_fiber == fiber && ce.ce_frame_depth >= target_depth))
            vm.control_stack;
        (* Drop inline try/provide markers opened inside the returned-from frames (a
           `return` inside such a body skips its TRY_END/PROVIDE_END). *)
        vm.handler_stack <-
          List.filter
            (function
              | HTry { ht_fiber; ht_frame_depth; _ } ->
                  not (ht_fiber == fiber && ht_frame_depth >= target_depth)
              | HProvide { hp_fiber; hp_frame_depth; _ } ->
                  not (hp_fiber == fiber && hp_frame_depth >= target_depth)
              | HFull _ -> true)
            vm.handler_stack;
        drop_provide_resumes_above vm fiber target_depth;
        (* Restore stack pointer and pop the target function's frame *)
        let base_sp = (frame vm).Bytecode.frame_base_sp in
        fiber.fiber_sp <- base_sp;
        fiber.fiber_frames <- List.tl fiber.fiber_frames;
        fiber.fiber_frame_depth <- fiber.fiber_frame_depth - 1;
        (* Clear any extra_args from over-applications within the unwound frames *)
        fiber.fiber_extra_args <- [];
        let result, entered = process_extra_args vm result in
        if entered then loop ()
        else if fiber.fiber_frames = [] then begin
          match find_handler_for_fiber vm fiber with
          | Some (HFull { hf_parent_fiber; hf_return; _ } as he) ->
              remove_handler vm he;
              vm.current_fiber <- hf_parent_fiber;
              internal_call vm (as_closure hf_return) result;
              loop ()
          | Some (HTry _ | HProvide _) | None -> result
        end
        else begin
          push vm result;
          loop ()
        end
    | Bytecode.LOOP_CONTINUE target -> (
        match vm.control_stack with
        | ce :: _ ->
            let fiber = ce.ce_fiber in
            (* Drop inline try/provide markers opened in the body (continue skips
               TRY_END/PROVIDE_END). *)
            drop_try_markers_above vm fiber ce.ce_stack_depth;
            drop_provide_resumes_above vm fiber ce.ce_frame_depth;
            fiber.fiber_sp <- ce.ce_stack_depth;
            let f = frame vm in
            f.frame_ip <- target;
            loop ()
        | [] -> error "LOOP_CONTINUE: no control entry")
    | Bytecode.FOLD_CONTINUE _n_pops -> (
        let continue_value = pop vm in
        let fiber = vm.current_fiber in
        (* Restore sp to frame base (cleans up locals + temps) *)
        fiber.fiber_sp <- (frame vm).Bytecode.frame_base_sp;
        (* Pop the fold callback frame and push result *)
        match fiber.fiber_frames with
        | _ :: rest ->
            fiber.fiber_frames <- rest;
            fiber.fiber_frame_depth <- fiber.fiber_frame_depth - 1;
            push vm continue_value;
            loop ()
        | [] -> error "FOLD_CONTINUE: no frame to return from")
    (* --- Arrays and indexing --- *)
    | Bytecode.MAKE_ARRAY n ->
        let elems = ref [] in
        for _ = 1 to n do
          elems := pop vm :: !elems
        done;
        push vm (Bytecode.VArray (Array.of_list !elems));
        loop ()
    | Bytecode.INDEX -> (
        let idx = as_int (pop vm) in
        let base = pop vm in
        match base with
        | Bytecode.VString s ->
            if idx < 0 || idx >= String.length s then
              error
                (Printf.sprintf "string index out of bounds: %d (length %d)" idx
                   (String.length s));
            push vm (Bytecode.VByte (Char.code s.[idx]));
            loop ()
        | Bytecode.VArray a ->
            if idx < 0 || idx >= Array.length a then
              error
                (Printf.sprintf "array index out of bounds: %d (length %d)" idx
                   (Array.length a));
            push vm a.(idx);
            loop ()
        | _ -> error "index operation requires string or array")
    | Bytecode.GET_LOCAL_CALL (slot, _arity) ->
        let fn_val = pop vm in
        let arg =
          vm.current_fiber.fiber_stack.((frame vm).frame_base_sp + slot)
        in
        dispatch_call fn_val arg false
    (* --- Fused opcodes --- *)
    | Bytecode.GET_LOCAL_TUPLE_GET (slot, idx) ->
        let tup =
          vm.current_fiber.fiber_stack.((frame vm).frame_base_sp + slot)
        in
        let arr = as_tuple tup in
        if idx >= Array.length arr then error "tuple index out of bounds";
        push vm arr.(idx);
        loop ()
    | Bytecode.GET_LOCAL_FIELD (slot, name) -> (
        let record =
          vm.current_fiber.fiber_stack.((frame vm).frame_base_sp + slot)
        in
        match record with
        | Bytecode.VRecord (shape, values) -> (
            match Hashtbl.find_opt shape.rs_index name with
            | Some idx ->
                push vm values.(idx);
                loop ()
            | None -> error (Printf.sprintf "record has no field: %s" name))
        | _ ->
            error
              (Printf.sprintf "GET_LOCAL_FIELD '%s': expected record, got %s"
                 name (Bytecode.pp_value record)))
    | Bytecode.GET_GLOBAL_CALL (idx, _arity) ->
        let fn_val = pop vm in
        let arg =
          match Hashtbl.find_opt vm.globals idx with
          | Some v -> v
          | None ->
              let name =
                if idx < Array.length vm.global_names then vm.global_names.(idx)
                else "?"
              in
              error (Printf.sprintf "undefined global: %s" name)
        in
        dispatch_call fn_val arg false
    | Bytecode.GET_GLOBAL_FIELD (idx, name) -> (
        let record =
          match Hashtbl.find_opt vm.globals idx with
          | Some v -> v
          | None ->
              let gname =
                if idx < Array.length vm.global_names then vm.global_names.(idx)
                else "?"
              in
              error (Printf.sprintf "undefined global: %s" gname)
        in
        match record with
        | Bytecode.VRecord (shape, values) -> (
            match Hashtbl.find_opt shape.rs_index name with
            | Some i ->
                push vm values.(i);
                loop ()
            | None -> error (Printf.sprintf "record has no field: %s" name))
        | _ ->
            error
              (Printf.sprintf "GET_GLOBAL_FIELD '%s': expected record, got %s"
                 name (Bytecode.pp_value record)))
    | Bytecode.JUMP_TABLE (min_tag, targets, default_target) ->
        let v = pop vm in
        let vtag, _, _ = as_variant v in
        let idx = vtag - min_tag in
        if idx >= 0 && idx < Array.length targets then
          (frame vm).frame_ip <- targets.(idx)
        else (frame vm).frame_ip <- default_target;
        loop ()
    (* --- Recursive update and halt --- *)
    | Bytecode.UPDATE_REC ->
        (* Backpatch a placeholder value with the actual computed value for recursive
         definitions (e.g. `let rec xs = 1 :: xs`). The placeholder was allocated with
         the right shape before the recursive body was evaluated, so both values have
         identical runtime representations. We use Obj.repr/Obj.set_field to mutate
         the placeholder in-place because: (1) VList wraps an immutable OCaml list
         that can't be mutated through normal APIs, and (2) VVariant's tag+payload
         fields aren't individually exposed. This relies on OCaml's heap object layout
         remaining stable. VTuple/VRecord/VArray use safe Array.blit instead. *)
        let placeholder = pop vm in
        let computed = pop vm in
        (match (placeholder, computed) with
        | Bytecode.VList _, Bytecode.VList _ ->
            let p_obj = Obj.repr placeholder in
            let c_obj = Obj.repr computed in
            let p_list = Obj.field p_obj 0 in
            let c_list = Obj.field c_obj 0 in
            Obj.set_field p_list 0 (Obj.field c_list 0);
            Obj.set_field p_list 1 (Obj.field c_list 1)
        | Bytecode.VTuple p_arr, Bytecode.VTuple c_arr ->
            Array.blit c_arr 0 p_arr 0 (Array.length c_arr)
        | Bytecode.VRecord (_, p_arr), Bytecode.VRecord (_, c_arr) ->
            Array.blit c_arr 0 p_arr 0 (Array.length c_arr)
        | Bytecode.VArray p_arr, Bytecode.VArray c_arr ->
            Array.blit c_arr 0 p_arr 0 (Array.length c_arr)
        | Bytecode.VVariant _, Bytecode.VVariant _ ->
            let p_obj = Obj.repr placeholder in
            let c_obj = Obj.repr computed in
            for i = 0 to Obj.size c_obj - 1 do
              Obj.set_field p_obj i (Obj.field c_obj i)
            done
        | _ ->
            error
              "UPDATE_REC: type mismatch between placeholder and computed value");
        loop ()
    | Bytecode.HALT ->
        if vm.current_fiber.fiber_sp > 0 then peek vm else Bytecode.VUnit
  and dispatch_call fn_val arg is_tail =
    let finish result =
      if is_tail then
        match complete_frame result with
        | FrameLoop -> loop ()
        | FrameReturn v -> v
      else begin
        push vm result;
        loop ()
      end
    in
    match fn_val with
    | Bytecode.VClosure cls when cls.fn_proto.arity = 1 ->
        enter_frame vm cls arg is_tail;
        loop ()
    | Bytecode.VClosure cls -> finish (Bytecode.VPartial (cls, [ arg ]))
    | Bytecode.VPartial (cls, args) ->
        let new_args = args @ [ arg ] in
        if List.length new_args = cls.fn_proto.arity then begin
          enter_frame_args vm cls new_args is_tail;
          loop ()
        end
        else finish (Bytecode.VPartial (cls, new_args))
    | Bytecode.VExternal ext ->
        let new_args = ext.ext_args @ [ arg ] in
        let result =
          if List.length new_args = ext.ext_arity then ext.ext_fn new_args
          else Bytecode.VExternal { ext with ext_args = new_args }
        in
        finish result
    | _ ->
        error
          (Printf.sprintf "expected function, got %s" (Bytecode.pp_value fn_val))
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
  try loop ()
  with Runtime_error msg ->
    let line = lookup_line () in
    let trace = Buffer.create 256 in
    List.iteri
      (fun i fr ->
        let fip = fr.Bytecode.frame_ip in
        let fname = fr.frame_closure.fn_proto.name in
        let fline =
          if
            fip > 0
            && fip - 1 < Array.length fr.frame_closure.fn_proto.line_table
          then fr.frame_closure.fn_proto.line_table.(fip - 1)
          else 0
        in
        Buffer.add_string trace
          (Printf.sprintf "\n  frame[%d]: %s ip=%d line=%d" i fname fip fline))
      vm.current_fiber.fiber_frames;
    let full_msg =
      if line > 0 then
        Printf.sprintf "[line %d] %s\nCall stack:%s" line msg
          (Buffer.contents trace)
      else Printf.sprintf "%s\nCall stack:%s" msg (Buffer.contents trace)
    in
    raise (Runtime_error full_msg)

let execute_with_globals program (globals : (int, Bytecode.value) Hashtbl.t) =
  let main_fiber = make_fiber () in
  let main_closure =
    Bytecode.{ fn_proto = program.Bytecode.main; upvalues = [||] }
  in
  let num_locals = program.main.num_locals in
  ensure_fiber_capacity main_fiber num_locals;
  Array.fill main_fiber.fiber_stack 0 num_locals Bytecode.VUnit;
  main_fiber.fiber_sp <- num_locals;
  let main_frame =
    Bytecode.{ frame_closure = main_closure; frame_ip = 0; frame_base_sp = 0 }
  in
  main_fiber.fiber_frames <- [ main_frame ];
  main_fiber.fiber_frame_depth <- 1;
  let vm =
    {
      current_fiber = main_fiber;
      handler_stack = [];
      control_stack = [];
      return_stack = [];
      provide_resumes = [];
      globals;
      global_names = program.global_names;
    }
  in
  run vm

let execute program = execute_with_globals program (Hashtbl.create 64)
