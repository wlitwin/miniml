(* Bytecode peephole optimizer *)

(* Action for each instruction during the mark pass *)
type action =
  | Keep
  | Remove
  | ReplaceWith of Bytecode.opcode list

(* Collect the set of all jump target offsets in a code array *)
let collect_jump_targets code =
  let targets = Hashtbl.create 64 in
  Array.iter (fun op ->
    match op with
    | Bytecode.JUMP t | Bytecode.JUMP_IF_FALSE t | Bytecode.JUMP_IF_TRUE t
    | Bytecode.ENTER_LOOP t | Bytecode.LOOP_CONTINUE t ->
      Hashtbl.replace targets t ()
    | _ -> ()
  ) code;
  targets

(* Check if an offset is a jump target *)
let is_jump_target targets offset =
  Hashtbl.mem targets offset

(* === Optimization 1: Dead code after TAIL_CALL === *)
let mark_dead_after_tail_call code targets actions =
  let len = Array.length code in
  let i = ref 0 in
  while !i < len do
    (match code.(!i) with
     | Bytecode.TAIL_CALL _ | Bytecode.TAIL_CALL_N _ ->
       let j = ref (!i + 1) in
       while !j < len && not (is_jump_target targets !j) do
         actions.(!j) <- Remove;
         incr j
       done;
       i := !j
     | _ -> incr i)
  done

(* === Optimization 2: JUMP to next instruction === *)
let mark_jump_to_next code actions =
  let len = Array.length code in
  for i = 0 to len - 1 do
    match code.(i) with
    | Bytecode.JUMP target when target = i + 1 ->
      actions.(i) <- Remove
    | _ -> ()
  done

(* === Optimization 4: SET_LOCAL n; GET_LOCAL n pair elimination === *)

(* Does this opcode read local slot n? *)
let reads_slot op n =
  match op with
  | Bytecode.GET_LOCAL s | Bytecode.GET_LOCAL_CALL (s, _)
  | Bytecode.GET_LOCAL_TUPLE_GET (s, _) | Bytecode.GET_LOCAL_FIELD (s, _) -> s = n
  | Bytecode.CLOSURE (_, caps) | Bytecode.CLOSURE_REC (_, caps, _) ->
    List.exists (fun c -> c = Bytecode.CaptureLocal n) caps
  | _ -> false

(* Does this opcode overwrite local slot n? *)
let writes_slot op n =
  match op with
  | Bytecode.SET_LOCAL s -> s = n
  | _ -> false

(* Conservative forward scan: is slot n dead (never read before overwritten)?
   Returns true only when we can prove n is dead on the straight-line path.
   Bails out conservatively (returns false) at any control flow. *)
let is_slot_dead code actions targets n start =
  let len = Array.length code in
  let i = ref start in
  let result = ref true in
  let done_ = ref false in
  while !i < len && not !done_ do
    if actions.(!i) = Remove then incr i
    else if is_jump_target targets !i then
      (result := false; done_ := true)
    else begin
      let op = code.(!i) in
      if reads_slot op n then (result := false; done_ := true)
      else if writes_slot op n then done_ := true
      else begin
        match op with
        (* Terminators: function ends, slot is guaranteed dead *)
        | Bytecode.RETURN | Bytecode.FUNC_RETURN | Bytecode.TAIL_CALL _ | Bytecode.TAIL_CALL_N _
        | Bytecode.MATCH_FAIL _ | Bytecode.HALT ->
          done_ := true (* result stays true: slot is dead *)
        (* Branches and loops: might reach code that reads the slot *)
        | Bytecode.JUMP _ | Bytecode.JUMP_IF_FALSE _ | Bytecode.JUMP_IF_TRUE _
        | Bytecode.LOOP_BREAK | Bytecode.LOOP_CONTINUE _ | Bytecode.FOLD_CONTINUE _
        | Bytecode.ENTER_LOOP _ | Bytecode.JUMP_TABLE _ ->
          result := false; done_ := true
        | _ -> incr i
      end
    end
  done;
  !result

let mark_set_get_pairs code targets actions =
  let len = Array.length code in
  let i = ref 0 in
  while !i < len - 1 do
    if actions.(!i) = Keep && actions.(!i + 1) = Keep
       && not (is_jump_target targets (!i + 1)) then begin
      match code.(!i), code.(!i + 1) with
      | Bytecode.SET_LOCAL n, Bytecode.GET_LOCAL m when n = m ->
        if is_slot_dead code actions targets n (!i + 2) then begin
          (* Dead slot: remove both, value stays on stack *)
          actions.(!i) <- Remove;
          actions.(!i + 1) <- Remove;
          i := !i + 2
        end else begin
          (* Live slot: rewrite to DUP; SET_LOCAL n *)
          actions.(!i) <- ReplaceWith [Bytecode.DUP; Bytecode.SET_LOCAL n];
          actions.(!i + 1) <- Remove;
          i := !i + 2
        end
      | _ -> incr i
    end else
      incr i
  done

(* === Optimization 3: Dead store elimination === *)
let mark_dead_stores code targets actions =
  let len = Array.length code in
  let i = ref 0 in
  while !i < len do
    let handled = ref false in
    (* Pattern A: DUP; SET_LOCAL n where slot n is dead → Remove both *)
    if !i + 1 < len
       && actions.(!i) = Keep && actions.(!i + 1) = Keep
       && not (is_jump_target targets (!i + 1))
    then begin
      match code.(!i), code.(!i + 1) with
      | Bytecode.DUP, Bytecode.SET_LOCAL n
        when is_slot_dead code actions targets n (!i + 2) ->
        actions.(!i) <- Remove;
        actions.(!i + 1) <- Remove;
        i := !i + 2;
        handled := true
      | _ -> ()
    end;
    (* Pattern B: standalone SET_LOCAL n where slot n is dead → POP *)
    if not !handled then begin
      match code.(!i) with
      | Bytecode.SET_LOCAL n
        when actions.(!i) = Keep
             && is_slot_dead code actions targets n (!i + 1) ->
        actions.(!i) <- ReplaceWith [Bytecode.POP];
        incr i
      | _ -> incr i
    end
  done

(* === Optimization 5: Superinstructions === *)
let mark_superinstructions code targets actions =
  let len = Array.length code in
  let i = ref 0 in
  while !i < len do
    if !i + 1 < len && actions.(!i) = Keep && actions.(!i + 1) = Keep
       && not (is_jump_target targets (!i + 1)) then begin
      match code.(!i), code.(!i + 1) with
      | Bytecode.GET_LOCAL slot, Bytecode.CALL arity ->
        actions.(!i) <- ReplaceWith [Bytecode.GET_LOCAL_CALL (slot, arity)];
        actions.(!i + 1) <- Remove;
        i := !i + 2
      | Bytecode.GET_LOCAL slot, Bytecode.TUPLE_GET idx ->
        actions.(!i) <- ReplaceWith [Bytecode.GET_LOCAL_TUPLE_GET (slot, idx)];
        actions.(!i + 1) <- Remove;
        i := !i + 2
      | Bytecode.GET_LOCAL slot, Bytecode.FIELD name ->
        actions.(!i) <- ReplaceWith [Bytecode.GET_LOCAL_FIELD (slot, name)];
        actions.(!i + 1) <- Remove;
        i := !i + 2
      | Bytecode.GET_GLOBAL idx, Bytecode.CALL arity ->
        actions.(!i) <- ReplaceWith [Bytecode.GET_GLOBAL_CALL (idx, arity)];
        actions.(!i + 1) <- Remove;
        i := !i + 2
      | Bytecode.GET_GLOBAL idx, Bytecode.FIELD name ->
        actions.(!i) <- ReplaceWith [Bytecode.GET_GLOBAL_FIELD (idx, name)];
        actions.(!i + 1) <- Remove;
        i := !i + 2
      | _ -> incr i
    end else
      incr i
  done

(* === Optimization 5: Jump tables for match expressions === *)

(* Detect a TAG_EQ chain starting at position i.
   Pattern: GET_LOCAL(s), TAG_EQ(t), JUMP_IF_FALSE(next)
   Returns list of (tag, arm_body_offset) and the final default target. *)
let detect_tag_chain code _targets start_pos scrut_slot =
  let len = Array.length code in
  let arms = ref [] in
  let pos = ref start_pos in
  let continue = ref true in
  let default_target = ref 0 in
  (* Check that only our chain jumps into chain positions *)
  let chain_positions = Hashtbl.create 16 in
  (* First pass: collect chain structure *)
  while !continue do
    if !pos + 2 < len
       && (match code.(!pos) with Bytecode.GET_LOCAL s when s = scrut_slot -> true | _ -> false)
       && (match code.(!pos + 1) with Bytecode.TAG_EQ _ -> true | _ -> false)
       && (match code.(!pos + 2) with Bytecode.JUMP_IF_FALSE t -> t > !pos | _ -> false)
    then begin
      let tag = match code.(!pos + 1) with Bytecode.TAG_EQ t -> t | _ -> assert false in
      let next = match code.(!pos + 2) with Bytecode.JUMP_IF_FALSE t -> t | _ -> assert false in
      let arm_body = !pos + 3 in
      arms := (tag, arm_body) :: !arms;
      Hashtbl.replace chain_positions !pos ();
      Hashtbl.replace chain_positions (!pos + 1) ();
      Hashtbl.replace chain_positions (!pos + 2) ();
      pos := next
    end else begin
      default_target := !pos;
      continue := false
    end
  done;
  let arms = List.rev !arms in
  (* Validate: no jump from outside the chain targets a chain position *)
  let valid = List.length arms >= 2 in
  let valid = valid && (
    let ok = ref true in
    Array.iteri (fun i op ->
      if !ok && not (Hashtbl.mem chain_positions i) then begin
        let check_target t =
          if Hashtbl.mem chain_positions t then ok := false
        in
        match op with
        | Bytecode.JUMP t | Bytecode.JUMP_IF_FALSE t | Bytecode.JUMP_IF_TRUE t
        | Bytecode.ENTER_LOOP t | Bytecode.LOOP_CONTINUE t -> check_target t
        | _ -> ()
      end
    ) code;
    !ok
  ) in
  (* Check density and uniqueness *)
  if valid && arms <> [] then begin
    let tags = List.map fst arms in
    (* Reject if any tag appears more than once (nested patterns/guards) *)
    let unique_tags = List.sort_uniq compare tags in
    if List.length unique_tags <> List.length tags then
      None
    else begin
      let min_tag = List.fold_left min (List.hd tags) tags in
      let max_tag = List.fold_left max (List.hd tags) tags in
      let range = max_tag - min_tag + 1 in
      if range <= 2 * List.length arms then
        Some (arms, min_tag, max_tag, !default_target)
      else
        None
    end
  end else
    None

let mark_jump_tables code targets actions =
  let len = Array.length code in
  let i = ref 0 in
  while !i < len do
    if !i + 2 < len && actions.(!i) = Keep then begin
      match code.(!i), code.(!i + 1), code.(!i + 2) with
      | Bytecode.GET_LOCAL scrut_slot, Bytecode.TAG_EQ _, Bytecode.JUMP_IF_FALSE _ ->
        (match detect_tag_chain code targets !i scrut_slot with
         | Some (arms, min_tag, max_tag, default_target) ->
           let range = max_tag - min_tag + 1 in
           let table = Array.make range default_target in
           List.iter (fun (tag, arm_body) ->
             table.(tag - min_tag) <- arm_body
           ) arms;
           (* Mark first GET_LOCAL as Keep (value needed on stack) *)
           (* Mark first TAG_EQ + JIF as ReplaceWith(JUMP_TABLE) *)
           actions.(!i + 1) <- ReplaceWith [Bytecode.JUMP_TABLE (min_tag, table, default_target)];
           actions.(!i + 2) <- Remove;
           (* Mark subsequent chain arms as Remove *)
           let remaining = List.tl arms in
           List.iter (fun (_, arm_body) ->
             let chain_start = arm_body - 3 in
             if chain_start >= 0 && chain_start < len then begin
               if actions.(chain_start) = Keep then actions.(chain_start) <- Remove;
               if chain_start + 1 < len && actions.(chain_start + 1) = Keep then
                 actions.(chain_start + 1) <- Remove;
               if chain_start + 2 < len && actions.(chain_start + 2) = Keep then
                 actions.(chain_start + 2) <- Remove
             end
           ) remaining;
           (* Skip past the entire chain *)
           let last_arm = List.nth arms (List.length arms - 1) in
           i := snd last_arm
         | None -> incr i)
      | _ -> incr i
    end else
      incr i
  done

(* === Optimization 7: Push/pop cancellation === *)
let mark_push_pop code targets actions =
  let len = Array.length code in
  let i = ref 0 in
  while !i < len - 1 do
    if actions.(!i) = Keep && actions.(!i + 1) = Keep
       && not (is_jump_target targets (!i + 1))
    then begin
      let is_pure_push = match code.(!i) with
        | Bytecode.CONST _ | Bytecode.GET_LOCAL _ | Bytecode.GET_GLOBAL _
        | Bytecode.GET_UPVALUE _ | Bytecode.NIL | Bytecode.DUP -> true
        | _ -> false
      in
      match code.(!i + 1) with
      | Bytecode.POP when is_pure_push ->
        actions.(!i) <- Remove;
        actions.(!i + 1) <- Remove;
        i := !i + 2
      | _ -> incr i
    end else
      incr i
  done

(* === Rewrite pass === *)

(* Build new code and line_table arrays from actions, computing offset mapping *)
let rewrite code line_table actions =
  let len = Array.length code in
  let new_code = ref [] in
  let new_lines = ref [] in
  let offset_map = Array.make (len + 1) 0 in
  let new_offset = ref 0 in
  for i = 0 to len - 1 do
    offset_map.(i) <- !new_offset;
    let line = if i < Array.length line_table then line_table.(i) else 0 in
    match actions.(i) with
    | Keep ->
      new_code := code.(i) :: !new_code;
      new_lines := line :: !new_lines;
      incr new_offset
    | Remove -> ()
    | ReplaceWith ops ->
      List.iter (fun op ->
        new_code := op :: !new_code;
        new_lines := line :: !new_lines;
        incr new_offset
      ) ops
  done;
  (* Map for offset len (one past end) *)
  offset_map.(len) <- !new_offset;
  let new_code = Array.of_list (List.rev !new_code) in
  let new_lines = Array.of_list (List.rev !new_lines) in
  (new_code, new_lines, offset_map)

(* === Fixup pass: remap all jump targets === *)
let fixup_jumps code offset_map =
  let map_len = Array.length offset_map in
  let remap t =
    if t >= 0 && t < map_len then offset_map.(t)
    else t (* leave unchanged if out of bounds *)
  in
  Array.iteri (fun i op ->
    let remapped = match op with
      | Bytecode.JUMP t -> Bytecode.JUMP (remap t)
      | Bytecode.JUMP_IF_FALSE t -> Bytecode.JUMP_IF_FALSE (remap t)
      | Bytecode.JUMP_IF_TRUE t -> Bytecode.JUMP_IF_TRUE (remap t)
      | Bytecode.ENTER_LOOP t -> Bytecode.ENTER_LOOP (remap t)
      | Bytecode.LOOP_CONTINUE t -> Bytecode.LOOP_CONTINUE (remap t)
      | Bytecode.JUMP_TABLE (min_tag, targets, default) ->
        let new_targets = Array.map (fun t -> remap t) targets in
        let new_default = remap default in
        Bytecode.JUMP_TABLE (min_tag, new_targets, new_default)
      | other -> other
    in
    code.(i) <- remapped
  ) code

(* === Post-rewrite pass: jump threading and JUMP→RETURN === *)
let thread_jumps code =
  let len = Array.length code in
  if len = 0 then ()
  else begin
    (* Follow a chain of unconditional JUMPs to find the final target *)
    let resolve_target target =
      let visited = Hashtbl.create 8 in
      let t = ref target in
      let continue = ref true in
      while !continue do
        if !t < 0 || !t >= len || Hashtbl.mem visited !t then
          continue := false
        else begin
          match code.(!t) with
          | Bytecode.JUMP next ->
            Hashtbl.replace visited !t ();
            t := next
          | _ -> continue := false
        end
      done;
      !t
    in
    (* Compute control depth for JUMP→RETURN safety.
       Only ENTER_LOOP/EXIT_LOOP affect the control_stack. *)
    let control_depth = Array.make len 0 in
    let depth = ref 0 in
    for i = 0 to len - 1 do
      control_depth.(i) <- !depth;
      (match code.(i) with
       | Bytecode.ENTER_LOOP _ -> incr depth
       | Bytecode.EXIT_LOOP -> if !depth > 0 then decr depth
       | _ -> ())
    done;
    (* Thread all jump instructions *)
    for i = 0 to len - 1 do
      match code.(i) with
      | Bytecode.JUMP target ->
        let final_target = resolve_target target in
        (* Check for JUMP→RETURN replacement (only at control depth 0) *)
        if final_target >= 0 && final_target < len
           && control_depth.(i) = 0 then begin
          match code.(final_target) with
          | Bytecode.RETURN ->
            code.(i) <- Bytecode.RETURN
          | _ ->
            if final_target <> target then
              code.(i) <- Bytecode.JUMP final_target
        end else begin
          if final_target <> target then
            code.(i) <- Bytecode.JUMP final_target
        end
      | Bytecode.JUMP_IF_FALSE target ->
        let final_target = resolve_target target in
        if final_target <> target then
          code.(i) <- Bytecode.JUMP_IF_FALSE final_target
      | Bytecode.JUMP_IF_TRUE target ->
        let final_target = resolve_target target in
        if final_target <> target then
          code.(i) <- Bytecode.JUMP_IF_TRUE final_target
      | Bytecode.JUMP_TABLE (min_tag, targets, default_target) ->
        let changed = ref false in
        let new_targets = Array.map (fun t ->
          let final_t = resolve_target t in
          if final_t <> t then changed := true;
          final_t
        ) targets in
        let new_default = resolve_target default_target in
        if new_default <> default_target then changed := true;
        if !changed then
          code.(i) <- Bytecode.JUMP_TABLE (min_tag, new_targets, new_default)
      | _ -> ()
    done
  end

(* === Main optimizer for a single prototype === *)
let optimize_code _constants code line_table =
  let len = Array.length code in
  if len = 0 then (code, line_table)
  else begin
    let targets = collect_jump_targets code in
    let actions = Array.make len Keep in
    (* Run mark passes *)
    mark_dead_after_tail_call code targets actions;
    mark_jump_to_next code actions;
    mark_dead_stores code targets actions;
    mark_set_get_pairs code targets actions;
    mark_jump_tables code targets actions;
    mark_push_pop code targets actions;
    mark_superinstructions code targets actions;
    (* Check if any mark actions were taken *)
    let any_changes = ref false in
    Array.iter (fun a -> if a <> Keep then any_changes := true) actions;
    let (final_code, final_lines) =
      if not !any_changes then (code, line_table)
      else begin
        let (new_code, new_lines, offset_map) = rewrite code line_table actions in
        fixup_jumps new_code offset_map;
        (new_code, new_lines)
      end
    in
    (* Post-rewrite: jump threading runs unconditionally *)
    thread_jumps final_code;
    (final_code, final_lines)
  end

(* Recursively optimize a prototype and all nested prototypes in its constants *)
let rec optimize_proto (proto : Bytecode.prototype) : Bytecode.prototype =
  (* First optimize nested prototypes in constants *)
  let new_constants = Array.map (fun v ->
    match v with
    | Bytecode.VProto p -> Bytecode.VProto (optimize_proto p)
    | other -> other
  ) proto.constants in
  (* Then optimize this prototype's code *)
  let (new_code, new_lines) = optimize_code new_constants proto.code proto.line_table in
  (* Debug: dump optimized code when a JUMP_TABLE exists *)
  { proto with
    code = new_code;
    constants = new_constants;
    line_table = new_lines;
  }
