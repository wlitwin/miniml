(** LLVM IR text emitter.

    Provides a buffer-based API for building well-formed LLVM IR text files.
    Manages SSA register numbering automatically. *)

type t = {
  buf: Buffer.t;
  mutable next_reg: int;
}

let create () =
  { buf = Buffer.create 4096; next_reg = 0 }

let fresh_reg t =
  let n = t.next_reg in
  t.next_reg <- n + 1;
  Printf.sprintf "%%%d" n

let reset_regs t =
  t.next_reg <- 0

let contents t =
  Buffer.contents t.buf

(* ---- Raw emission ---- *)

let emit_raw t s =
  Buffer.add_string t.buf s;
  Buffer.add_char t.buf '\n'

let emit_blank t =
  Buffer.add_char t.buf '\n'

(* ---- Module-level declarations ---- *)

let emit_target_triple t triple =
  Printf.bprintf t.buf "target triple = \"%s\"\n" triple

let emit_declare t ~ret_ty ~name ~param_tys =
  Printf.bprintf t.buf "declare %s @%s(%s)\n"
    ret_ty name (String.concat ", " param_tys)

let emit_define_start t ~ret_ty ~name ~params =
  let param_str = String.concat ", "
    (List.map (fun (ty, reg) -> Printf.sprintf "%s %s" ty reg) params)
  in
  Printf.bprintf t.buf "define %s @%s(%s) {\n" ret_ty name param_str;
  reset_regs t

let emit_define_end t =
  Buffer.add_string t.buf "}\n"

(* ---- Labels ---- *)

let emit_label t name =
  Printf.bprintf t.buf "%s:\n" name

(* ---- Instructions ---- *)

let emit_ret t ty value =
  Printf.bprintf t.buf "  ret %s %s\n" ty value

let emit_ret_void t =
  Buffer.add_string t.buf "  ret void\n"

let emit_call t ~ret_ty ~name ~args =
  let reg = fresh_reg t in
  let args_str = String.concat ", "
    (List.map (fun (ty, v) -> Printf.sprintf "%s %s" ty v) args)
  in
  Printf.bprintf t.buf "  %s = call %s @%s(%s)\n" reg ret_ty name args_str;
  reg

let emit_call_void t ~name ~args =
  let args_str = String.concat ", "
    (List.map (fun (ty, v) -> Printf.sprintf "%s %s" ty v) args)
  in
  Printf.bprintf t.buf "  call void @%s(%s)\n" name args_str

let emit_binop t ~op ~ty ~lhs ~rhs =
  let reg = fresh_reg t in
  Printf.bprintf t.buf "  %s = %s %s %s, %s\n" reg op ty lhs rhs;
  reg

let emit_alloca t ~ty =
  let reg = fresh_reg t in
  Printf.bprintf t.buf "  %s = alloca %s\n" reg ty;
  reg

let emit_alloca_array t ~ty ~count =
  let reg = fresh_reg t in
  Printf.bprintf t.buf "  %s = alloca %s, i32 %d\n" reg ty count;
  reg

let emit_store t ~ty ~value ~ptr =
  Printf.bprintf t.buf "  store %s %s, ptr %s\n" ty value ptr

let emit_load t ~ty ~ptr =
  let reg = fresh_reg t in
  Printf.bprintf t.buf "  %s = load %s, ptr %s\n" reg ty ptr;
  reg

let emit_br t ~label =
  Printf.bprintf t.buf "  br label %%%s\n" label

let emit_condbr t ~cond ~if_true ~if_false =
  Printf.bprintf t.buf "  br i1 %s, label %%%s, label %%%s\n"
    cond if_true if_false

let emit_icmp t ~cmp ~ty ~lhs ~rhs =
  let reg = fresh_reg t in
  Printf.bprintf t.buf "  %s = icmp %s %s %s, %s\n" reg cmp ty lhs rhs;
  reg

let emit_phi t ~ty ~incoming =
  let reg = fresh_reg t in
  let pairs = String.concat ", "
    (List.map (fun (v, label) -> Printf.sprintf "[ %s, %%%s ]" v label) incoming)
  in
  Printf.bprintf t.buf "  %s = phi %s %s\n" reg ty pairs;
  reg

let emit_comment t msg =
  Printf.bprintf t.buf "  ; %s\n" msg

let emit_global_constant t ~name ~ty ~value =
  Printf.bprintf t.buf "@%s = global %s %s\n" name ty value

(* ---- Phase 1 additions ---- *)

let emit_select t ~cond ~ty ~if_true ~if_false =
  let reg = fresh_reg t in
  Printf.bprintf t.buf "  %s = select i1 %s, %s %s, %s %s\n"
    reg cond ty if_true ty if_false;
  reg

let emit_zext t ~src_ty ~value ~dst_ty =
  let reg = fresh_reg t in
  Printf.bprintf t.buf "  %s = zext %s %s to %s\n" reg src_ty value dst_ty;
  reg

let emit_ptrtoint t ~value =
  let reg = fresh_reg t in
  Printf.bprintf t.buf "  %s = ptrtoint ptr %s to i64\n" reg value;
  reg

let emit_unreachable t =
  Buffer.add_string t.buf "  unreachable\n"

let emit_fcmp t ~cmp ~lhs ~rhs =
  let reg = fresh_reg t in
  Printf.bprintf t.buf "  %s = fcmp %s double %s, %s\n" reg cmp lhs rhs;
  reg

let emit_fneg t ~value =
  let reg = fresh_reg t in
  Printf.bprintf t.buf "  %s = fneg double %s\n" reg value;
  reg

let emit_float_binop t ~op ~lhs ~rhs =
  let reg = fresh_reg t in
  Printf.bprintf t.buf "  %s = %s double %s, %s\n" reg op lhs rhs;
  reg

let emit_sitofp t ~value =
  let reg = fresh_reg t in
  Printf.bprintf t.buf "  %s = sitofp i64 %s to double\n" reg value;
  reg

let emit_fptosi t ~value =
  let reg = fresh_reg t in
  Printf.bprintf t.buf "  %s = fptosi double %s to i64\n" reg value;
  reg

let emit_global_string_constant t ~name ~len ~value =
  Printf.bprintf t.buf "@%s = private unnamed_addr constant [%d x i8] c\"%s\"\n"
    name len value

let emit_global_float_constant t ~name ~value =
  Printf.bprintf t.buf "@%s = private unnamed_addr constant double %s\n"
    name value

(* ---- Phase 2 additions ---- *)

let emit_inttoptr t ~value =
  let reg = fresh_reg t in
  Printf.bprintf t.buf "  %s = inttoptr i64 %s to ptr\n" reg value;
  reg

let emit_gep t ~ty ~ptr ~index =
  let reg = fresh_reg t in
  Printf.bprintf t.buf "  %s = getelementptr %s, ptr %s, i32 %d\n" reg ty ptr index;
  reg

let emit_gep_dynamic t ~ty ~ptr ~index =
  let reg = fresh_reg t in
  Printf.bprintf t.buf "  %s = getelementptr %s, ptr %s, i64 %s\n" reg ty ptr index;
  reg

let emit_switch t ~value ~default ~cases =
  Printf.bprintf t.buf "  switch i64 %s, label %%%s [\n" value default;
  List.iter (fun (v, label) ->
    Printf.bprintf t.buf "    i64 %d, label %%%s\n" v label
  ) cases;
  Buffer.add_string t.buf "  ]\n"
