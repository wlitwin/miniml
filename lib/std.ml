(* Standard library modules for the interpreter.
   Each module is opt-in via its register_* function.
   register_all loads everything. *)

open Bytecode

let arg n args = List.nth args n

let some v = VVariant (1, "Some", Some v)
let none = VVariant (0, "None", None)

(* ---- String module ---- *)

let register_string state =
  let state = Interp.register_fns state "String" [
    ("length", 1, fun args ->
      VInt (String.length (Interp.as_string (arg 0 args))));

    ("sub", 3, fun args ->
      let s = Interp.as_string (arg 0 args) in
      let start = Interp.as_int (arg 1 args) in
      let len = Interp.as_int (arg 2 args) in
      if start < 0 || len < 0 || start + len > String.length s then
        raise (Vm.Runtime_error "String.sub: index out of bounds");
      VString (String.sub s start len));

    ("split", 2, fun args ->
      let delim = Interp.as_string (arg 0 args) in
      let input = Interp.as_string (arg 1 args) in
      if String.length delim = 0 then
        (* Split into individual characters *)
        VList (List.init (String.length input) (fun i ->
          VString (String.make 1 input.[i])))
      else begin
        let dlen = String.length delim in
        let slen = String.length input in
        let parts = ref [] in
        let start = ref 0 in
        let i = ref 0 in
        while !i <= slen - dlen do
          if String.sub input !i dlen = delim then begin
            parts := VString (String.sub input !start (!i - !start)) :: !parts;
            start := !i + dlen;
            i := !i + dlen
          end else
            incr i
        done;
        parts := VString (String.sub input !start (slen - !start)) :: !parts;
        VList (List.rev !parts)
      end);

    ("trim", 1, fun args ->
      VString (String.trim (Interp.as_string (arg 0 args))));

    ("starts_with", 2, fun args ->
      let prefix = Interp.as_string (arg 0 args) in
      let s = Interp.as_string (arg 1 args) in
      let plen = String.length prefix in
      VBool (String.length s >= plen && String.sub s 0 plen = prefix));

    ("contains", 2, fun args ->
      let sub = Interp.as_string (arg 0 args) in
      let s = Interp.as_string (arg 1 args) in
      let sublen = String.length sub in
      let slen = String.length s in
      if sublen = 0 then VBool true
      else begin
        let found = ref false in
        let i = ref 0 in
        while !i <= slen - sublen && not !found do
          if String.sub s !i sublen = sub then found := true
          else incr i
        done;
        VBool !found
      end);

    ("replace", 3, fun args ->
      let old_s = Interp.as_string (arg 0 args) in
      let new_s = Interp.as_string (arg 1 args) in
      let input = Interp.as_string (arg 2 args) in
      if String.length old_s = 0 then VString input
      else begin
        let olen = String.length old_s in
        let buf = Buffer.create (String.length input) in
        let i = ref 0 in
        let slen = String.length input in
        while !i < slen do
          if !i <= slen - olen && String.sub input !i olen = old_s then begin
            Buffer.add_string buf new_s;
            i := !i + olen
          end else begin
            Buffer.add_char buf input.[!i];
            incr i
          end
        done;
        VString (Buffer.contents buf)
      end);

    ("to_int", 1, fun args ->
      let s = Interp.as_string (arg 0 args) in
      (match int_of_string_opt s with
       | Some n -> some (VInt n)
       | None -> none));

    ("to_float", 1, fun args ->
      let s = Interp.as_string (arg 0 args) in
      (match float_of_string_opt s with
       | Some f -> some (VFloat f)
       | None -> none));

    ("uppercase", 1, fun args ->
      VString (String.uppercase_ascii (Interp.as_string (arg 0 args))));

    ("lowercase", 1, fun args ->
      VString (String.lowercase_ascii (Interp.as_string (arg 0 args))));

    ("get", 2, fun args ->
      let s = Interp.as_string (arg 0 args) in
      let i = Interp.as_int (arg 1 args) in
      if i < 0 || i >= String.length s then
        raise (Vm.Runtime_error (Printf.sprintf "String.get: index %d out of bounds (length %d)" i (String.length s)));
      VByte (Char.code s.[i]));

    ("to_bytes", 1, fun args ->
      let s = Interp.as_string (arg 0 args) in
      VList (List.init (String.length s) (fun i -> VByte (Char.code s.[i]))));

    ("of_bytes", 1, fun args ->
      let bytes = match arg 0 args with Bytecode.VList l -> l | _ -> raise (Vm.Runtime_error "expected list") in
      let buf = Buffer.create (List.length bytes) in
      List.iter (fun b -> Buffer.add_char buf (Char.chr (Interp.as_byte b))) bytes;
      VString (Buffer.contents buf));

    ("to_byte_array", 1, fun args ->
      let s = Interp.as_string (arg 0 args) in
      VArray (Array.init (String.length s) (fun i -> Bytecode.VByte (Char.code s.[i]))));

    ("of_byte_array", 1, fun args ->
      let arr = Interp.as_array (arg 0 args) in
      let buf = Buffer.create (Array.length arr) in
      Array.iter (fun b -> Buffer.add_char buf (Char.chr (Interp.as_byte b))) arr;
      VString (Buffer.contents buf));

    ("to_runes", 1, fun args ->
      let s = Interp.as_string (arg 0 args) in
      let runes = ref [] in
      let i = ref 0 in
      let len = String.length s in
      while !i < len do
        let b = Char.code s.[!i] in
        let (cp, nbytes) =
          if b < 0x80 then (b, 1)
          else if b land 0xE0 = 0xC0 then
            ((b land 0x1F) lsl 6 lor (Char.code s.[!i+1] land 0x3F), 2)
          else if b land 0xF0 = 0xE0 then
            ((b land 0x0F) lsl 12 lor (Char.code s.[!i+1] land 0x3F) lsl 6
             lor (Char.code s.[!i+2] land 0x3F), 3)
          else
            ((b land 0x07) lsl 18 lor (Char.code s.[!i+1] land 0x3F) lsl 12
             lor (Char.code s.[!i+2] land 0x3F) lsl 6
             lor (Char.code s.[!i+3] land 0x3F), 4)
        in
        runes := VRune cp :: !runes;
        i := !i + nbytes
      done;
      VList (List.rev !runes));

    ("of_runes", 1, fun args ->
      let runes = match arg 0 args with Bytecode.VList l -> l | _ -> raise (Vm.Runtime_error "expected list") in
      let buf = Buffer.create (List.length runes) in
      List.iter (fun r ->
        let cp = Interp.as_rune r in
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
        end
      ) runes;
      VString (Buffer.contents buf));

    ("get_rune", 2, fun args ->
      let s = Interp.as_string (arg 0 args) in
      let n = Interp.as_int (arg 1 args) in
      let len = String.length s in
      let i = ref 0 in
      let count = ref 0 in
      while !i < len && !count < n do
        let b = Char.code s.[!i] in
        let nbytes =
          if b < 0x80 then 1
          else if b land 0xE0 = 0xC0 then 2
          else if b land 0xF0 = 0xE0 then 3
          else 4
        in
        i := !i + nbytes;
        incr count
      done;
      if !i >= len then
        raise (Vm.Runtime_error (Printf.sprintf "String.get_rune: index %d out of bounds" n));
      let b = Char.code s.[!i] in
      let cp =
        if b < 0x80 then b
        else if b land 0xE0 = 0xC0 then
          (b land 0x1F) lsl 6 lor (Char.code s.[!i+1] land 0x3F)
        else if b land 0xF0 = 0xE0 then
          (b land 0x0F) lsl 12 lor (Char.code s.[!i+1] land 0x3F) lsl 6
          lor (Char.code s.[!i+2] land 0x3F)
        else
          (b land 0x07) lsl 18 lor (Char.code s.[!i+1] land 0x3F) lsl 12
          lor (Char.code s.[!i+2] land 0x3F) lsl 6
          lor (Char.code s.[!i+3] land 0x3F)
      in
      VRune cp);

    ("of_byte", 1, fun args ->
      let b = Interp.as_byte (arg 0 args) in
      VString (String.make 1 (Char.chr b)));

    ("rune_length", 1, fun args ->
      let s = Interp.as_string (arg 0 args) in
      let len = String.length s in
      let i = ref 0 in
      let count = ref 0 in
      while !i < len do
        let b = Char.code s.[!i] in
        let nbytes =
          if b < 0x80 then 1
          else if b land 0xE0 = 0xC0 then 2
          else if b land 0xF0 = 0xE0 then 3
          else 4
        in
        i := !i + nbytes;
        incr count
      done;
      VInt !count);

    ("make", 2, fun args ->
      let n = Interp.as_int (arg 0 args) in
      let b = Interp.as_byte (arg 1 args) in
      if n < 0 then raise (Vm.Runtime_error "String.make: negative length");
      VString (String.make n (Char.chr b)));

    ("index_opt", 2, fun args ->
      let s = Interp.as_string (arg 0 args) in
      let b = Interp.as_byte (arg 1 args) in
      let c = Char.chr b in
      (match String.index_opt s c with
       | Some i -> some (VInt i)
       | None -> none));

    ("rindex_opt", 2, fun args ->
      let s = Interp.as_string (arg 0 args) in
      let b = Interp.as_byte (arg 1 args) in
      let c = Char.chr b in
      (match String.rindex_opt s c with
       | Some i -> some (VInt i)
       | None -> none));

    ("concat", 2, fun args ->
      let sep = Interp.as_string (arg 0 args) in
      let lst = match arg 1 args with Bytecode.VList l -> l | _ -> raise (Vm.Runtime_error "expected list") in
      let strs = List.map (fun v -> Interp.as_string v) lst in
      VString (String.concat sep strs));

    ("compare", 2, fun args ->
      let a = Interp.as_string (arg 0 args) in
      let b = Interp.as_string (arg 1 args) in
      VInt (String.compare a b));
  ] in
  let state = Interp.eval_setup state {|
    module String =
      pub extern length : string -> int
      pub extern sub : string -> int -> int -> string
      pub extern split : string -> string -> string list
      pub extern trim : string -> string
      pub extern starts_with : string -> string -> bool
      pub extern contains : string -> string -> bool
      pub extern replace : string -> string -> string -> string
      pub extern to_int : string -> int option
      pub extern to_float : string -> float option
      pub extern uppercase : string -> string
      pub extern lowercase : string -> string
      pub extern get : string -> int -> byte
      pub extern to_bytes : string -> byte list
      pub extern of_bytes : byte list -> string
      pub extern to_byte_array : string -> byte array
      pub extern of_byte_array : byte array -> string
      pub extern to_runes : string -> rune list
      pub extern of_runes : rune list -> string
      pub extern get_rune : string -> int -> rune
      pub extern of_byte : byte -> string
      pub extern rune_length : string -> int
      pub extern make : int -> byte -> string
      pub extern index_opt : string -> byte -> int option
      pub extern rindex_opt : string -> byte -> int option
      pub extern concat : string -> string list -> string
      pub extern compare : string -> string -> int
    end
  |} in
  Interp.eval_setup state
    "module String = pub let iter f s = let n = String.length s in let rec go i = if i >= n do () else (f (String.get s i); go (i + 1)) in go 0 end"

(* ---- Byte module ---- *)

let register_byte state =
  Interp.eval_setup state Stdlib_sources.byte

(* ---- Rune module ---- *)

let register_rune state =
  Interp.eval_setup state Stdlib_sources.rune

(* ---- Set module ---- *)

let register_set state =
  Interp.eval_setup state Stdlib_sources.set

(* ---- Enum module ---- *)

let register_enum state =
  Interp.eval_setup state Stdlib_sources.enum

(* ---- Seq module ---- *)

let register_seq state =
  Interp.eval_setup state Stdlib_sources.seq

(* ---- IO module ---- *)

let register_io state =
  let state = Interp.register_fns state "IO" [
    ("read_file", 1, fun args ->
      let path = Interp.as_string (arg 0 args) in
      (try
        let ic = open_in path in
        let contents = In_channel.input_all ic in
        close_in ic;
        VString contents
      with Sys_error msg ->
        raise (Vm.Runtime_error ("IO.read_file: " ^ msg))));

    ("write_file", 2, fun args ->
      let path = Interp.as_string (arg 0 args) in
      let contents = Interp.as_string (arg 1 args) in
      (try
        let oc = open_out path in
        output_string oc contents;
        close_out oc;
        VUnit
      with Sys_error msg ->
        raise (Vm.Runtime_error ("IO.write_file: " ^ msg))));

    ("append_file", 2, fun args ->
      let path = Interp.as_string (arg 0 args) in
      let contents = Interp.as_string (arg 1 args) in
      (try
        let oc = open_out_gen [Open_append; Open_creat] 0o644 path in
        output_string oc contents;
        close_out oc;
        VUnit
      with Sys_error msg ->
        raise (Vm.Runtime_error ("IO.append_file: " ^ msg))));

    ("read_line", 1, fun _args ->
      (try VString (input_line stdin)
       with End_of_file -> VString ""));

    ("file_exists", 1, fun args ->
      VBool (Sys.file_exists (Interp.as_string (arg 0 args))));
  ] in
  Interp.eval_setup state {|
    module IO =
      pub extern read_file : string -> string
      pub extern write_file : string -> string -> unit
      pub extern append_file : string -> string -> unit
      pub extern read_line : unit -> string
      pub extern file_exists : string -> bool
    end
  |}

(* ---- Sys module ---- *)

let register_sys state =
  let state = Interp.register_fns state "Sys" [
    ("args", 1, fun _args ->
      VList (Array.to_list (Array.map (fun s -> VString s) !Interp.script_argv)));

    ("getenv", 1, fun args ->
      let name = Interp.as_string (arg 0 args) in
      (match Sys.getenv_opt name with
       | Some v -> some (VString v)
       | None -> none));

    ("exit", 1, fun args ->
      exit (Interp.as_int (arg 0 args)));

    ("time", 1, fun _args ->
      VFloat (Unix.gettimeofday ()));
  ] in
  Interp.eval_setup state {|
    module Sys =
      pub extern args : unit -> string list
      pub extern getenv : string -> string option
      pub extern exit : int -> unit
      pub extern time : unit -> float
    end
  |}

(* ---- Math module ---- *)

let register_math state =
  Interp.eval_setup state Stdlib_sources.math

(* ---- List module ---- *)

let register_list state =
  Interp.eval_setup state Stdlib_sources.list

(* ---- Array module ---- *)

let register_array state =
  let state = Interp.register_fns state "Array" [
    ("make", 2, fun args ->
      let n = Interp.as_int (arg 0 args) in
      VArray (Array.make n (arg 1 args)));

    ("get", 2, fun args ->
      let arr = Interp.as_array (arg 0 args) in
      let idx = Interp.as_int (arg 1 args) in
      if idx < 0 || idx >= Array.length arr then
        raise (Vm.Runtime_error (Printf.sprintf "Array.get: index %d out of bounds (length %d)" idx (Array.length arr)));
      arr.(idx));

    ("set", 3, fun args ->
      let arr = Interp.as_array (arg 0 args) in
      let idx = Interp.as_int (arg 1 args) in
      let v = arg 2 args in
      if idx < 0 || idx >= Array.length arr then
        raise (Vm.Runtime_error (Printf.sprintf "Array.set: index %d out of bounds (length %d)" idx (Array.length arr)));
      arr.(idx) <- v; VUnit);

    ("length", 1, fun args ->
      VInt (Array.length (Interp.as_array (arg 0 args))));

    ("to_list", 1, fun args ->
      VList (Array.to_list (Interp.as_array (arg 0 args))));

    ("of_list", 1, fun args ->
      match arg 0 args with
      | VList l -> VArray (Array.of_list l)
      | _ -> raise (Vm.Runtime_error "Array.of_list: expected list"));

    ("copy", 1, fun args ->
      VArray (Array.copy (Interp.as_array (arg 0 args))));

    ("sub", 3, fun args ->
      let arr = Interp.as_array (arg 0 args) in
      let start = Interp.as_int (arg 1 args) in
      let len = Interp.as_int (arg 2 args) in
      if start < 0 || len < 0 || start + len > Array.length arr then
        raise (Vm.Runtime_error "Array.sub: index out of bounds");
      VArray (Array.sub arr start len));
  ] in
  Interp.eval_setup state {|
    module Array =
      pub extern make : int -> 'a -> 'a array
      pub extern get : 'a array -> int -> 'a
      pub extern set : 'a array -> int -> 'a -> unit
      pub extern length : 'a array -> int
      pub extern to_list : 'a array -> 'a list
      pub extern of_list : 'a list -> 'a array
      pub extern copy : 'a array -> 'a array
      pub extern sub : 'a array -> int -> int -> 'a array
    end
  |}

let register_array_extra state =
  Interp.eval_setup state Stdlib_sources.array_extra

(* ---- Result module (defined in source) ---- *)


let register_result state =
  Interp.eval_setup state Stdlib_sources.result

(* ---- Option module (defined in source) ---- *)

let register_option state =
  Interp.eval_setup state Stdlib_sources.option

(* ---- Buffer module (defined in source) ---- *)

let register_buffer state =
  Interp.eval_setup state Stdlib_sources.buffer

(* ---- Fmt module (defined in source) ---- *)

let register_fmt state =
  Interp.eval_setup state Stdlib_sources.fmt

(* ---- Hashtbl module (defined in source) ---- *)

let register_hashtbl state =
  let state = Interp.eval_setup state Stdlib_sources.hash in
  Interp.eval_setup state Stdlib_sources.hashtbl

(* ---- Eval module ---- *)

let register_eval state =
  Interp.eval_state := Some state;
  let state = Interp.register_fns state "Runtime" [
    ("eval", 1, fun args ->
      let source = Interp.as_string (arg 0 args) in
      let _ = Interp.eval_source source in
      VUnit);

    ("eval_file", 1, fun args ->
      let path = Interp.as_string (arg 0 args) in
      (try
        let ic = open_in path in
        let source = In_channel.input_all ic in
        close_in ic;
        let _ = Interp.eval_source source in
        VUnit
      with Sys_error msg ->
        raise (Vm.Runtime_error ("Runtime.eval_file: " ^ msg))));
  ] in
  Interp.eval_setup state {|
    module Runtime =
      pub extern eval : string -> unit
      pub extern eval_file : string -> unit
    end
  |}

(* ---- Ref module (defined in source) ---- *)

let register_ref state =
  Interp.eval_setup state Stdlib_sources.ref

(* ---- Dynarray module (defined in source) ---- *)

let register_dynarray state =
  Interp.eval_setup state Stdlib_sources.dynarray

(* ---- Compat (int_of_string, float_of_string) ---- *)

let register_compat state =
  Interp.eval_setup state Stdlib_sources.compat

(* ---- Register all ---- *)

let register_all state =
  let state =
    state
    |> register_string
    |> register_list
    |> register_array
    |> register_array_extra
    |> register_io
    |> register_sys
    |> register_math
    |> register_result
    |> register_byte
    |> register_rune
    |> register_set
    |> register_enum
    |> register_seq
    |> register_option
    |> register_buffer
    |> register_fmt
    |> register_hashtbl
    |> register_ref
    |> register_dynarray
    |> register_compat
  in
  let state = register_eval state in
  Interp.eval_state := Some state;
  Types.pp_synonyms := state.Interp.ctx.Typechecker.type_env.Types.type_synonyms;
  state
