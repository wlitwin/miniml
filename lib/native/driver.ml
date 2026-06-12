(** Native compilation driver.

    Orchestrates the full pipeline: source -> frontend -> codegen -> .ll ->
    clang -> binary. Also provides an --emit-ir mode that just prints the LLVM
    IR. *)

open Interpreter

exception Driver_error of string

let error msg = raise (Driver_error msg)

(* ---- Target triple detection (host glue) ----

   Lives in the driver, the host-interaction layer, so that codegen stays a pure
   IR generator that takes the triple as an input. *)

let run_cmd cmd =
  let ic = Unix.open_process_in cmd in
  let line = In_channel.input_line ic in
  ignore (Unix.close_process_in ic);
  match line with Some s -> String.trim s | None -> ""

let detect_target_triple () =
  let arch = match run_cmd "uname -m" with "" -> "x86_64" | s -> s in
  let os = match run_cmd "uname -s" with "" -> "Linux" | s -> s in
  match (os, arch) with
  | "Darwin", arch ->
      let ver =
        match run_cmd "sw_vers -productVersion" with "" -> "11.0" | s -> s
      in
      let major =
        match String.split_on_char '.' ver with v :: _ -> v | [] -> "11"
      in
      Printf.sprintf "%s-apple-macosx%s.0.0"
        (if arch = "arm64" then "arm64" else "x86_64")
        major
  | _, "aarch64" -> "aarch64-unknown-linux-gnu"
  | _, _ -> "x86_64-unknown-linux-gnu"

(* ---- Runtime path discovery ---- *)

let find_runtime_file filename =
  let candidates =
    [
      "native_rt/" ^ filename;
      Filename.concat
        (Filename.dirname Sys.executable_name)
        ("../../../native_rt/" ^ filename);
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None -> (
      match Sys.getenv_opt "MMLC_RUNTIME_DIR" with
      | Some dir ->
          let path = Filename.concat dir filename in
          if Sys.file_exists path then path
          else
            error
              (Printf.sprintf "%s not found in MMLC_RUNTIME_DIR=%s" filename dir)
      | None ->
          error
            (Printf.sprintf
               "Cannot find native_rt/%s. Run from project root or set \
                MMLC_RUNTIME_DIR."
               filename))

let find_runtime_c () = find_runtime_file "runtime.c"

let find_context_asm () =
  let arch =
    let ic = Unix.open_process_in "uname -m" in
    let a = try String.trim (input_line ic) with End_of_file -> "" in
    ignore (Unix.close_process_in ic);
    a
  in
  let filename =
    match arch with
    | "x86_64" -> "context_x86_64.S"
    | "arm64" | "aarch64" -> "context_arm64.S"
    | _ -> error ("Unsupported architecture for native codegen: " ^ arch)
  in
  find_runtime_file filename

(* ---- Frontend pipeline (shared with bytecode compiler) ---- *)

let run_frontend source =
  let state = Std.register_all (Interp.repl_state_init ()) in
  let stdlib_programs = state.Interp.setup_typed in
  Interp.wrap_errors (fun () ->
      let tokens = Lexer.tokenize source in
      let program = Parser.parse_program tokens in
      let ctx', typed_program =
        Typechecker.check_program_in_ctx state.Interp.ctx program
      in
      let result = Typechecker.transform_constraints ctx' typed_program in
      (stdlib_programs, result, ctx'.type_env))

(* ---- Compilation ---- *)

(* Lower both the user program and the stdlib programs (shared by the
   single-unit and multi-unit codegen paths). *)
let lower_all source =
  let stdlib_programs, typed_program, type_env = run_frontend source in
  let typed_programs_for_externs = List.map snd stdlib_programs in
  (* Native runs all-THOpTry bodies as setjmp thunks and all-THOpProvide bodies
     inline, and supports `return` escaping both (the thunk early-return flag +
     handler-mark restore in codegen). So allow it — `return` crossing a fiber
     (full/THOp or mixed) handler is still rejected by check_returns. *)
  let typed_program =
    Pipeline.lower ~stdlib_programs:typed_programs_for_externs type_env
      typed_program
  in
  let stdlib_programs =
    List.map (fun (te, p) -> (te, Pipeline.lower te p)) stdlib_programs
  in
  (type_env, stdlib_programs, typed_program)

(* Single combined LLVM unit (stdlib + user in one .ll). Used by --emit-ir. *)
let compile_ir (source : string) : string =
  let type_env, stdlib_programs, typed_program = lower_all source in
  Codegen.compile_program_with_stdlib
    ~target_triple:(detect_target_triple ()) type_env stdlib_programs
    typed_program

(* Separately-linkable LLVM units: [(unit_name, ir)] in link order (stdlib, then
   entry). The basis for incremental builds — each unit's object can be cached. *)
let compile_units (source : string) : (string * string) list =
  let type_env, stdlib_programs, typed_program = lower_all source in
  Codegen.compile_units ~target_triple:(detect_target_triple ()) type_env
    stdlib_programs typed_program

(* Discover Boehm GC flags for the platform, split into COMPILE flags (the include
   path, needed when compiling runtime.c which #includes <gc.h>) and LINK flags
   (the library, needed only at the final link). Keeping them apart avoids clang's
   "-lgc: linker input unused" warning when compiling to an object. *)
let gc_flags () =
  if Sys.file_exists "/opt/homebrew/include/gc/gc.h" then
    ("-I/opt/homebrew/include", "-L/opt/homebrew/lib -lgc")
  else if Sys.file_exists "/usr/local/include/gc/gc.h" then
    ("-I/usr/local/include", "-L/usr/local/lib -lgc")
  else
    let line cmd =
      let ic = Unix.open_process_in cmd in
      let s = try input_line ic with End_of_file -> "" in
      ignore (Unix.close_process_in ic);
      s
    in
    let cflags = line "pkg-config --cflags bdw-gc 2>/dev/null" in
    let ldflags =
      match line "pkg-config --libs bdw-gc 2>/dev/null" with
      | "" -> "-lgc"
      | s -> s
    in
    (cflags, ldflags)

let run_clang cmd =
  let exit_code = Sys.command cmd in
  if exit_code <> 0 then
    error (Printf.sprintf "clang failed with exit code %d" exit_code)

let read_file_str path =
  let ic = open_in_bin path in
  let s = In_channel.input_all ic in
  close_in ic;
  s

(* The compiler/toolchain identity folded into every object cache key, so a clang
   upgrade invalidates cached objects. The optimization flags are keyed separately
   (per build, since release adds -flto), so release and debug objects never mix. *)
let toolchain_key =
  lazy
    (let ic = Unix.open_process_in "clang --version 2>/dev/null" in
     let s = try input_line ic with End_of_file -> "unknown" in
     ignore (Unix.close_process_in ic);
     s)

let obj_cache_dir () =
  let d = Filename.concat (Fetch.cache_root ()) "obj" in
  Fetch.mkdir_p d;
  d

(* Content-addressed object cache: return the path to a .o for [content],
   compiling it (via [compile objpath]) on a miss. The key folds in the toolchain
   identity and [key_extra] (the optimization flags); [content] is whatever bytes
   determine the object (an .ll unit's text, which already embeds the target triple,
   or a runtime source's contents). The compile writes to a temp path that is
   atomically renamed into place, so concurrent builds racing on the same key stay
   correct. This is the heart of incremental builds: the stdlib unit's .ll is
   identical across builds, so its object is compiled once and reused (likewise the
   C runtime + context asm). *)
(* Length-prefix a string so concatenations of variable components are
   unambiguous without a separator byte. (A NUL separator would be both
   unrepresentable in MiniML's null-terminated strings when this driver
   self-hosts, and ambiguous if a component itself contained a NUL.) *)
let framed s = string_of_int (String.length s) ^ ":" ^ s

let ensure_object ~key_extra ~content ~compile : string =
  let key =
    Digest.to_hex
      (Digest.string
         (framed (Lazy.force toolchain_key) ^ framed key_extra ^ content))
  in
  let obj = Filename.concat (obj_cache_dir ()) (key ^ ".o") in
  if not (Sys.file_exists obj) then begin
    let tmp = Printf.sprintf "%s.tmp.%d" obj (Unix.getpid ()) in
    compile tmp;
    (try Sys.rename tmp obj with _ -> (try Sys.remove tmp with _ -> ()))
  end;
  obj

(* Compile one LLVM unit (name, ir) to a cached object with optimization flags
   [opt] ("-O2", or "-O2 -flto" for a release build's cross-unit inlining). *)
let unit_object ~opt ~name ~ir =
  ensure_object ~key_extra:opt ~content:ir ~compile:(fun objpath ->
      let llf = Filename.temp_file ("mml_" ^ name ^ "_") ".ll" in
      Fun.protect
        ~finally:(fun () -> try Sys.remove llf with _ -> ())
        (fun () ->
          let oc = open_out llf in
          output_string oc ir;
          close_out oc;
          run_clang
            (Printf.sprintf "clang %s -c %s -o %s" opt (Filename.quote llf)
               (Filename.quote objpath))))

(* [release] enables -flto: each unit's object becomes LLVM bitcode and the link
   step does cross-module optimization (inlining across units), recovering what
   whole-program -O2 used to give. It is off by default because LTO re-optimizes
   the whole program at every link, which would defeat fast incremental rebuilds;
   `mml build --release` opts in. *)
(* Experimental MPS moving-GC backend (roadmap: native moving GC). Opt-in at build
   time via MML_LINK_MPS so the default build/gate stay byte-identical and MPS-free;
   selected at runtime within an MPS-linked binary by MML_GC=mps (else Boehm). When
   enabled, runtime.c is compiled with -DMML_HAVE_MPS and the binary links mml_gc.o +
   the (big, cached) mps.o. *)
let mps_enabled () = try Sys.getenv "MML_LINK_MPS" <> "" with Not_found -> false

let find_mps_dir () =
  let candidates =
    [ "third_party/mps/code"; "../../../third_party/mps/code" ]
  in
  match List.find_opt (fun d -> Sys.file_exists (Filename.concat d "mps.c")) candidates with
  | Some d -> d
  | None ->
      error "Cannot find third_party/mps/code (MML_LINK_MPS needs the project root)"

let compile_to_native ?(release = false) ~source_file ~output () =
  let source = read_file_str source_file in
  let units = compile_units source in
  let runtime_c = find_runtime_c () in
  let context_asm = find_context_asm () in
  let gc_cflags, gc_ldflags = gc_flags () in
  let opt = if release then "-O2 -flto" else "-O2" in
  (* MPS objects (cached) + the -DMML_HAVE_MPS / include flags folded into runtime.c. *)
  let mps_runtime_cflags, mps_objs =
    if not (mps_enabled ()) then ("", [])
    else
      let mdir = find_mps_dir () in
      let mps_c = Filename.concat mdir "mps.c" in
      let mml_gc_c = find_runtime_file "mml_gc.c" in
      let mps_inc = "-I" ^ mdir in
      let mps_obj =
        ensure_object ~key_extra:"mps-O2-clockpatch" ~content:(read_file_str mps_c)
          ~compile:(fun o ->
            run_clang
              (Printf.sprintf "clang -O2 -c %s -o %s" (Filename.quote mps_c)
                 (Filename.quote o)))
      in
      let mml_gc_obj =
        ensure_object ~key_extra:("mmlgc" ^ opt)
          ~content:(framed (read_file_str mml_gc_c) ^ mps_inc ^ gc_cflags)
          ~compile:(fun o ->
            run_clang
              (Printf.sprintf "clang %s -DMML_HAVE_MPS %s %s -c %s -o %s" opt mps_inc
                 gc_cflags (Filename.quote mml_gc_c) (Filename.quote o)))
      in
      ("-DMML_HAVE_MPS " ^ mps_inc, [ mml_gc_obj; mps_obj ])
  in

  (* Each MiniML unit -> cached object. The stdlib unit's .ll is build-stable, so
     after the first build its object is a cache hit (the dominant compile cost). *)
  let unit_objs = List.map (fun (name, ir) -> unit_object ~opt ~name ~ir) units in
  (* The C runtime and the context-switch asm rarely change: cache their objects
     too, keyed by their source contents (+ GC include flags for the runtime). *)
  let runtime_obj =
    ensure_object ~key_extra:(opt ^ mps_runtime_cflags)
      ~content:(framed (read_file_str runtime_c) ^ gc_cflags ^ mps_runtime_cflags)
      ~compile:(fun o ->
        run_clang
          (Printf.sprintf "clang %s %s %s -c %s -o %s" opt gc_cflags
             mps_runtime_cflags (Filename.quote runtime_c) (Filename.quote o)))
  in
  let context_obj =
    (* hand-written assembly: not LLVM-optimizable, so no opt flags / no -flto *)
    ensure_object ~key_extra:"asm" ~content:(read_file_str context_asm)
      ~compile:(fun o ->
        run_clang
          (Printf.sprintf "clang -c %s -o %s" (Filename.quote context_asm)
             (Filename.quote o)))
  in
  (* Link the (mostly cached) objects into the final executable; -flto here drives
     the cross-module optimization when release. *)
  run_clang
    (Printf.sprintf "clang %s %s -o %s %s %s %s %s" opt gc_ldflags
       (Filename.quote output) (Filename.quote runtime_obj)
       (Filename.quote context_obj)
       (String.concat " " (List.map Filename.quote mps_objs))
       (String.concat " " (List.map Filename.quote unit_objs)))

let emit_ir_to_stdout ~source_file =
  let ic = open_in source_file in
  let source = In_channel.input_all ic in
  close_in ic;
  let ir_text = compile_ir source in
  print_string ir_text
