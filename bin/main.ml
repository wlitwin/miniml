let repl () =
  print_endline "MiniML interpreter (type expressions, ;; to evaluate)";
  print_endline "Press Ctrl-D to exit.";
  let buf = Buffer.create 256 in
  let state = ref (Interpreter.Std.register_all (Interpreter.Interp.repl_state_init ())) in
  try while true do
    if Buffer.length buf = 0 then
      print_string ">>> "
    else
      print_string "... ";
    flush stdout;
    let line = input_line stdin in
    let trimmed_line = String.trim line in
    let repl_cmd =
      if String.length trimmed_line > 5 && String.sub trimmed_line 0 5 = ":type"
      then `Type (String.trim (String.sub trimmed_line 5 (String.length trimmed_line - 5)))
      else if String.length trimmed_line > 2 && String.sub trimmed_line 0 3 = ":t "
      then `Type (String.trim (String.sub trimmed_line 3 (String.length trimmed_line - 3)))
      else if trimmed_line = ":modules" || trimmed_line = ":m"
      then `Modules
      else if String.length trimmed_line > 7 && String.sub trimmed_line 0 7 = ":browse"
      then `Browse (String.trim (String.sub trimmed_line 7 (String.length trimmed_line - 7)))
      else if String.length trimmed_line > 2 && String.sub trimmed_line 0 3 = ":b "
      then `Browse (String.trim (String.sub trimmed_line 3 (String.length trimmed_line - 3)))
      else if trimmed_line = ":classes" || trimmed_line = ":c"
      then `Classes
      else if trimmed_line = ":externs" || trimmed_line = ":e"
      then `Externs
      else if trimmed_line = ":help" || trimmed_line = ":h"
      then `Help
      else `None
    in
    if Buffer.length buf = 0 && repl_cmd <> `None then begin
      (match repl_cmd with
       | `Type expr ->
         (try
           let ty_str = Interpreter.Interp.typeof_source !state expr in
           Printf.printf "%s\n%!" ty_str
         with Interpreter.Interp.Error msg ->
           Printf.eprintf "%s\n%!" msg)
       | `Modules ->
         Printf.printf "%s\n%!" (Interpreter.Interp.list_modules !state)
       | `Browse mod_name ->
         Printf.printf "%s\n%!" (Interpreter.Interp.browse_module !state mod_name)
       | `Classes ->
         Printf.printf "%s\n%!" (Interpreter.Interp.list_classes !state)
       | `Externs ->
         Printf.printf "%s\n%!" (Interpreter.Interp.list_externs !state)
       | `Help ->
         print_endline "Commands:";
         print_endline "  :type <expr>    Show the type of an expression (shortcut: :t)";
         print_endline "  :modules        List all loaded modules (shortcut: :m)";
         print_endline "  :browse <mod>   Show contents of a module (shortcut: :b)";
         print_endline "  :classes        List all type classes (shortcut: :c)";
         print_endline "  :externs        List all registered externals (shortcut: :e)";
         print_endline "  :help           Show this help message (shortcut: :h)";
         print_endline "";
         print_endline "End expressions with ;; to evaluate."
       | `None -> ())
    end else begin
      Buffer.add_string buf line;
      Buffer.add_char buf '\n';
      let content = Buffer.contents buf in
      if String.contains content ';' && String.length content >= 2 then begin
        (* Check for ;; at the end *)
        let trimmed = String.trim content in
        if String.length trimmed >= 2 &&
           trimmed.[String.length trimmed - 2] = ';' &&
           trimmed.[String.length trimmed - 1] = ';' then begin
          Buffer.clear buf;
          let source = String.sub trimmed 0 (String.length trimmed - 2) in
          state := Interpreter.Interp.eval_repl_show !state source;
          Interpreter.Interp.eval_state := Some !state
        end
      end
    end
  done with
  | End_of_file -> print_newline ()

let () =
  let argc = Array.length Sys.argv in
  match () with
  | () when argc = 1 -> repl ()
  | () when argc = 2 && Sys.argv.(1) = "--emit-json" ->
    (try
      Interpreter.Interp.start_capture ();
      let state = Interpreter.Std.register_all (Interpreter.Interp.repl_state_init ()) in
      Interpreter.Interp.stop_capture ();
      let json = Interpreter.Interp.emit_json_bundle state () in
      print_string json
    with
    | Interpreter.Interp.Error msg ->
      Printf.eprintf "%s\n" msg;
      exit 1)
  | () when argc >= 3 && Sys.argv.(1) = "--emit-json" ->
    (try
      Interpreter.Interp.start_capture ();
      let state = Interpreter.Std.register_all (Interpreter.Interp.repl_state_init ()) in
      Interpreter.Interp.stop_capture ();
      let buf = Buffer.create 4096 in
      for i = 2 to argc - 1 do
        let ic = open_in Sys.argv.(i) in
        Buffer.add_string buf (In_channel.input_all ic);
        Buffer.add_char buf '\n';
        close_in ic
      done;
      let source = Buffer.contents buf in
      let json = Interpreter.Interp.wrap_errors (fun () ->
        Interpreter.Interp.emit_json_bundle state ~source ()) in
      print_string json
    with
    | Interpreter.Interp.Error msg ->
      Printf.eprintf "%s\n" msg;
      exit 1)
  | () when argc >= 3 && Sys.argv.(1) = "--run-json" ->
    (try
      Interpreter.Interp.script_argv := Array.sub Sys.argv 2 (argc - 2);
      let state = Interpreter.Std.register_all (Interpreter.Interp.repl_state_init ()) in
      let builtins = Interpreter.Interp.builtin_table state in
      let ic = open_in Sys.argv.(2) in
      let json_str = In_channel.input_all ic in
      close_in ic;
      let _ = Interpreter.Deserialize.load_bundle json_str builtins in
      ()
    with
    | Interpreter.Vm.Runtime_error msg ->
      Printf.eprintf "Runtime error: %s\n" msg;
      exit 1
    | Interpreter.Interp.Error msg ->
      Printf.eprintf "%s\n" msg;
      exit 1
    | Sys_error msg ->
      Printf.eprintf "%s\n" msg;
      exit 1
    | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1)
  | () when argc = 2 && Sys.argv.(1) = "--emit-binary" ->
    (try
      Interpreter.Interp.start_capture ();
      let state = Interpreter.Std.register_all (Interpreter.Interp.repl_state_init ()) in
      Interpreter.Interp.stop_capture ();
      let bin = Interpreter.Interp.emit_binary_bundle state () in
      set_binary_mode_out stdout true;
      print_string bin
    with
    | Interpreter.Interp.Error msg ->
      Printf.eprintf "%s\n" msg;
      exit 1)
  | () when argc >= 3 && Sys.argv.(1) = "--emit-binary" ->
    (try
      Interpreter.Interp.start_capture ();
      let state = Interpreter.Std.register_all (Interpreter.Interp.repl_state_init ()) in
      Interpreter.Interp.stop_capture ();
      let buf = Buffer.create 4096 in
      for i = 2 to argc - 1 do
        let ic = open_in Sys.argv.(i) in
        Buffer.add_string buf (In_channel.input_all ic);
        Buffer.add_char buf '\n';
        close_in ic
      done;
      let source = Buffer.contents buf in
      let bin = Interpreter.Interp.wrap_errors (fun () ->
        Interpreter.Interp.emit_binary_bundle state ~source ()) in
      set_binary_mode_out stdout true;
      print_string bin
    with
    | Interpreter.Interp.Error msg ->
      Printf.eprintf "%s\n" msg;
      exit 1)
  | () when argc >= 3 && Sys.argv.(1) = "--run-binary" ->
    (try
      Interpreter.Interp.script_argv := Array.sub Sys.argv 2 (argc - 2);
      let state = Interpreter.Std.register_all (Interpreter.Interp.repl_state_init ()) in
      let builtins = Interpreter.Interp.builtin_table state in
      let ic = open_in_bin Sys.argv.(2) in
      let data = In_channel.input_all ic in
      close_in ic;
      let _ = Interpreter.Deserialize_bin.load_bundle_binary data builtins in
      ()
    with
    | Interpreter.Vm.Runtime_error msg ->
      Printf.eprintf "Runtime error: %s\n" msg;
      exit 1
    | Interpreter.Interp.Error msg ->
      Printf.eprintf "%s\n" msg;
      exit 1
    | Sys_error msg ->
      Printf.eprintf "%s\n" msg;
      exit 1
    | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1)
  | () when argc = 3 && Sys.argv.(1) = "--json-to-binary" ->
    (try
      let ic = open_in Sys.argv.(2) in
      let json_str = In_channel.input_all ic in
      close_in ic;
      let bin = Interpreter.Serialize_bin.convert_json_to_binary json_str in
      set_binary_mode_out stdout true;
      print_string bin
    with
    | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
    | Sys_error msg ->
      Printf.eprintf "%s\n" msg;
      exit 1)
  | () when argc = 2 && Sys.argv.(1) = "--emit-js" ->
    (try
      Interpreter.Interp.start_capture ();
      Interpreter.Interp.start_js_capture ();
      let state = Interpreter.Std.register_all (Interpreter.Interp.repl_state_init ()) in
      Interpreter.Interp.stop_capture ();
      Interpreter.Interp.stop_js_capture ();
      let js = Interpreter.Interp.emit_js state () in
      print_string js
    with
    | Interpreter.Interp.Error msg ->
      Printf.eprintf "%s\n" msg;
      exit 1
    | Interpreter.Js_codegen.Codegen_error msg ->
      Printf.eprintf "JS codegen error: %s\n" msg;
      exit 1)
  | () when argc >= 3 && Sys.argv.(1) = "--emit-js" ->
    (try
      Interpreter.Interp.start_capture ();
      Interpreter.Interp.start_js_capture ();
      let state = Interpreter.Std.register_all (Interpreter.Interp.repl_state_init ()) in
      Interpreter.Interp.stop_capture ();
      Interpreter.Interp.stop_js_capture ();
      let buf = Buffer.create 4096 in
      for i = 2 to argc - 1 do
        let ic = open_in Sys.argv.(i) in
        Buffer.add_string buf (In_channel.input_all ic);
        Buffer.add_char buf '\n';
        close_in ic
      done;
      let source = Buffer.contents buf in
      let js = Interpreter.Interp.wrap_errors (fun () ->
        Interpreter.Interp.emit_js state ~source ()) in
      print_string js
    with
    | Interpreter.Interp.Error msg ->
      Printf.eprintf "%s\n" msg;
      exit 1
    | Interpreter.Js_codegen.Codegen_error msg ->
      Printf.eprintf "JS codegen error: %s\n" msg;
      exit 1)
  | () when argc >= 2 ->
    Interpreter.Interp.script_argv := Array.sub Sys.argv 1 (argc - 1);
    let state = Interpreter.Std.register_all (Interpreter.Interp.repl_state_init ()) in
    (try
      let ic = open_in Sys.argv.(1) in
      let source = In_channel.input_all ic in
      close_in ic;
      let _ = Interpreter.Interp.wrap_errors (fun () ->
        Interpreter.Interp.run_string_in_state state source) in
      ()
    with
    | Interpreter.Interp.Error msg ->
      Printf.eprintf "%s\n" msg;
      exit 1)
  | () ->
    Printf.eprintf "Usage: interpreter [file] [args...]\n       interpreter --emit-json [file]\n       interpreter --run-json <file.json> [args...]\n       interpreter --emit-binary [file]\n       interpreter --run-binary <file.mmlb> [args...]\n";
    exit 1
