(* The .tests file format — THE single OCaml implementation, shared by every
   OCaml test runner (cross_test/runner.ml, native_test/runner.ml,
   compiler_test/parity_runner.ml, diff_test/diff_runner.ml). The single JS
   implementation is cross_test/test_parser.js. Any format change must update
   exactly those two files.

   Format:

     --- test: <name>
     <MiniML source lines>
     --- expect: <observable output>

   Expectations:
     --- expect: <value>                 program output equals <value>
     --- expect: "<value>"               quoted: preserves leading/trailing
                                         whitespace, and interprets \n as a
                                         newline and \\ as a backslash (for
                                         multi-line output, e.g. consecutive
                                         `print` calls)
     --- expect-type-error               any type error
     --- expect-type-error: <substr>     type error containing <substr>
     --- expect-runtime-error: <substr>  runtime error containing <substr>

   Per-backend directives (between the source and the expectation):
     --- skip-<backend>: <reason>        skip this test on <backend>; the
                                         reason must reference a tracked bug
     --- expect-<backend>: <value>       backend-specific expected output
                                         (for documented, tracked divergences)

   Backend names in directives: native, emit-js, playground, ... — each runner
   asks for its own name via [skip_reason] / [expected_for]. Section banners
   (=== ... ===) and directives for other backends are ignored. *)

type expectation =
  | Value of string
  | TypeError
  | TypeErrorMsg of string
  | RuntimeError of string

type test_case = {
  name : string;
  source : string;
  expect : expectation;
  skips : (string * string) list; (* backend -> reason *)
  expect_overrides : (string * string) list; (* backend -> expected value *)
}

(* Why this test should be skipped on [backend], if a skip directive says so. *)
let skip_reason tc ~backend = List.assoc_opt backend tc.skips

(* The expectation [backend] should check: its --- expect-<backend>: override
   when present, the base expectation otherwise. *)
let expected_for tc ~backend =
  match List.assoc_opt backend tc.expect_overrides with
  | Some v -> Value v
  | None -> tc.expect

(* --- Parsing ------------------------------------------------------------ *)

let starts_with prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

(* Interpret a raw expected value: the quoted form preserves whitespace and
   supports \n (newline) and \\ (backslash) escapes; the bare form is taken
   literally. *)
let parse_expected_value raw =
  let len = String.length raw in
  if len >= 2 && raw.[0] = '"' && raw.[len - 1] = '"' then begin
    let inner = String.sub raw 1 (len - 2) in
    let buf = Buffer.create (String.length inner) in
    let i = ref 0 in
    while !i < String.length inner do
      if inner.[!i] = '\\' && !i + 1 < String.length inner then begin
        (match inner.[!i + 1] with
        | 'n' -> Buffer.add_char buf '\n'
        | '\\' -> Buffer.add_char buf '\\'
        | c ->
            Buffer.add_char buf '\\';
            Buffer.add_char buf c);
        i := !i + 2
      end
      else begin
        Buffer.add_char buf inner.[!i];
        incr i
      end
    done;
    Buffer.contents buf
  end
  else raw

type parse_state = Idle | CollectingSource of string (* test name *)

let parse_test_file filename =
  let ic = open_in filename in
  let tests = ref [] in
  let state = ref Idle in
  let source_buf = Buffer.create 256 in
  let pending_skips = ref [] in
  let pending_overrides = ref [] in
  let flush name expect =
    let source = String.trim (Buffer.contents source_buf) in
    tests :=
      {
        name;
        source;
        expect;
        skips = List.rev !pending_skips;
        expect_overrides = List.rev !pending_overrides;
      }
      :: !tests;
    state := Idle;
    Buffer.clear source_buf;
    pending_skips := [];
    pending_overrides := []
  in
  (* "--- skip-native: reason" -> Some ("native", "reason") *)
  let parse_backend_directive ~prefix line =
    if starts_with prefix line then
      match String.index_opt line ':' with
      | Some colon when colon > String.length prefix ->
          let backend =
            String.sub line (String.length prefix)
              (colon - String.length prefix)
          in
          let rest =
            String.trim
              (String.sub line (colon + 1) (String.length line - colon - 1))
          in
          Some (backend, rest)
      | _ -> None
    else None
  in
  (try
     while true do
       let line = input_line ic in
       let trimmed = String.trim line in
       if starts_with "--- test:" trimmed then begin
         let name =
           String.trim (String.sub trimmed 9 (String.length trimmed - 9))
         in
         state := CollectingSource name;
         Buffer.clear source_buf;
         pending_skips := [];
         pending_overrides := []
       end
       else if starts_with "--- expect: " line then begin
         match !state with
         | CollectingSource name ->
             let raw = String.sub line 12 (String.length line - 12) in
             flush name (Value (parse_expected_value raw))
         | Idle -> ()
       end
       else if starts_with "--- expect-type-error:" trimmed then begin
         match !state with
         | CollectingSource name ->
             let substr =
               String.trim (String.sub trimmed 22 (String.length trimmed - 22))
             in
             if substr = "" then flush name TypeError
             else flush name (TypeErrorMsg substr)
         | Idle -> ()
       end
       else if trimmed = "--- expect-type-error" then begin
         match !state with
         | CollectingSource name -> flush name TypeError
         | Idle -> ()
       end
       else if starts_with "--- expect-runtime-error:" trimmed then begin
         match !state with
         | CollectingSource name ->
             let substr =
               String.trim (String.sub trimmed 25 (String.length trimmed - 25))
             in
             flush name (RuntimeError substr)
         | Idle -> ()
       end
       else
         match parse_backend_directive ~prefix:"--- skip-" line with
         | Some (backend, reason) ->
             pending_skips := (backend, reason) :: !pending_skips
         | None -> (
             match parse_backend_directive ~prefix:"--- expect-" line with
             | Some (backend, value) ->
                 pending_overrides :=
                   (backend, parse_expected_value value) :: !pending_overrides
             | None -> (
                 (* Source line or ignored line *)
                 match !state with
                 | CollectingSource _ ->
                     (* Skip section banners; skip blank lines before the first
                        source line; absorb everything else as source *)
                     if Buffer.length source_buf > 0 || trimmed <> "" then begin
                       if starts_with "===" trimmed then ()
                       else if starts_with "--- " trimmed then
                         () (* unrecognized directive: not source *)
                       else begin
                         if Buffer.length source_buf > 0 then
                           Buffer.add_char source_buf '\n';
                         Buffer.add_string source_buf line
                       end
                     end
                 | Idle -> ()))
     done
   with End_of_file -> ());
  close_in ic;
  List.rev !tests

(* --- Helpers ------------------------------------------------------------- *)

let contains_substring haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen > hlen then false
  else begin
    let found = ref false in
    for i = 0 to hlen - nlen do
      if String.sub haystack i nlen = needle then found := true
    done;
    !found
  end

let find_test_files dir =
  let files = ref [] in
  let handle = Unix.opendir dir in
  (try
     while true do
       let entry = Unix.readdir handle in
       if Filename.check_suffix entry ".tests" then
         files := Filename.concat dir entry :: !files
     done
   with End_of_file -> ());
  Unix.closedir handle;
  List.sort String.compare !files

(* Keep tests whose (lowercased) name contains any of the filter strings. *)
let filter_tests filters tests =
  match filters with
  | [] -> tests
  | _ ->
      List.filter
        (fun tc ->
          let name_lower = String.lowercase_ascii tc.name in
          List.exists (fun f -> contains_substring name_lower f) filters)
        tests
