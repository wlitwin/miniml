#!/usr/bin/env node
// Test harness: compiles programs with OCaml, runs them in JS VM, compares output

const { execSync } = require("child_process");
const fs = require("fs");
const path = require("path");
const { loadBundle } = require("./loader");
const vm = require("./vm");

const INTERPRETER = path.resolve(__dirname, "..", "_build", "default", "bin", "main.exe");

let passed = 0;
let failed = 0;
const failures = [];

function test(name, source, expected) {
  // Write source to temp file
  const tmpFile = `/tmp/jsvm_test_${process.pid}.ml`;
  fs.writeFileSync(tmpFile, source);

  try {
    // Compile to JSON bundle
    const json = execSync(`${INTERPRETER} --emit-json ${tmpFile}`, {
      encoding: "utf-8",
      maxBuffer: 10 * 1024 * 1024,
    });

    // Capture output
    const outputs = [];
    globalThis._vmOutput = (s) => outputs.push(s);

    // Run in JS VM
    let result;
    try {
      result = loadBundle(json);
    } catch (e) {
      if (e instanceof vm.RuntimeError) {
        result = `Runtime error: ${e.message}`;
      } else {
        throw e;
      }
    }

    globalThis._vmOutput = null;

    // Build actual output
    let actual;
    if (typeof result === "string") {
      // error case
      actual = result;
    } else if (outputs.length > 0 && result.tag === "unit") {
      actual = outputs.join("\n");
    } else if (outputs.length > 0) {
      actual = outputs.join("\n") + "\n" + vm.ppValue(result);
    } else {
      actual = vm.ppValue(result);
    }

    if (actual === expected) {
      console.log(`  PASS: ${name}`);
      passed++;
    } else {
      console.log(`  FAIL: ${name}`);
      console.log(`    expected: ${JSON.stringify(expected)}`);
      console.log(`    actual:   ${JSON.stringify(actual)}`);
      failed++;
      failures.push(name);
    }
  } catch (e) {
    console.log(`  FAIL: ${name}`);
    console.log(`    error: ${e.message}`);
    failed++;
    failures.push(name);
  } finally {
    try { fs.unlinkSync(tmpFile); } catch (_) {}
  }
}

function testError(name, source, expectedError) {
  const tmpFile = `/tmp/jsvm_test_${process.pid}.ml`;
  fs.writeFileSync(tmpFile, source);

  try {
    const json = execSync(`${INTERPRETER} --emit-json ${tmpFile}`, {
      encoding: "utf-8",
      maxBuffer: 10 * 1024 * 1024,
    });

    globalThis._vmOutput = () => {};

    try {
      loadBundle(json);
      console.log(`  FAIL: ${name} (expected error, got success)`);
      failed++;
      failures.push(name);
    } catch (e) {
      if (e instanceof vm.RuntimeError && e.message.includes(expectedError)) {
        console.log(`  PASS: ${name}`);
        passed++;
      } else {
        console.log(`  FAIL: ${name}`);
        console.log(`    expected error containing: ${JSON.stringify(expectedError)}`);
        console.log(`    actual: ${e.message}`);
        failed++;
        failures.push(name);
      }
    }
    globalThis._vmOutput = null;
  } catch (e) {
    console.log(`  FAIL: ${name} (compile error: ${e.message})`);
    failed++;
    failures.push(name);
  } finally {
    try { fs.unlinkSync(tmpFile); } catch (_) {}
  }
}

// ============================================================
// Tests
// ============================================================

console.log("=== Arithmetic ===");
test("integer literal", "42", "42");
test("addition", "1 + 2", "3");
test("subtraction", "10 - 3", "7");
test("multiplication", "4 * 5", "20");
test("division", "17 / 3", "5");
test("modulo", "17 mod 3", "2");
test("negation", "-(5)", "-5");
test("precedence", "2 + 3 * 4", "14");
test("float add", "1.5 + 2.5", "4.");
test("float sub", "10.0 - 3.5", "6.5");
test("float mul", "2.0 * 3.0", "6.");
test("float div", "7.0 / 2.0", "3.5");
test("float neg", "-(1.5)", "-1.5");

console.log("\n=== Comparisons ===");
test("less than", "1 < 2", "true");
test("greater than", "2 > 1", "true");
test("less equal", "2 <= 2", "true");
test("greater equal", "3 >= 4", "false");
test("equal", "42 = 42", "true");
test("not equal", "1 <> 2", "true");

console.log("\n=== Booleans ===");
test("and true", "true && true", "true");
test("and false", "true && false", "false");
test("or true", "false || true", "true");
test("or false", "false || false", "false");
test("not", "not true", "false");

console.log("\n=== Strings ===");
test("string literal", '"hello"', "hello");
test("string concat", '"hello" ^ " " ^ "world"', "hello world");
test("string interpolation", 'let x = 42 in $"value is {x}"', "value is 42");

console.log("\n=== Let/Functions ===");
test("let binding", "let x = 10 in x + 5", "15");
test("nested let", "let x = 1 in let y = 2 in x + y", "3");
test("lambda", "(fn x -> x + 1) 10", "11");
test("multi-arg", "let add x y = x + y in add 3 4", "7");
test("higher-order", "let apply f x = f x in apply (fn x -> x * 2) 5", "10");
test("closure", "let x = 10 in let f y = x + y in f 5", "15");
test("recursion", "let rec fact n = if n <= 1 do 1 else n * fact (n - 1) in fact 5", "120");

console.log("\n=== Tuples ===");
test("tuple creation", "(1, 2, 3)", "(1, 2, 3)");
test("tuple destructure", "let (a, b) = (10, 20) in a + b", "30");

console.log("\n=== Lists ===");
test("empty list", "[]", "[]");
test("list literal", "[1; 2; 3]", "[1; 2; 3]");
test("cons", "1 :: [2; 3]", "[1; 2; 3]");
test("list match", "match [1; 2; 3] with | x :: _ -> x | [] -> 0", "1");

console.log("\n=== Records ===");
test("record creation", '{ x = 1; y = "hello" }', '{ x = 1; y = hello }');
test("record field", "let r = { x = 42; y = 0 } in r.x", "42");

console.log("\n=== Variants ===");
test("variant", 'match Some 42 with | Some x -> x | None -> 0', "42");
test("variant none", 'match None with | Some x -> x | None -> 0', "0");

console.log("\n=== Print ===");
test("print int", "print 42", "42");
test("print string", 'print "hello"', "hello");
test("print multiple", 'print 1; print 2; print 3', "1\n2\n3");

console.log("\n=== Type Classes ===");
test("show int", "show 42", "42");
test("show bool", "show true", "true");
test("show string", 'show "hi"', "hi");
test("show list", "show [1; 2; 3]", "[1; 2; 3]");
test("show option some", "show (Some 42)", "Some 42");
test("show option none", "show (None : int option)", "None");

console.log("\n=== Bitwise ===");
test("land", "5 land 3", "1");
test("lor", "5 lor 3", "7");
test("lxor", "5 lxor 3", "6");
test("lsl", "1 lsl 4", "16");
test("lnot", "lnot 0", "-1");

console.log("\n=== String Module ===");
test("String.length", 'String.length "hello"', "5");
test("String.trim", 'String.trim "  hi  "', "hi");
test("String.uppercase", 'String.uppercase "hello"', "HELLO");
test("String.lowercase", 'String.lowercase "HELLO"', "hello");
test("String.starts_with", 'String.starts_with "he" "hello"', "true");
test("String.contains", 'String.contains "ell" "hello"', "true");
test("String.split", 'String.split "," "a,b,c"', "[a; b; c]");
test("String.replace", 'String.replace "world" "there" "hello world"', "hello there");
test("String.to_int some", 'String.to_int "42"', "Some 42");
test("String.to_int none", 'String.to_int "abc"', "None");

console.log("\n=== List Module ===");
test("List.length", "List.length [1; 2; 3]", "3");
test("List.rev", "List.rev [1; 2; 3]", "[3; 2; 1]");
test("List.map", "List.map (fn x -> x * 2) [1; 2; 3]", "[2; 4; 6]");
test("List.filter", "List.filter (fn x -> x > 2) [1; 2; 3; 4]", "[3; 4]");
test("List.fold", "List.fold (fn acc x -> acc + x) 0 [1; 2; 3]", "6");
test("List.concat", "List.concat [1; 2] [3; 4]", "[1; 2; 3; 4]");
test("List.flatten", "List.flatten [[1; 2]; [3; 4]]", "[1; 2; 3; 4]");
test("List.sort", "List.sort (fn a b -> if a < b do 0 - 1 else if a > b do 1 else 0) [3; 1; 2]", "[1; 2; 3]");

console.log("\n=== Array Module ===");
test("Array.make", "Array.make 3 0", "#[0; 0; 0]");
test("Array.of_list", "Array.of_list [1; 2; 3]", "#[1; 2; 3]");
test("Array.length", "Array.length (Array.of_list [1; 2; 3])", "3");
test("Array.to_list", "Array.to_list (Array.of_list [1; 2; 3])", "[1; 2; 3]");

console.log("\n=== Map ===");
test("map literal", 'let m = #{"a": 1; "b": 2} in get "a" m', "Some 1");
test("map size", 'size #{"x": 1; "y": 2}', "2");

console.log("\n=== Effects ===");
test("basic handle return", `
  effect Greeting =
    greet : unit -> string
  end
  handle
    42
  with
  | return x -> x
`, "42");

test("perform and handle", `
  effect Ask =
    ask : unit -> string
  end
  handle
    perform ask ()
  with
  | return x -> x
  | ask () k -> resume k "hello"
`, "hello");

test("state effect", `
  effect State =
    get : unit -> int
    put : int -> unit
  end
  handle
    let x = perform get () in
    perform put (x + 1);
    perform get ()
  with
  | return x -> x
  | get () k -> resume k 10
  | put v k -> resume k ()
`, "10");

console.log("\n=== Pipe Operator ===");
test("basic pipe", "5 |> (fn x -> x + 1)", "6");
test("chained pipes", "[1; 2; 3] |> List.rev |> List.length", "3");

console.log("\n=== For Loops ===");
test("for loop sum", `
  let mut n = 0 in
  for x in [1; 2; 3] do
    n := n + x
  end;
  n
`, "6");
test("fold for", "for x in [1; 2; 3] with acc = 0 do acc + x end", "6");

console.log("\n=== Mutable Variables ===");
test("mutable basic", "let mut x = 1 in x := 2; x", "2");
test("mutable record field", `
  type point = { mut x: int; y: int };;
  let p = { x = 1; y = 2 } in
  p.x := 10;
  p.x
`, "10");

console.log("\n=== Pattern Matching ===");
test("exhaustive match", `
  type color = Red | Green | Blue
  let name c = match c with
    | Red -> "red"
    | Green -> "green"
    | Blue -> "blue"
  in name Green
`, "green");

test("or-pattern", "match 2 with | 1 | 2 -> true | _ -> false", "true");
test("as-pattern", "match [1; 2] with | (x :: _) as l -> (x, l) | [] -> (0, [])", "(1, [1; 2])");
test("guard", "match 5 with | n when n > 3 -> true | _ -> false", "true");

console.log("\n=== Byte/Rune ===");
test("byte to_int", "Byte.to_int #41", "65");
test("rune to_string", "Rune.to_string 'A'", "A");

console.log("\n=== Enum Module ===");
test("Enum.sum", "Enum.sum [1; 2; 3]", "6");
test("Enum.count", "Enum.count (fn x -> x > 2) [1; 2; 3; 4; 5]", "3");
test("Enum.take", "Enum.take 2 [1; 2; 3]", "[1; 2]");
test("Enum.join", 'Enum.join ", " ["a"; "b"; "c"]', "a, b, c");

console.log("\n=== Seq Module ===");
test("Seq.range", "Seq.range 1 4 |> Seq.to_list", "[1; 2; 3]");
test("Seq.map", "Seq.range 1 4 |> Seq.map (fn x -> x * 2) |> Seq.to_list", "[2; 4; 6]");
test("Seq.filter", "Seq.range 1 6 |> Seq.filter (fn x -> x mod 2 = 0) |> Seq.to_list", "[2; 4]");
test("Seq.take infinite", "Seq.iterate 0 (fn x -> x + 1) |> Seq.take 5 |> Seq.to_list", "[0; 1; 2; 3; 4]");

console.log("\n=== Return ===");
test("return from function", `
  let f x =
    if x > 0 do return (x * 2) else ();
    0 - 1
  in f 5
`, "10");

test("return fallthrough", `
  let f x =
    if x > 0 do return (x * 2) else ();
    0 - 1
  in f (0 - 1)
`, "-1");

test("return from inside while loop", `
  let f () =
    let mut i = 0 in
    for i < 10 do
      if i = 5 do return i else ();
      i := i + 1
    end;
    0 - 1
  in f ()
`, "5");

test("return from inside for-in loop", `
  let f () =
    let mut result = 0 - 1 in
    for x in [1; 3; 4; 6] do
      if x mod 2 = 0 do (result := x; return result) else ()
    end;
    result
  in f ()
`, "4");

test("return from nested context", `
  let f x =
    let y = x + 1 in
    if y > 5 do return y else ();
    y * 2
  in f 10
`, "11");

test("return in let rec", `
  let rec search xs target =
    match xs with
    | [] -> 0 - 1
    | x :: rest ->
      if x = target do return x else ();
      search rest target
  in search [1; 2; 3] 2
`, "2");

console.log("\n=== Format Specifiers ===");
test("format float .2f", '$"{3.14159:.2f}"', "3.14");
test("format float .0f", '$"{3.14159:.0f}"', "3");
test("format float .4f", '$"{2.5:.4f}"', "2.5000");
test("format hex lowercase", '$"{255:x}"', "ff");
test("format hex uppercase", '$"{255:X}"', "FF");
test("format octal", '$"{8:o}"', "10");
test("format binary", '$"{10:b}"', "1010");
test("format binary zero", '$"{0:b}"', "0");
test("format zero-pad hex", '$"{42:08x}"', "0000002a");
test("format right-align", '$"{42:>10}"', "        42");
test("format left-align", 'let s = "hi" in $"{s:<10}"', "hi        ");
test("format combined align and float", '$"{3.14159:>10.2f}"', "      3.14");
test("format no spec backward compat", '$"{1 + 2}"', "3");
test("format colon in parens", '$"{(42 : int)}"', "42");
test("format mixed", '$"hex: {255:x}, dec: {255}"', "hex: ff, dec: 255");

console.log("\n=== Error Handling ===");
testError("division by zero", "1 / 0", "division by zero");
testError("match fail", `
  type ab = A | B | C
  let f x =
    @partial
    match x with
    | A -> 1
    | B -> 2
  in f C
`, "non-exhaustive match");

// ============================================================
// Summary
// ============================================================

console.log("\n==============================");
console.log(`${passed + failed} tests: ${passed} passed, ${failed} failed`);
if (failures.length > 0) {
  console.log("Failures:");
  failures.forEach((f) => console.log(`  - ${f}`));
  process.exit(1);
}
