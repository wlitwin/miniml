# Algebraic Effects

Algebraic effects are one of MiniML's most distinctive features. They provide a structured, composable way to handle side effects -- from exceptions and state to generators, non-determinism, and beyond. If you know exceptions, you already understand half the story: effects are exceptions you can *resume from*.

## What Are Algebraic Effects?

In most languages, when you throw an exception, control flow goes one way -- up the stack to the nearest handler, and the computation that threw is abandoned. Algebraic effects generalize this: when you *perform* an effect operation, control transfers to a handler, but the handler receives a **continuation** -- a frozen snapshot of the suspended computation. The handler can then *continue* that computation by supplying a return value, as if the effect operation had returned normally.

This means effects can model:
- **Exceptions** -- perform an effect and never continue (discard the continuation)
- **State** -- handle `get` and `put` by continuing with values
- **Generators** -- yield values one at a time, with the handler collecting them
- **Non-determinism** -- copy a continuation with `copy_continuation` and explore multiple branches
- **Logging, tracing, dependency injection** -- intercept operations and decide how to handle them

All of these patterns compose naturally, without the boilerplate of monad transformers or callback threading.

## Declaring Effects

An effect declaration introduces a named group of operations. Each operation has a type signature describing what argument it takes and what it returns.

```
effect Yield =
  yield : int -> unit
end
```

This declares an effect called `Yield` with one operation, `yield`, which takes an `int` and returns `unit`. The return type describes what the caller of `perform yield` sees when the handler continues the computation.

Effects can have multiple operations:

```
effect State =
  get : unit -> int
  put : int -> unit
end
```

Here `get` takes unit and returns an `int` (the current state), while `put` takes an `int` and returns `unit`.

Operations can also use polymorphic type variables:

```
effect Exn =
  raise : string -> 'a
end
```

A return type of `'a` means the operation never returns normally -- useful for modeling exceptions. The handler can return any type because the continuation is never resumed.

### Parameterized Effects

Effects can take type parameters, making their operations generic over a type. For example, a state effect can be parameterized by the type of value it holds:

```
effect State 'a =
  get : unit -> 'a
  put : 'a -> unit
end
```

Here `State` takes a type parameter `'a`. The operations `get` and `put` use `'a` to describe the type of the stored value. This means `State int` is a state effect holding an `int`, `State string` holds a `string`, and so on -- all from a single declaration.

Without parameterization, you would need a separate effect declaration for each state type. With it, the same `State` effect works for any type:

```
-- State holding an int
handle
  perform put 10;
  perform get ()
with
| return x -> x
| get () k -> resume k state
| put v k -> state := v; resume k ()

-- State holding a string, same effect declaration
handle
  perform put "hello";
  perform get ()
with
| return x -> x
| get () k -> resume k state
| put v k -> state := v; resume k ()
```

## Performing Effects

Use `perform` to invoke an effect operation. The operation name and its argument are required:

```
perform yield 42
```

```
let x = perform get () in
x + 1
```

When `perform` executes, control transfers to the nearest enclosing handler for that operation. The computation is suspended until the handler decides to continue it (or not).

If no handler is installed for the operation, a runtime error occurs:

```
effect Boom =
  boom : unit -> int
end

perform boom ()
-- Runtime error: unhandled effect operation: boom
```

## Handling Effects with `handle/with`

The `handle/with` expression installs effect handlers around a body expression. It has two kinds of arms:

- **`return`** arms: transform the final value when the body completes normally
- **operation** arms: intercept a performed effect

```
handle
  <body>
with
| return x -> <transform x>
| op1 arg k -> <handle op1>
| op2 arg k -> <handle op2>
```

### A Complete Example

```
effect Ask =
  ask : unit -> string
end

handle
  perform ask ()
with
| return x -> x
| ask () k -> resume k "hello"
```

Here is what happens step by step:

1. The body calls `perform ask ()`.
2. Control transfers to the `ask` handler arm.
3. The handler receives `()` as the argument and `k` as the continuation.
4. `resume k "hello"` resumes the body, making `perform ask ()` return `"hello"`.
5. The body finishes with the value `"hello"`.
6. The `return` arm receives `"hello"` as `x` and returns it unchanged.

Result: `"hello"`

### Return Handlers

The `return` arm transforms the final value of the body. This is useful for wrapping results:

```
effect Eff =
  op : unit -> int
end

handle
  perform op ()
with
| return x -> x + 100
| op () k -> resume k 42
```

The body performs `op`, the handler continues with `42`, the body returns `42`, and the return handler transforms it to `142`.

### Discarding the Continuation

You do not have to call `resume`. If the handler simply returns a value without continuing, the suspended computation is abandoned -- just like catching an exception:

```
effect Abort =
  abort : int -> unit
end

handle
  perform abort 42;
  0
with
| return x -> x
| abort v k -> v
```

The handler receives `42` as `v`, ignores the continuation `k`, and returns `42` directly. The `0` after `perform abort 42` is never reached. Result: `42`.

## Simplified Handling with `try/with`

MiniML provides a `try/with` syntax for the common case where you just want to catch effect operations without explicitly using continuations -- similar to exception handling in other languages.

```
try <body> with
| op1 arg -> <result>
| op2 arg -> <result>
```

Key differences from `handle/with`:

- There is no `return` arm -- the body's return value passes through unchanged.
- There is no continuation parameter `k` -- the handler cannot resume the computation.
- The handler arms only receive the operation argument.

### Exception-Style Error Handling

```
effect Exn =
  raise : string -> 'a
end

try
  perform raise "oops"
with
| raise msg -> msg
```

Result: `"oops"`. The body is abandoned and the handler's value is returned.

### Value Pass-Through

When the body completes without performing any handled operations, its value passes through:

```
effect Exn =
  raise : string -> 'a
end

try "hello" with
| raise msg -> "caught"
```

Result: `"hello"`. The `raise` handler is never triggered.

### Multiple Operations

You can handle operations from different effects in a single `try/with`:

```
effect IO =
  file_not_found : string -> 'a
end
effect Validation =
  invalid_input : string -> 'a
end

try
  perform invalid_input "bad data"
with
| file_not_found path -> "missing: " ^ path
| invalid_input msg -> "invalid: " ^ msg
```

Result: `"invalid: bad data"`.

## Continuing with a Value

When a handler calls `resume k value`, it resumes the suspended computation, making the `perform` expression return `value`. The handler then receives the final result of the resumed computation through the `return` arm.

```
effect Val =
  get_val : int -> int
end

handle
  let a = perform get_val 1 in
  let b = perform get_val 2 in
  a + b
with
| return x -> x
| get_val n k -> resume k (n + 10)
```

Step by step:

1. Body performs `get_val 1`. Handler receives `n = 1`, continues with `11`.
2. Body performs `get_val 2`. Handler receives `n = 2`, continues with `12`.
3. Body returns `11 + 12 = 23`.
4. Return handler passes `23` through.

Result: `23`.

## Nested Handlers

Handlers can be nested. Each `perform` is caught by the nearest enclosing handler for that operation:

```
effect Inner =
  inner_op : unit -> int
end
effect Outer =
  outer_op : unit -> int
end

handle
  handle
    let a = perform inner_op () in
    let b = perform outer_op () in
    a + b
  with
  | return x -> x
  | inner_op () k -> resume k 10
with
| return x -> x
| outer_op () k -> resume k 20
```

The inner handler catches `inner_op` and the outer handler catches `outer_op`. Result: `30`.

## One-Shot vs Multi-Shot Continuations

By default, continuations in MiniML are **one-shot**: they can only be called once. Attempting to call the same continuation twice results in a runtime error:

```
effect Eff =
  op : unit -> int
end

handle
  perform op ()
with
| return x -> x
| op () k ->
  let _ = resume k 1 in
  resume k 2
-- Runtime error: continuation already resumed
```

### Multi-Shot Continuations with `copy_continuation`

To use a continuation more than once, use the built-in `copy_continuation` function to create a fresh copy before each use. A copied continuation is a completely independent snapshot that can be resumed separately:

```
effect Pick =
  pick : unit -> int
end

handle
  perform pick ()
with
| return x -> [x]
| pick () k ->
  let k2 = copy_continuation k in
  let a = resume k 1 in
  let b = resume k2 2 in
  List.concat a b
```

Here `resume k 1` runs the body with `pick` returning `1`, producing `[1]`. Then `resume k2 2` runs a completely separate copy of the body with `pick` returning `2`, producing `[2]`. The results are concatenated: `[1; 2]`.

Multi-shot continuations are powerful for modeling non-determinism, backtracking search, and other patterns that explore multiple execution paths.

## Practical Examples

### Generators / Yield

Collect values yielded during a tree traversal:

```
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree
  deriving Show

effect Yield =
  yield : int -> unit
end

let rec emit t =
  match t with
  | Leaf -> ()
  | Node (left, v, right) ->
    emit left;
    perform yield v;
    emit right

let tree = Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf))

let collected =
  let mut result = [] in
  handle
    emit tree
  with
  | return _ -> List.rev result
  | yield v k ->
    result := v :: result;
    resume k ()

-- collected = [1; 2; 3]
```

Each time `emit` performs `yield v`, the handler appends `v` to the result list and resumes the traversal with `resume k ()`. When the traversal finishes, the `return` arm reverses and returns the collected list. This is deep handling at work -- the handler is automatically reinstalled after each `resume`, so every `yield` throughout the recursive traversal is caught.

### Mutable State as an Effect

Model readable/writable state using effects and a mutable variable. Here we use the parameterized `State` effect with `int` as the state type:

```
effect State 'a =
  get : unit -> 'a
  put : 'a -> unit
end

let mut state = 0

handle
  perform put 10;
  let x = perform get () in
  perform put (x + 1);
  perform get ()
with
| return x -> x
| get () k -> resume k state
| put v k ->
  state := v;
  resume k ()
```

The handler intercepts `get` and `put` operations, using a mutable variable `state` to track the current value. The body sets the state to `10`, reads it back, increments it to `11`, and reads again. Result: `11`.

A simpler stateless variant always returns a fixed value for `get`:

```
handle
  let x = perform get () in
  x + 1
with
| return x -> x
| get () k -> resume k 42
```

Result: `43`. The handler acts as a dependency injection point -- the body does not know where the value comes from.

### Non-Determinism with Multi-Shot Continuations

Use `copy_continuation` to explore all branches of a non-deterministic computation:

```
let rec append xs ys =
  match xs with
  | [] -> ys
  | x :: rest -> x :: append rest ys

effect Choice =
  choose : unit -> bool
end

handle
  let x = perform choose () in
  let y = perform choose () in
  if x && y do 1 else 0
with
| return x -> [x]
| choose () k ->
  let k2 = copy_continuation k in
  let a = resume k true in
  let b = resume k2 false in
  append a b
```

Result: `[1; 0; 0; 0]` -- all four combinations of `(true, true)`, `(true, false)`, `(false, true)`, `(false, false)`, evaluated through `if x && y do 1 else 0`.

### Exceptions as Effects

Early exit with a value, exactly like traditional exceptions:

```
effect Exn =
  raise : string -> 'a
end

let safe_divide a b =
  try
    if b = 0 do perform raise "division by zero" end
    else a / b
  with
  | raise msg -> 0

safe_divide 10 2   -- 5
safe_divide 10 0   -- 0
```

The `'a` return type on `raise` means any code after `perform raise` is unreachable -- the type checker is satisfied regardless of what the surrounding context expects.

### Logging

A logging effect that can be handled in different ways depending on context:

```
effect Log =
  log : string -> unit
end

let computation () =
  perform log "starting";
  let result = 21 * 2 in
  perform log "done";
  result

-- Handle by collecting log messages into a list
let mut messages = [] in
let result =
  handle
    computation ()
  with
  | return x -> x
  | log msg k ->
    messages := msg :: messages;
    resume k ()
in
(result, List.rev messages)
-- (42, ["starting"; "done"])
```

The same `computation` function can be handled differently in different contexts -- silently discarding logs, printing them, or collecting them into a list -- without changing the function itself:

```
-- Handle by silently discarding all log messages
handle
  computation ()
with
| return x -> x
| log msg k -> resume k ()
```

## Deep Handlers

MiniML implements **deep handlers**. This means that when a handler continues a computation, the handler is automatically reinstalled for any subsequent `perform` calls in that computation. You do not need to explicitly re-wrap the body.

```
effect Counter =
  tick : unit -> int
end

let mut count = 0 in
handle
  let a = perform tick () in
  let b = perform tick () in
  let c = perform tick () in
  (a, b, c)
with
| return x -> x
| tick () k ->
  count := count + 1;
  resume k count
```

Each `perform tick ()` is caught by the same handler. The handler increments the counter and continues with the new value. Result: `(1, 2, 3)`.

With a **shallow** handler (which MiniML does not use), only the first `perform` would be caught, and you would need to manually reinstall the handler for subsequent operations.

## How Seq Uses Effects

MiniML's lazy sequence library (`Seq`) is powered by algebraic effects under the hood. Sequences are defined as functions that take a callback:

```
type 'a seq = ('a -> unit) -> unit
```

Operations like `Seq.take` and `Seq.take_while` use an internal `SeqStop` effect for early termination:

```
effect SeqStop =
  __seq_stop : unit -> unit
end

-- Seq.take is implemented roughly as:
let take n s = fn yield ->
  let mut i = 0 in
  try
    s (fn x ->
      if i >= n do perform __seq_stop () end
      else (yield x; i := i + 1))
  with
  | __seq_stop () -> ()
```

This pattern lets sequences be truly lazy -- `Seq.take 5 (Seq.range 0 1000000)` only evaluates the first 5 elements, because the `__seq_stop` effect halts iteration early by abandoning the computation.

Similarly, `Seq.find` and `Seq.any` use effects for early exit:

```
-- Seq.find is implemented roughly as:
let find f s =
  let mut result = None in
  let _ = try
    s (fn x ->
      if f x do (result := Some x; perform __seq_stop ()) end
      else ())
  with
  | __seq_stop () -> ()
  in result
```

You can use sequences without knowing about the effects underneath:

```
Seq.range 0 100
  |> Seq.filter (fn x -> x mod 2 = 0)
  |> Seq.map (fn x -> x * x)
  |> Seq.take 5
  |> Seq.to_list
-- [0; 4; 16; 36; 64]
```

## How Effects Work Under the Hood

Understanding the implementation helps build intuition about performance and behavior.

### Fibers

When you write `handle body with ...`, the runtime creates a new **fiber** -- a lightweight execution context with its own call stack. The body runs on this fiber. When the body performs an effect:

1. The current fiber is suspended (its stack and instruction pointer are frozen).
2. A **continuation** value is created, capturing the suspended fiber.
3. Control returns to the parent fiber (where the handler was installed).
4. The handler receives the operation argument and the continuation.

### Resume

When the handler calls `resume k value`:

1. The value is pushed onto the suspended fiber's stack (as the return value of `perform`).
2. The handler is reinstalled (deep handling).
3. Execution switches back to the suspended fiber.

### copy_continuation

`copy_continuation k` creates a deep copy of the fiber -- duplicating the stack and all call frames. This gives you an independent execution path that can be resumed separately.

## Effect Typing

MiniML tracks effects at compile time. Every function type internally carries an effect annotation on its arrow -- `a -[eff]-> b` -- describing which effects the function may perform. Effects are inferred by default, but you can also write explicit effect annotations when you want to.

### Unhandled Effects Are Compile Errors

If you perform an effect at the top level without an enclosing handler, the typechecker rejects the program at compile time. You do not need to run the code to discover the mistake:

```
effect Boom =
  boom : unit -> int
end

perform boom ()
-- Compile error: expression has unhandled effects
```

Previously this was only caught at runtime. With effect typing, the compiler tells you immediately that an effect is missing a handler.

### Effects Flow Through Higher-Order Functions

When you pass an effectful callback to a higher-order function, the effect is tracked through the call. The typechecker understands that the enclosing context must handle it:

```
effect Log =
  log : string -> unit
end

-- The Log effect flows through List.map
let results =
  handle
    List.map (fn x -> perform log (show x); x * 2) [1; 2; 3]
  with
  | return x -> x
  | log msg k -> resume k ()
```

If you removed the `handle`/`with` wrapper, the typechecker would reject the code because the `Log` effect would be unhandled. This works with any higher-order function -- `List.map`, `List.fold`, `Seq.filter`, your own abstractions -- without any special annotations.

### Handlers Remove Effects

A `handle`/`with` or `try`/`with` block removes the handled effect from the type. Once all effects are handled, the result is pure:

```
effect Log =
  log : string -> unit
end

-- This function carries the Log effect
let f () = perform log "hello"; 42

-- This expression is pure (Log is handled away)
let result = handle f () with
  | return x -> x
  | log msg k -> resume k ()
```

The typechecker sees that `f ()` has the `Log` effect, and that the handler covers `Log`, so the overall `handle` expression has no remaining effects and can be used freely at the top level or in any pure context.

### Effect Inference Is the Default

By default, effect annotations are inferred automatically behind the scenes:

- Pure functions like arithmetic and builtins carry no effects.
- Functions that call `perform` carry the corresponding effect.
- Functions that call other effectful functions inherit those effects.
- Handlers remove the effects they cover.

You can think of it as the compiler quietly keeping track of what each function might do, so it can reject programs that forget to handle something. Omitting effect annotations means inference works exactly as described above -- the behavior is unchanged from before explicit annotations were introduced.

### Explicit Effect Annotations

While inference handles effects automatically, you can now optionally annotate function return types with the effects they perform using the `/ Effect` syntax. This is useful for documentation, for enforcing intent, and for catching mistakes closer to the source.

The general form is a `/` after the return type, followed by one or more effect names:

```
let f (x: int) : string / IO = ...
```

This declares that `f` takes an `int`, returns a `string`, and performs the `IO` effect.

To mark a function as explicitly pure (performing no effects), use `/ pure`:

```
let g (x: int) : int / pure = ...
```

The typechecker will reject `g` if its body performs any unhandled effects.

Multiple effects are separated by commas:

```
let h (x: int) : int / State int, IO = ...
```

This says `h` may perform both `State int` (a parameterized effect) and `IO`.

Effect variables are also supported, which is useful in polymorphic or type class contexts:

```
let apply (f: 'a -> 'b) (x: 'a) : 'b / 'e = f x
```

Here `'e` is an effect variable -- the function is polymorphic over whatever effects `f` might perform.

#### Effect Annotations on Arrow Types

In type annotations, the `/ Effect` syntax applies to the arrow it follows. For example:

```
int -> string / IO
```

This describes a function from `int` to `string` that performs `IO`. The effect is on the arrow between `int` and `string`.

You can parenthesize arrows with effects when they appear as arguments to other functions:

```
(int -> int / IO) -> string
```

This describes a function that takes an effectful callback (one that performs `IO`) and returns a `string`.

#### When to Use Explicit Annotations

Explicit annotations are entirely optional. Common reasons to use them:

- **Documentation** -- making the effect contract visible in the signature.
- **Enforcement** -- `/ pure` ensures a function stays effect-free as the codebase evolves.
- **Clarity** -- when a function's effects are not obvious from the body.

If you omit the `/`, the typechecker infers effects exactly as before. Existing code requires no changes.

### Effect-Polymorphic Class Methods

Type class methods can use effect variables to be polymorphic over effects. This allows a single class to have instances that are pure and instances that are effectful:

```
class Apply 'f =
  do_thing : 'a -> 'a / 'e
end
```

Here `'e` is an effect variable on the method `do_thing`. An instance can implement `do_thing` as a pure function (where `'e` is empty) or as an effectful function (where `'e` includes specific effects). The caller's context determines which effects must be handled.

### Effects Compose with Type Classes

Effect tracking works seamlessly with `where` constraints. A constrained function can also perform effects, and both the constraint and the effect are tracked:

```
effect Log =
  log : string -> unit
end

let log_show (x: 'a) : unit where Show 'a =
  perform log (show x)

handle
  log_show 42;
  log_show "hello"
with
| return x -> x
| log msg k -> resume k ()
```

The typechecker knows that `log_show` requires `Show 'a` and also performs the `Log` effect. Both are resolved independently -- the type class at the call site, the effect by the enclosing handler.

## Effects vs Other Approaches

### vs Exceptions

Exceptions are a special case of effects where the continuation is always discarded. Effects generalize exceptions by giving the handler the *choice* of whether to resume, and what value to resume with.

```
-- Exceptions: one-way control flow
try dangerous_operation ()
with raise msg -> handle_error msg

-- Effects: two-way control flow
handle computation ()
with op arg k ->
  let result = process arg in
  resume k result
```

### vs Monads (Haskell)

In Haskell, different effects require different monads (`State`, `Reader`, `IO`), and combining them requires monad transformers -- a source of complexity and boilerplate. With algebraic effects, you simply declare the effects you need and handle them independently. They compose naturally:

```
-- Two independent effects, no transformers needed
effect State 'a =
  get : unit -> 'a
  put : 'a -> unit
end
effect Log =
  log : string -> unit
end

handle
  handle
    let x = perform get () in
    perform log $"got {x}";
    perform put (x + 1)
  with
  | return x -> x
  | get () k -> resume k 0
  | put v k -> resume k ()
with
| return x -> x
| log msg k -> print msg; resume k ()
```

### vs Direct Mutation

Direct mutation (global variables, mutable references) is unscoped -- any code can modify shared state, making programs harder to reason about. Effects provide the same power but with explicit scoping: the handler decides how operations are interpreted, and different parts of the program can handle the same effect differently.

```
-- Same code, different interpretations
let computation () =
  let x = perform get () in
  perform put (x + 1)

-- In production: use real mutable state
handle computation () with ...

-- In tests: use a pure mock
handle computation () with ...
```

Effects make side effects explicit in the program structure while keeping the code that *uses* effects clean and direct-style.
