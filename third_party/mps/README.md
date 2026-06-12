# MPS — Ravenbrook Memory Pool System (vendored)

The native backend's moving/generational garbage collector (`native_rt/mml_gc.c`)
is built on the [Memory Pool System](https://www.ravenbrook.com/project/mps/), a
production precise/moving GC library. Its AMC pool gives a bump-pointer nursery plus
generational copying collection, with conservative (ambiguous) roots for the stack —
exactly the model the MiniML native runtime needs (see the moving-GC design in the
`path-b-maturity-audit` knowledge doc).

## Provenance

- Upstream: <https://github.com/Ravenbrook/mps>
- Commit: `9fd0577cf1231e61c9801c81499e5d16d0743806` (2024-11-27)
- Platform autodetected here: `xca6ll` (macOS, Apple Silicon / arm64, clang).

## What is vendored

`code/` contains the **library** subset only — `mps.c` (the single-translation-unit
amalgamation) plus every `.c` it conditionally `#include`s across platforms, and all
public/internal `.h` headers. The upstream test suite, tools, manual, and build
scripts are **not** vendored. The whole library compiles as one object:

    clang -c -O2 code/mps.c -o mps.o

## Build integration

`mps.c` is compiled once to a cached object and linked with `native_rt/mml_gc.c`.
See the `test-mml-gc` target in the top-level `Makefile`. (Allocation is not yet
routed through MPS — the runtime still uses Boehm; MPS is wired in behind a flag in
a later increment.)

## Local patches

- `code/mpsliban.c` — `mps_clock()`/`mps_clocks_per_sec()` changed from ANSI `clock()`
  to `clock_gettime(CLOCK_MONOTONIC)`. On macOS `clock()` is a `getrusage()` syscall,
  and MPS polls the clock on the allocation path for incremental-collection
  scheduling, so the stock plinth made allocation syscall-bound (it dominated the
  profile). `clock_gettime(CLOCK_MONOTONIC)` is a fast commpage read here. `mpsliban.c`
  itself documents supplying a higher-resolution clock; this is that. Search
  "LOCAL PATCH (MiniML)" in the file. No other vendored files are modified.

## License

MPS is distributed under the Ravenbrook open-source license (BSD-2-Clause-style); see
[`license.txt`](license.txt). Copyright (c) Ravenbrook Limited.
