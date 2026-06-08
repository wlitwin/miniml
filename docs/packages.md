# Projects & packages

MiniML's build and package system (driven by the `mml` binary) is modelled on
Go's: decentralized, no central registry, deterministic version selection, and
no code execution at fetch time.

## A project

A project is a directory with an `mml.mod` manifest and `.mml` files:

```
myapp/
  mml.mod
  util.mml        → module Util   (pub declarations are its exports)
  greeter.mml     → module Greeter
  main.mml        the entry program
```

Each file `foo.mml` is the module `Foo`; other files use it through the ordinary
qualified syntax — `Foo.bar`, `open Foo` — there is no separate `import`
statement. `main.mml` is the entry. The build infers the dependency order from
these references and rejects cycles.

```sh
mml run   myapp        # build and run
mml build myapp        # native executable
mml check myapp        # diagnostics, attributed to each file:line:col
```

## The manifest

```
module github.com/me/myapp          # this module's import path (a repo URL)
mml 0.1                             # toolchain version (advisory)
require github.com/u/lib v1.2.0     # a dependency, at its MINIMUM version
replace github.com/u/lib => ../lib  # optional: use a local checkout instead
```

A module is identified by its **import path, which is its git URL** — there is
no central index. A dependency's `pub` modules join the build flat (referenced
as `Lib.thing`); a module-name collision between dependencies is an error.

## Dependencies

```sh
mml get github.com/u/lib            # fetch the latest tagged version, add a require
mml get github.com/u/lib@v1.2.0     # fetch a specific version
```

- **Versions are git tags** (`v1.2.0`, semver).
- **Selection is Minimal Version Selection (MVS):** each module pins the minimum
  it needs; the build takes the maximum of those minimums across the whole graph.
  Deterministic and reproducible with no lockfile and no solver — the trade-off
  is that you stay on old versions until a `require` bumps them (run `mml get` to
  move up).
- **Fetching** is `git clone` of the tag into a module **cache** under
  `$MML_CACHE` (default `~/.mml/cache`); each version is downloaded once and
  builds are offline thereafter. Nothing runs at fetch time — the tree is data.
- **`mml.sum`** records a content hash of each fetched dependency; the first
  build writes it, later builds verify it, so a moved tag or tampered cache is
  caught.

## Status / not yet

- Module-member / qualified-path granularity beyond whole-file modules.
- Incremental (separate) compilation — builds are currently whole-program.
- `mml test`, a `replace`-free private-host scheme, and a hosted checksum
  database are future work.
