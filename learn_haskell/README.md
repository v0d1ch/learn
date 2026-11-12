# Learn Haskell

Small exercises progressing from continuations to typed state machines. This
project is independent of Hydra. It lives inside the learn repository and uses
that repository's root flake for its development environment.
Existing answers are preserved; this is a learning workspace, not a blank answer sheet.

## Setup with Nix (recommended)

Use the existing flake at the root of the `learn` repository:

```sh
# From the learn repository root:
nix develop
cd learn_haskell
cabal update
cabal build all
```

The shared environment provides Rust and Go as before, plus **GHC 9.6.7**, Cabal,
Git, pkg-config, GMP and zlib for these exercises. The repository's existing
`flake.lock` pins Nixpkgs. There is no separate flake inside `learn_haskell`.
Enable Nix flakes if necessary with
`nix --extra-experimental-features 'nix-command flakes' develop` at the repository root.

The first build needs internet access to the Nix caches, Hackage, and GitHub.
Cabal fetches and builds dependencies; `cabal.project` pins the required Crem fork
commit, and `cabal.project.freeze` pins package versions. No Hydra checkout or
Cardano packages are needed. Full dependency validation remains unfinished:
the initial clean build was stopped at your request.

### Without Nix

Install **GHC 9.6.7**, **cabal-install 3.10.3.0 or newer**, **Git**, and GHC's
normal native build prerequisites (for example through GHCup). Then run
`cabal update` and `cabal build all` from this directory. Use a normal toolchain,
not Hydra's Nix shell. The dependency freeze targets GHC 9.6.7.

## Exercises

| File | Topic | Load |
| --- | --- | --- |
| `exercises/ContExercise.hs` | Functor, Applicative and Monad for Cont | `cabal repl exe:cont` |
| `exercises/ContPredictions.hs` | Predict continuation behaviour | Same REPL, then `:module *ContPredictions` |
| `exercises/FreeMonadExercise.hs` | Tree and Church representations | `cabal repl exe:free-monad` |
| `exercises/HydraProgramExercise.hs` | A standalone model of Hydra's Program | `cabal repl exe:hydra-program` |
| `exercises/CremExercise.hs` | Typed states and allowed transitions | `cabal repl exe:crem` |
| `exercises/CremProgramExercise.hs` | Programs and interpreters inside Crem | `cabal repl exe:crem-program` |

Each executable has its own `Main` module; open one target at a time. Use the
`exe:` prefix to distinguish the `crem` exercise from the dependency library.
Cabal selects dependencies: do not add `-package crem` or load files using
Hydra's `cabal exec` environment.

In the exercise REPL:

```haskell
:main          -- run all checks in this exercise
:reload        -- reload saved edits
```

`ContPredictions` has no test runner. After `:module *ContPredictions`, evaluate
`exercise1`, `exercise2`, etc. To return to the continuation tests, use
`:module *Main`.

You can also run checks without opening GHCi:

```sh
cabal run exe:cont
cabal run exe:free-monad -- tree
cabal run exe:hydra-program -- monad
cabal run exe:crem -- transitions
cabal run exe:crem-program -- interpreter
```

The files list their supported filters. The latest Crem/Program exercise still
contains TODOs: it builds and loads, but its checks intentionally fail until you
implement them. A successful build verifies setup, not completion of the exercises.
Its checks have a one-second timeout; for an accidental loop in the older
exercises, interrupt with Ctrl+C, fix it and `:reload`.

## Sharing and reproducibility

Share the `learn` repository with its root `flake.nix` and `flake.lock`, and this
whole `learn_haskell` directory. Keep both Cabal project files and all exercise
sources. Exclude `dist-newstyle/`, which is a machine-specific build cache.

The Haskell subdirectory also works on its own with the non-Nix setup above.
`cabal sdist` creates a Haskell source archive including the Cabal dependency
pins; use the repository's root flake separately if you want the Nix toolchain.

The dependency pins target GHC 9.6.7. Supporting a different compiler requires
resolving and validating a new freeze file deliberately.
