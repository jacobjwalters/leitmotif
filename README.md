# leitmotif

Leitmotif is a small language I created to explore implementing pattern matching and ADTs.

It's a typed, monomorphic language, based off of the simply-typed lambda calculus.
Since it's monomorphic, there is no parametric polymorphism (generics).
This extends to ADTs; you can't define a datatype that has type parameters.

## Installation
You'll need a modern version of [cabal](https://www.haskell.org/cabal/) installed, as well as GHC.

Navigate to the root of this directory, and run:
```sh
cabal install exe:leitmotif
```

## Usage
Run `leitmotif` in the terminal to print the help message.
There's pretty much only two things you can do:
- `leitmotif repl <filename>` to enter a REPL (optionally loading definitions from `<filename>`;
- `leitmotif exec <filename>` to load a file, and run its main function.

# Examples
You can find some code examples in the `examples/` directory in this repo.
