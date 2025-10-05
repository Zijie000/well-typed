# Well‑Typed: Small Typed Language Implementations in OCaml

## Overview 🏜️
> I am doing this project to put into practice the knowledge from *Types and Programming Languages* by Benjamin C. Pierce, mainly to demonstrate my understanding of the concepts presented in the book.
- A collection of small, well-scoped language implementations with parsing, type checking, and evaluation:
  - STLC (Simply Typed Lambda Calculus)
    - TAPL Chapter 3 Untyped Arithmetic Expressions
    - TAPL Chapter 5 The Untyped Lambda-Calculus
    -  TAPL Chapter 8 Typed Arithmetic Expressions
    - TAPL Chapter 9 Simply Typed Lambda-Calculus
  - System F (polymorphic lambda calculus)
    - TAPL Chapter 22 Type Reconstruction
    - TAPL Chapter 23 Universal Types
  - F<: (System F with subtyping)
     - TAPL Chapter 15 Subtyping
     - TAPL Chapter 16 Metatheory of Subtyping
     - TAPL Chapter 23 Universal Types
  - Refs (simply typed lambda calculus with references)
    - TAPL Chapter 11 Simple Extensions

- Each language includes:
  - Parser/AST: parse strings to AST and pretty-print
  - Typechecker: static semantics with helpful error messages
  - Evaluator: small-step operational behavior via an interpreter
  - Tests: positive/negative programs to validate behavior

## Repository Layout 🐫
- src/stlc/
  - syntax.ml: Types and terms (Bool, Nat, Arrow). Pretty printers.
  - parser.ml: Lexer + parser for lambdas, if/then/else, succ/pred/iszero, application.
  - typechecker.ml: STLC type inference/checking.
  - evaluator.ml: CBV interpreter using closures.
  - stlc.ml: Public entry points: parse, parse_type, infer, eval.

- src/systemf/
  - syntax.ml: Adds type variables, forall types, type abstractions/applications.
  - parser.ml: Parses term lambdas and type lambdas/applications.
  - typechecker.ml: Type contexts for terms and types; substitution for forall.
  - evaluator.ml: Erasure-style runtime handling for type abstractions/applications.
  - systemf.ml: Public entry points.

- src/fsub/
  - syntax.ml: Adds Top, bounded forall (forall X <: T. U).
  - parser.ml: Parses bounds and type applications with brackets.
  - typechecker.ml: Subtyping (Top, arrows with contravariant domain), bounded quantification.
  - evaluator.ml: Same runtime model as System F (types erased at runtime).
  - fsub.ml: Public entry points.

- src/refs/
  - syntax.ml: Adds Unit, Ref T, ref, !, :=
  - parser.ml: Parses reference operations and assignment.
  - typechecker.ml: Typing rules for references (assignment returns Unit).
  - evaluator.ml: Store-passing interpreter with location allocation.
  - refs.ml: Public entry points.

- tests/
  - run_tests.ml: End-to-end tests for parsing, typing, evaluation; includes error cases.
  - dune: Dune test configuration.

## Getting Started 🐫
Prerequisites
- OCaml (recommended via opam)
- dune (build/test)
- On macOS: `brew install ocaml opam dune`
- With opam:
  - `opam init`
  - `eval $(opam env)`
  - `opam install dune`

## Build and Test 🦂
- From the repository root:
  - `dune build`
  - `dune runtest`
- If you prefer running the test binary directly:
  - `dune exec tests/run_tests.exe`

## Usage Examples 🌵
- STLC:
  - Parse and type-check: (λ is written as backslash)
    - "(\\x:Bool. x) true"  → type: Bool, evaluates to: true
    - "(\\x:Bool. x) 0"     → type error

- System F:
  - "(biglambda X. \\x:X. x)[Bool] true" → type: Bool, eval: true
  - "((biglambda X. \\x:X. x)[Bool]) 0"  → type error

- F<::
  - "(biglambda X <: Top. \\x:X. x)[Bool] true" → type: Bool, eval: true
  - "(biglambda X <: Bool. \\x:X. x)[Nat]"      → type error
  - "(\\x:Top. x) true"                         → type: Top

- Refs:
  - "!(ref 0)" → evaluates to 0
  - "(\\r:Ref Nat. (\\x:Unit. !r) (r := succ !r)) (ref 0)" → type: Nat, eval: 1
  - "(\\r:Ref Nat. r := true) (ref 0)" → type error

## Design Notes
- Parsers: simple, hand-written lexers and recursive-descent parsers. ASCII only; use backslash for lambda and keywords like “biglambda” or “forall”.
- Typecheckers:
  - STLC: equality on types; function application checks argument type.
  - System F: type contexts and capture-avoiding substitution for forall.
  - F<:: subtyping with Top, arrow subtyping, and bounded quantification.
  - Refs: typing tracks Ref T, deref, and assignment.
- Evaluators:
  - All languages use CBV closures.
  - System F and F<: erase types at runtime.
  - Refs interpreter threads a store with integer locations.

## Conventions
- Error messages raise exceptions:
  - Parse_error for parsing
  - Type_error for typing
  - Runtime_error for evaluation
- Pretty printers for types and values help testing and debugging.

## Extending
- Add new terms/types in the `syntax.ml` files.
- Update lexer/parser rules accordingly.
- Extend type rules in `typechecker.ml`.
- Update the evaluator with corresponding operational semantics.
- Add tests in `tests/run_tests.ml` to cover success and failure cases.

## License
- See LICENSE for details.
