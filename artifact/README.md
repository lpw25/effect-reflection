# Effect Reflection: artifact

OxCaml implementation of the effect reflection library from the paper
"Effect Reflection", together with the effects and handlers used as
examples in the paper.

- `reflect/`: the library. `reflect.mli` follows the figures in the
  section "Effect reflection in practice"; `reflect.ml` follows the
  appendix "OxCaml implementation", adjusted to use the standard
  library's `Effect.Deep.match_with` because released OxCaml is based
  on OCaml 5.2, which lacks the OCaml 5.3 `effect` match syntax used in
  the paper.
- `examples/`: the effects and handlers from the paper. `state.ml`
  (integer state), `generator.ml` (integer generators), `scheduler.ml`
  (cooperative threads), `swap.ml` (a function using two pieces of
  state, with nested handlers). `local_list.ml` provides the `iter` with a local
  function parameter that the paper assumes of `List.iter`.

The `Sum` and `Select` functors in the library implement the sums of
signatures and the `outl`/`outr` projections of the calculus; the paper
discusses these only in the calculus and its semantics.

## Install opam

Instructions: <https://opam.ocaml.org/doc/Install.html>

    bash -c "sh <(curl -fsSL https://opam.ocaml.org/install.sh)"
    opam init

Or use a system package: `apt install opam`, `dnf install opam`,
`brew install opam`.

## Install OxCaml and dune

Instructions: <https://oxcaml.org/get-oxcaml/> (adapted here to create
a local switch). From this directory:

    opam update --all
    opam switch create . 5.2.0+ox --repos ox=git+https://github.com/oxcaml/opam-repository.git,default
    eval $(opam env)
    opam install dune

Creating the switch builds the OxCaml compiler, which takes a
while.

## Build and run

    dune build
    dune test

The `test/` directory has one test program per runnable example
(`test_state.ml`, `test_generator.ml`, `test_scheduler.ml`), each
printing its results.

## Note on `reify`

The released OxCaml runtime does not yet support effect handlers that
close over local values, so `Reflection(E).reify` type-checks with the
type given in the paper but is a stub that raises `Failure` if
executed. `reify_global` is fully functional. The state, generator and
scheduler handlers in `examples/` therefore use `reify_global`, which
is preferred here because it is runnable; `state.ml` also contains the
paper's `reify`-based handler, which the nested two-state example in
`swap.ml` needs and which type-checks but cannot be run.
