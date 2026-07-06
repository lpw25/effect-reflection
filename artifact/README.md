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
  (integer state), `generator.ml` (integer generators), `aio.ml`
  (asynchronous I/O over a stand-in `Deferred` monad), `gen_await.ml`
  (generators forwarding `Await` operations, via `Sum` and `Select`).

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
    opam install dune async

Creating the switch builds the OxCaml compiler, which takes a
while. Installing async pulls in a lot of dependencies and also takes a
long time.

## Build

    dune build

## Note on `reify`

The released OxCaml runtime does not yet support effect handlers that
close over local values, so `Reflection(E).reify` type-checks with the
type given in the paper but is a stub that raises `Failure` if
executed. `reify_global` is fully functional, so the generator and
asynchronous I/O handlers run for real.
