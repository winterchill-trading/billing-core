# examples: billing-core

`quickstart.ml` is the worked example behind the README. `dune test` runs it and compares its
output with `quickstart.expected`.

- If the output changes on purpose, update `quickstart.expected` in the same commit and keep the
  README excerpt in step.
- Keep it small; it is the first code a reader copies.

Run: `opam exec --switch 5.2.0+ox -- dune build --root .. @billing-core/examples/runtest`.
