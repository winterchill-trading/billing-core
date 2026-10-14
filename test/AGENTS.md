# test: billing-core tests

One file per module (`test_<module>.ml`), plus `test_engine.ml` (worked scenarios),
`test_engine_properties.ml` (random billing days checked against the ledger) and `test_store.ml`
(the conformance suite on `Store_memory`).

Run: `opam exec --switch 5.2.0+ox -- dune build --root .. @billing-core/runtest`, then
`dune promote --root ..` to accept new expect output after reading every diff.

Rules:

- `stable digest` tests pin bin_prot shapes; a changed digest means a new `Stable` version.
- When Quickcheck finds a failure, keep the shrunk case as an expect test with the fix.
