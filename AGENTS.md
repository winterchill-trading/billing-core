# billing-core: agent guide

## Purpose

An OCaml library for recurring and usage-based billing: price lists, subscriptions with
proration, usage ingestion, invoices, payments, receipts, dunning and credit notes. It is pure:
it computes values and balanced `ledger` entries, and leaves persistence to a `Store` the caller
implements.

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | Library `billing-core` (module `Billing_core`) |
| `test/` | Expect tests and Base_quickcheck properties |
| `examples/` | Runnable examples checked by CI |
| `.github/` | CI workflow |

## Build, test, docs

Run from this repository, inside the workspace:

    opam exec --switch 5.2.0+ox -- dune build --root .. @billing-core/all
    opam exec --switch 5.2.0+ox -- dune build --root .. @billing-core/runtest
    opam exec --switch 5.2.0+ox -- dune build --root .. @billing-core/doc

Several agents building at once need `DUNE_BUILD_DIR=_build-billing-core`.

## Where to read more

- `README.md`
- `../oxcaml-claude-skills/AGENTS.md` (workspace-wide guide)
