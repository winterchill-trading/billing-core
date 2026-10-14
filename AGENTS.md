# billing-core: agent guide

## Purpose

An OCaml library for recurring and usage-based billing: price lists, subscriptions with
proration, usage ingestion, invoices with tax and numbering, payments, receipts, dunning,
credit notes and the ledger entries for all of them. It is pure: it computes values and
balanced `ledger` entries and leaves persistence to a `Store` signature the caller implements.

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | Library `billing-core` (module `Billing_core`): values, state machines, booking, store, engine |
| `test/` | Expect tests, Base_quickcheck properties, stable digests, the store conformance run |
| `examples/` | `quickstart.ml` and its expected output, run by `dune test` and CI |
| `.github/` | CI workflow |

## Build, test, docs

Run from this repository, inside the workspace:

    opam exec --switch 5.2.0+ox -- dune build --root .. @billing-core/all
    opam exec --switch 5.2.0+ox -- dune build --root .. @billing-core/runtest
    opam exec --switch 5.2.0+ox -- dune build --root .. @billing-core/doc

Several agents building at once need `DUNE_BUILD_DIR=_build-billing-core` (resolved from the
workspace root because of `--root ..`). The docs build must produce no warnings; CI fails on any.

Standalone, outside the workspace: add the organisation's opam repository
(`opam repo add winterchill https://github.com/winterchill-trading/opam-repository.git`), install
the published dependencies, then run `dune build` here.

## Conventions

- Format this repository only: `dune build --root .. @billing-core/fmt --auto-promote`. Never
  `dune fmt` at the workspace root.
- Every module has an `.mli`; every public value has an odoc comment.
- Persisted types have `Stable.V1` modules; their bin_prot digests are pinned in the tests. A
  change to a pinned digest needs a new version, not an edited expectation. Decoding validates
  (an invoice's totals are recomputed and checked).
- Validation of new values returns `Or_error`; state transitions return `(_, Refusal.t) Result.t`
  so a service maps them to codes without parsing messages.
- Money is `Money.Amount.t`; prices finer than a minor unit are `Unit_price.t` (exact `Q.t`), and
  every computed amount rounds once, after the exact sum.
- Engine operations must stay safe to retry: derived ids, idempotent numbering, compare-and-set
  saves, deterministic ledger keys. Add a retry to `test/test_engine_properties.ml` for any new
  operation.
- Expect tests and Quickcheck properties for every invariant; leave a short comment for humans
  at any non-obvious choice.
- User-visible changes get a note under `## Unreleased` in `CHANGES.md`.

## Dependencies

- Sibling repositories: `money` (amounts, rounding, fees) and `ledger` (accounts, entries,
  balances). `money` depends on `fixed-decimal`.
- External: `core`, `zarith`, `ppx_jane`.
- CI pin order: `fixed-decimal`, `money`, `ledger`, as in `.github/workflows/ci.yml`.
- Dependents: none in the organisation yet.

## CI

`.github/workflows/ci.yml` runs the `infra` ox-setup action with the pins above, then
`dune build`, `dune test`, the examples, and the odoc docs with warnings as errors. The
`obj-magic` job calls the reusable workflow in `winterchill-trading/infra`. To check a run, use
`gh run list --limit 1` and at most one `gh run watch <id> --exit-status`. Never poll in a loop.

## Pitfalls

- `Time_ns.t` sexps print in the machine's zone; records use `Time_ns.Alternate_sexp.t` and
  stable types `Time_ns.Stable.Alternate_sexp.V1.t`, so expect output matches on macOS and Linux.
- A functor result bound to the name of a library module (`module Engine = Engine.Make (...)`)
  hides that module's `Config` and `Outcome`; tests name the instance `Billing`.
- Constructors of another module's variant need a type annotation or a qualified name inside
  `match` (`match (kind : Kind.t) with`, `Refusal.Invalid`), or the build reports an unbound
  constructor.
- `Binable.Of_binable_without_uuid` needs `[@alert "-legacy"]`; it is how decoding validates
  while keeping the derived wire shape.
- A refusal in the middle of a multi-step engine operation must not drop results already
  produced (entries to book); make every step idempotent and let a retry return them again.
- Other agents' half-written repositories can print errors in a workspace build; filter the
  output for `billing-core` paths.

## Where to read more

- `README.md` (model, rules, known gaps)
- `../specs/repos/billing-core.md` (design summary, when present)
- `../oxcaml-claude-skills/AGENTS.md` and `../oxcaml-claude-skills/GOTCHAS.md`
- Skills in `../oxcaml-claude-skills/skills/`: `banking-standards`, `janestreet-ocaml-style`

## Never do

- Run `opam install`, `upgrade` or `pin` that would change `ocaml-variants`, `core`, `base`,
  `async` or `ppx_jane` on the shared `5.2.0+ox` switch.
- Publish, tag or release.
- Commit credentials or print secrets (the library needs none).
- Edit a pinned digest to make a test pass, or poll CI in a loop.
