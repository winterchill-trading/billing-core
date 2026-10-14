# lib: billing-core library

Library `billing-core` (module `Billing_core`). Modules group into layers:

- Ids and values: `*_id`, `Meter`, `Period`, `Interval`, `Unit_price`, `Price`, `Plan`,
  `Price_list`, `Line`, `Tax`, `Rounding_rules`, `Numbering`, `Refusal`.
- Pure state machines: `Subscription`, `Usage`, `Invoice`, `Credit_note`, `Receipt`, `Dunning`;
  helpers `Charges` and `Proration`.
- Ledger: `Booking` builds balanced entries with deterministic keys; it never appends.
- Storage and operations: `Store` (signature), `Store_memory`, `Store_conformance`, `Engine`.

Rules:

- Pure modules take no clock and do no I/O; dates and times are arguments.
- Every transition bumps `revision`; stores save by compare-and-set on it.
- Totals derive from items through `Invoice.Totals.compute`; credit notes use the same function.
- New exports go in both `billing_core.ml` and `billing_core.mli`, with a doc comment.
