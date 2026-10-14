# Changes

All notable changes to this library. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/); versions follow semantic
versioning once released.

## Unreleased

- Plans and price lists: flat, per-seat, tiered usage (graduated or volume) and
  percentage-fee components, exact unit prices, trials and feature names.
- Subscriptions with month or day intervals computed by index from the anchor, pending
  plan changes, seat changes and day-based proration, cancellation now or at period end.
- Usage records ingested once per id, with conflict detection and per-period summaries.
- Invoices with a tax hook, rounding rules (tax per rate or per line), gapless numbering,
  payments idempotent by id, credits, voiding and write-off; receipts per payment.
- Credit notes that mirror their invoice's tax and split into amount due and refund.
- A dunning state machine with retry waits and final actions.
- Ledger entries for issue, payment (and recovery), credit note, write-off and void.
- A store signature with an in-memory implementation and a conformance suite, and an
  engine that bills over any store, safe to retry and to run concurrently.
