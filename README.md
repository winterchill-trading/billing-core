# billing-core

Recurring and usage-based billing in OCaml, as pure values and state machines: price
lists of plans (flat fees, per-seat prices, tiered usage, percentage fees with a minimum
and maximum), subscriptions with calendar periods, trials, plan changes and day-based
proration, usage records ingested once per id, invoices with lines, a tax hook, rounding
rules and gapless numbering, payments and receipts, dunning, credit notes, and the
balanced [`ledger`](https://github.com/winterchill-trading/ledger) entries for
receivables, fee income, tax and bad debt. Amounts come from
[`money`](https://github.com/winterchill-trading/money): exact minor units, no floats.
The library targets the [OxCaml](https://oxcaml.org) compiler and the Jane Street `core`
stack.

## Why separate

Banks and payment platforms charge their customers for accounts, seats, API calls and
processed volume, and those charges have to end up in the books exactly once: an
invoice per period however many processes run billing, a usage record counted once
however often it is resent, a credit that never exceeds what was charged, a receivable
that always equals what customers still owe. These rules are the same for every
product, and they are easy to get subtly wrong (month-end periods drifting to the 28th,
tax rounded per line on one document and per rate on another, a payment booked twice
after a timeout). Keeping them in a small library with no storage, HTTP or provider
code means they are tested once, with properties, and any service gets them by
implementing one store signature.

## Install

Not on opam yet. With an OxCaml switch (`5.2.0+ox`) that has the ox opam repository,
pin the library and its organisation dependencies in order:

```sh
opam pin add fixed-decimal https://github.com/winterchill-trading/fixed-decimal.git
opam pin add money https://github.com/winterchill-trading/money.git
opam pin add ledger https://github.com/winterchill-trading/ledger.git
opam pin add billing-core https://github.com/winterchill-trading/billing-core.git
```

Then add `billing-core` to the `libraries` of a dune stanza. Build: `dune build`, test:
`dune test`. No environment variables, no network.

## Quick start

```ocaml
open! Core
open Billing_core
module Store = Store_memory.Direct
module Billing = Engine.Make (Monad.Ident) (Store)

(* [plan] (49.00 EUR a month plus API calls above 10,000 at 0.0008 EUR) and [noon] are
   defined in examples/quickstart.ml. *)
let () =
  let vat = Tax.Rate.create ~name:"vat-standard" ~bps:2000 |> Or_error.ok_exn in
  let config = { Engine.Config.default with tax = Tax.flat vat } in
  let store = Store.create () in
  let subscription =
    Subscription.create ~id:(Subscription_id.of_string "sub-1")
      ~customer:(Customer_id.of_string "customer-1") ~plan
      ~started_on:(Date.of_string "2026-03-01") ()
    |> Or_error.ok_exn
  in
  (* The opening invoice bills March's flat fee in advance. *)
  let _, opening =
    Billing.subscribe store config subscription ~today:(Date.of_string "2026-03-01")
      ~now:(noon "2026-03-01")
    |> Refusal.ok_exn
  in
  (* Usage arrives with producer-chosen ids; a resend is a [Duplicate]. *)
  ignore (Billing.record_usage store config usage_record : _ result);
  (* On 1 April, March closes: its usage plus April's flat fee, one invoice. *)
  let _, closing =
    Billing.bill store config subscription.id ~today:(Date.of_string "2026-04-01")
      ~now:(noon "2026-04-01")
    |> Refusal.ok_exn
  in
  (* [closing.entries] are ledger entries to append to the journal. *)
  ignore (opening, closing)
;;
```

The complete program, with its output checked by `dune test`, is
[examples/quickstart.ml](examples/quickstart.ml):

```text
INV-2026-000001 (open), due 58.80 EUR
  Growth plan                     1  49.00 EUR
  tax vat-standard                   9.80 EUR
(batch-1 Accepted)
(batch-2 Accepted)
(batch-1 Duplicate)
INV-2026-000002 (open), due 73.20 EUR
  API calls above 10,000      25000  12.00 EUR
  Growth plan                     1  49.00 EUR
  tax vat-standard                   12.20 EUR
entry billing:invoice:sub-1@2026-04-01:issue
  Credit income:billing                     61.00 EUR
  Credit liability:tax/vat-standard         12.20 EUR
  Debit  receivable:billing/customer-1      73.20 EUR
...
```

## Modules

| Module | What it is |
| --- | --- |
| `Customer_id`, `Plan_id`, `Subscription_id`, `Invoice_id`, `Credit_note_id`, `Payment_id`, `Usage_id`, `Meter` | Validated ids in the ledger's id alphabet |
| `Unit_price` | An exact price per unit, finer than a minor unit if needed; rounded once when extended |
| `Price` | Flat, per seat (with included seats), tiered usage (graduated or volume) and percentage fees (`Money.Fee`, per event or per period) |
| `Plan`, `Price_list` | Plans of priced components with a currency, an interval, a trial and feature names; plans by id |
| `Interval`, `Period` | Month or day intervals; half-open periods computed by index from the anchor |
| `Subscription` | Trialing, active, past due, cancelled; periods, pending plan changes, seats, prorations |
| `Proration` | Day-based share of a period; credit-and-charge, charge-only or none |
| `Usage` | Usage records (units or money volume), idempotent ingestion, per-period summaries |
| `Charges` | The in-advance and in-arrears lines a plan produces for a period |
| `Line`, `Tax`, `Rounding_rules`, `Numbering` | Invoice lines; the tax hook and exclusive rates; where and how to round; document numbers per series |
| `Invoice` | Draft, open, paid, void, uncollectible; totals, payments, credits, write-off |
| `Receipt` | Proof of one payment, as data, with a plain-text rendering |
| `Credit_note` | Credits against invoice lines, tax mirrored from the invoice, split into amount due and refund |
| `Dunning` | Retry waits after a failed payment, then cancellation and write-off as the policy says |
| `Booking` | Balanced ledger entries for issue, payment, credit note, write-off and void |
| `Refusal` | Every refused transition as data with a stable code |
| `Store`, `Store_memory`, `Store_conformance` | Persistence as a module type, an in-memory store and the scenarios any store must pass |
| `Engine` | The operations over any store and monad: subscribe, record usage, bill, change plan or seats, cancel, pay, fail, dunning ticks, credit, void |

## Rules

- **Periods.** Period `n` starts `n` intervals after the anchor, computed from the
  anchor, so a monthly subscription from 31 January runs 31 Jan, 28 Feb, 31 Mar, 30 Apr.
  Periods are half-open `[start, stop)`.
- **What is billed when.** Flat and per-seat components are billed in advance; usage is
  billed in arrears. The opening invoice pays period 0; closing period `k` raises one
  invoice with its usage, pending prorations and the advance for period `k + 1` (with a
  pending plan change applied). A trial delays the anchor and bills nothing.
- **Proration.** A mid-period change credits the unused share of the old price and
  charges the remaining share of the new one, by days, rounded once per line. Seat
  changes net into one line per component. An immediate cancellation credits the unused
  share of the period's advance with a credit note.
- **Usage.** One record per id: the same id with the same body is a duplicate, with
  another body a conflict. Usage dated before the start of the current period (already
  billed) is refused; `close_after_days` gives producers a grace period.
- **Rounding.** Unit prices extend exactly and round once per line (`Half_up` by
  default). Tax rounds once per rate over the lines it applies to, or per line, as the
  rules say. `total = subtotal + tax`, always; decoding a stored invoice checks it.
- **Numbering.** Numbers are given at finalisation only, per counter (`INV-2026`),
  gapless and idempotent per document, so a retried finalisation gets the same number.
- **Payments.** Idempotent by payment id; never above the amount due. A payment after a
  write-off is a recovery and credits bad debt.
- **Credit notes.** Never above what is left to credit; the part above the amount due is
  a refund.
- **Ledger.** Issue debits the customer's receivable and credits fee income and tax
  payable; payments credit the receivable; credit notes reverse income and tax; write-offs
  move the receivable to bad debt; voids reverse the issue entry. Every entry has a
  deterministic key (`billing:invoice:<id>:issue`, `...:payment:<id>`,
  `billing:credit-note:<id>`, ...), so a retry books nothing twice.
- **Concurrency.** Store writes are insert-once or compare-and-set by revision. The
  engine derives invoice ids from the subscription and the period boundary, so two
  runners closing the same period raise one invoice.

A Quickcheck property drives random days of usage, payments, failures, credits and
cancellations through the engine for three subscriptions, retrying operations, and
checks after every step that each customer's receivable in the ledger equals the amount
due on their open invoices, that billing twice on one day raises nothing, and that
invoice numbers have no gaps.

## Storage

`Store.S` is the persistence contract, in domain types. A service implements it over
its database (one schema per service, idempotent inserts, compare-and-set saves, an
atomic numbering counter) and runs `Store_conformance.Make (M) (Its_store).check`
in its own tests next to `Store_memory`. The library never touches a database.

## Known gaps

- Tax is exclusive only; tax-inclusive prices are not modelled.
- An invoice whose lines sum below zero (a downgrade credit larger than the next
  period's charge) is refused (`NEGATIVE_TOTAL`); there is no customer credit balance
  carried to later invoices yet.
- Usage accepted while its period is being closed can miss that invoice; run closes with
  a grace period (`close_after_days`) longer than producers' delays.
- Changing the interval takes effect at the next period only.
- Discounts and coupons are not modelled.

## Docs

- API documentation: `dune build @doc` (every public value has an odoc comment).
- [CHANGES.md](CHANGES.md), [AGENTS.md](AGENTS.md) for contributors and coding agents.
