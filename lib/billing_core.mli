(** Recurring and usage-based billing as pure values and state machines.

    A business publishes a {!Price_list} of {!Plan}s. Each plan has components priced
    flat, per seat, by usage tiers or as a percentage fee ({!Price}). A customer's
    {!Subscription} runs in calendar periods ({!Interval}, {!Period}); flat and per-seat
    components are billed at the start of a period and usage at its end, from {!Usage}
    records ingested once per id. Mid-period changes are prorated by day ({!Proration}).

    An {!Invoice} holds {!Line}s, applies the caller's {!Tax} hook, rounds by the
    {!Rounding_rules} and gets a gapless number at finalisation ({!Numbering}). Payments
    produce {!Receipt}s, failed payments go through {!Dunning}, and {!Credit_note}s reduce
    what an invoice charged. {!Booking} turns each of these into balanced [ledger] entries
    for receivables, fee income, tax and bad debt.

    Nothing here persists. {!Store.S} is what a service implements over its database
    ({!Store_memory} for tests and offline use, {!Store_conformance} to check an
    implementation) and {!Engine} runs the operations over a store, safe to retry and to
    run from several processes.

    {[
      module Billing = Engine.Make (Monad.Ident) (Store_memory.Direct)

      let _subscription, outcome =
        Billing.subscribe store Engine.Config.default subscription ~today ~now
        |> Refusal.ok_exn
      ;;
      (* outcome.invoices: the opening invoice; outcome.entries: its ledger entry *)
    ]}

    The complete program is [examples/quickstart.ml] in the source tree. *)

(** {1 Identifiers} *)

module Customer_id = Customer_id
module Plan_id = Plan_id
module Subscription_id = Subscription_id
module Invoice_id = Invoice_id
module Credit_note_id = Credit_note_id
module Payment_id = Payment_id
module Usage_id = Usage_id
module Meter = Meter

(** {1 Prices and plans} *)

module Unit_price = Unit_price
module Price = Price
module Plan = Plan
module Price_list = Price_list

(** {1 Periods and subscriptions} *)

module Period = Period
module Interval = Interval
module Subscription = Subscription
module Proration = Proration
module Usage = Usage
module Charges = Charges

(** {1 Documents} *)

module Line = Line
module Tax = Tax
module Rounding_rules = Rounding_rules
module Numbering = Numbering
module Invoice = Invoice
module Receipt = Receipt
module Credit_note = Credit_note

(** {1 Collection} *)

module Dunning = Dunning

(** {1 Ledger} *)

module Booking = Booking

(** {1 Storage and operations} *)

module Refusal = Refusal
module Store = Store
module Store_memory = Store_memory
module Store_conformance = Store_conformance
module Engine = Engine
