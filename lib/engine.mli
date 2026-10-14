(** Billing operations over a {!Store.S}: the pure state machines glued to storage.

    Every operation is safe to retry and safe to run from several processes at once:
    invoice ids are derived from the subscription and the period boundary they bill,
    numbers are idempotent per document, saves are compare-and-set (a stale save reloads
    and redoes the pure step), and ledger entries carry deterministic keys. The engine
    never appends to a journal: it returns the entries and the caller books them, so a
    retried call returns the same entries again and the journal recognises the duplicates.

    Periods close [close_after_days] after they end (default 0), which leaves producers
    time to report late usage: a usage record dated before the start of the current period
    is refused with {!Refusal.Period_closed}. *)

open! Core

module Config : sig
  type t =
    { tax : Tax.t
    ; rounding : Rounding_rules.t
    ; invoice_numbers : Numbering.t
    ; credit_note_numbers : Numbering.t
    ; payment_terms_days : int (** due date = issue date + this *)
    ; close_after_days : int (** grace before a finished period is closed *)
    ; proration : Proration.Policy.t
    ; dunning : Dunning.Policy.t
    ; chart : Booking.Chart.t
    ; zone : Time_float.Zone.t (** the billing time zone: usage dates and period days *)
    }

  (** No tax, default rounding, [INV-<year>-NNNNNN] and [CN-<year>-NNNNNN] numbers, 14
      days to pay, no grace, credit-and-charge proration, the default dunning policy, the
      default chart, UTC. *)
  val default : t
end

(** What an operation produced. *)
module Outcome : sig
  type t =
    { invoices : Invoice.t list (** raised or changed, in order *)
    ; credit_notes : Credit_note.t list
    ; entries : Ledger.Entry.t list (** to append to the journal, in order *)
    ; actions : Dunning.Action.t list (** dunning actions taken *)
    }
  [@@deriving sexp_of]

  val empty : t
end

module Make (M : Monad.S) (Store : Store.S with type 'a io = 'a M.t) : sig
  type 'a result := ('a, Refusal.t) Result.t M.t

  (** Stores a new subscription (an existing id is returned unchanged) and bills what is
      due on [today], usually the opening invoice. *)
  val subscribe
    :  Store.t
    -> Config.t
    -> Subscription.t
    -> today:Date.t
    -> now:Time_ns.t
    -> (Subscription.t * Outcome.t) result

  (** Records usage once per id. Refuses an unknown subscription and usage dated in a
      period that is already billed. *)
  val record_usage : Store.t -> Config.t -> Usage.Record.t -> Usage.Outcome.t result

  (** Raises every invoice due for one subscription on [today]: the opening invoice and
      one invoice per finished period, oldest first. *)
  val bill
    :  Store.t
    -> Config.t
    -> Subscription_id.t
    -> today:Date.t
    -> now:Time_ns.t
    -> (Subscription.t * Outcome.t) result

  (** {!bill} for every live subscription. *)
  val run
    :  Store.t
    -> Config.t
    -> today:Date.t
    -> now:Time_ns.t
    -> (Subscription_id.t * (Subscription.t * Outcome.t, Refusal.t) Result.t) list M.t

  (** See {!Subscription.change_plan}; proration lines wait for the next invoice. *)
  val change_plan
    :  Store.t
    -> Config.t
    -> Subscription_id.t
    -> Plan.t
    -> effective:[ `Next_period | `Immediately of Date.t ]
    -> (Subscription.t * Line.t list) result

  (** See {!Subscription.set_seats}. *)
  val set_seats
    :  Store.t
    -> Config.t
    -> Subscription_id.t
    -> int
    -> on:Date.t
    -> (Subscription.t * Line.t list) result

  (** Cancels. [`Immediately on] also raises a final invoice for the usage and pending
      lines of the current period up to [on], and, under a crediting proration policy, a
      credit note for the unused share of what the period's invoice billed in advance (the
      refund part goes to [refund_to]). *)
  val cancel
    :  Store.t
    -> Config.t
    -> Subscription_id.t
    -> at:[ `At_period_end | `Immediately of Date.t ]
    -> refund_to:Ledger.Account_id.t
    -> now:Time_ns.t
    -> (Subscription.t * Outcome.t) result

  (** Records a payment and returns its receipt. A payment that settles an invoice in
      dunning ends the dunning and brings a past-due subscription back to active. *)
  val pay
    :  Store.t
    -> Config.t
    -> Invoice_id.t
    -> Invoice.Payment.t
    -> from:Ledger.Account_id.t
    -> (Invoice.t * Receipt.t * Outcome.t) result

  (** Records a failed collection: starts dunning or moves it on, and performs the
      resulting actions (past due, write-off, cancellation). *)
  val payment_failed
    :  Store.t
    -> Config.t
    -> Invoice_id.t
    -> on:Date.t
    -> reason:string
    -> now:Time_ns.t
    -> (Dunning.t * Outcome.t) result

  (** Advances every open dunning process to [today] and returns the retries to attempt
      now, as (invoice, retry number). *)
  val dunning_due : Store.t -> Config.t -> today:Date.t -> (Invoice_id.t * int) list M.t

  (** Issues a credit note ([`Full] or [`Lines]) and applies it to the invoice. Ends any
      dunning when the invoice is settled. *)
  val credit
    :  Store.t
    -> Config.t
    -> Invoice_id.t
    -> id:Credit_note_id.t
    -> reason:Credit_note.Reason.t
    -> [ `Full | `Lines of (int * Money.Amount.t) list ]
    -> issued_on:Date.t
    -> refund_to:Ledger.Account_id.t
    -> now:Time_ns.t
    -> (Credit_note.t * Invoice.t * Outcome.t) result

  (** Voids a draft or an unpaid open invoice and reverses its issue entry. *)
  val void
    :  Store.t
    -> Config.t
    -> Invoice_id.t
    -> now:Time_ns.t
    -> (Invoice.t * Outcome.t) result
end
