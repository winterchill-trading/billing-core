(** The ledger entries billing produces. Pure: each function builds a balanced
    {!Ledger.Entry.t} and the caller appends it to its journal. Every entry has a
    deterministic idempotency key, so booking the same event twice is a duplicate the
    journal recognises.

    - Issuing an invoice debits the customer's receivable with the total and credits fee
      income (per component code, net of tax) and tax payable (per rate).
      [billing:invoice:<id>:issue]
    - A payment debits the account the money came from (the customer's own account at
      the bank, or a clearing account for card and bank collections) and credits the
      receivable; a payment on a written-off invoice credits bad debt instead (a
      recovery). [billing:invoice:<id>:payment:<payment id>]
    - A credit note debits fee income and tax payable and credits the receivable with
      the part that reduces the amount due and the refund account with the rest.
      [billing:credit-note:<id>]
    - Writing off debits bad debt and credits the receivable with the amount due.
      [billing:invoice:<id>:write-off]
    - Voiding an issued invoice reverses its issue entry. [billing:invoice:<id>:void]

    Amounts of zero produce no entry ([None]). *)

open! Core

module Chart : sig
  type t =
    { receivable : Customer_id.t -> Ledger.Account_id.t (** an asset per customer *)
    ; income : string -> Ledger.Account_id.t (** income by plan component code *)
    ; tax_payable : Tax.Rate.t -> Ledger.Account_id.t (** a liability per rate *)
    ; bad_debt : Ledger.Account_id.t (** an expense *)
    }

  (** [receivable:billing/<customer>], [income:billing] for every code,
      [liability:tax/<rate name>], [expense:bad-debt]. *)
  val default : t

  (** The accounts to open before booking, with their kinds and purposes. *)
  val accounts
    :  t
    -> customers:Customer_id.t list
    -> codes:string list
    -> rates:Tax.Rate.t list
    -> Ledger.Account.t list
end

(** The key of an invoice's issue entry. *)
val issue_key : Invoice_id.t -> Ledger.Idempotency_key.t Or_error.t

(** The entry for an issued invoice. Fails for a draft or void invoice. *)
val issue : Chart.t -> Invoice.t -> at:Time_ns.t -> Ledger.Entry.t option Or_error.t

(** The entry for a payment the invoice has recorded (so a retry derives the same
    entry). [from] is the account debited. Fails if the invoice has no such payment. *)
val payment
  :  Chart.t
  -> Invoice.t
  -> Payment_id.t
  -> from:Ledger.Account_id.t
  -> Ledger.Entry.t Or_error.t

(** The entry for a credit note against [invoice]. [refund_to] receives the refund part
    (the customer's own account, or a refunds payable account). *)
val credit_note
  :  Chart.t
  -> Invoice.t
  -> Credit_note.t
  -> refund_to:Ledger.Account_id.t
  -> at:Time_ns.t
  -> Ledger.Entry.t option Or_error.t

(** The write-off of an uncollectible invoice: the amount due when it was marked. Fails
    if the invoice was never marked uncollectible. *)
val write_off : Chart.t -> Invoice.t -> at:Time_ns.t -> Ledger.Entry.t option Or_error.t

(** The reversal of a voided invoice's issue entry; [None] if it was voided as a draft or
    had a zero total. Fails unless the invoice is [Void]. *)
val void : Chart.t -> Invoice.t -> at:Time_ns.t -> Ledger.Entry.t option Or_error.t

(** Applies entries to balances in order, as a journal would; use it to check entries
    before appending them, or in tests. *)
val apply : Ledger.Balances.t -> Ledger.Entry.t list -> Ledger.Balances.t Or_error.t
