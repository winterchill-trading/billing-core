(** A credit note: a document that reduces what an issued invoice charged.

    It credits net amounts against the invoice's lines; tax is recomputed with the rates
    and rounding rules the invoice used, so a credit note mirrors its invoice. Its total
    first reduces the invoice's amount due ([against_due]); the rest ([refund]) was
    already paid and is owed back to the customer. Applying it to the invoice is
    {!Invoice.credit}. *)

open! Core

module Reason : sig
  type t =
    | Duplicate
    | Fraudulent
    | Service_issue
    | Order_change
    | Other of string
  [@@deriving sexp, compare, equal]
end

module Line : sig
  type t =
    { invoice_line : int (** the index of the invoice line credited, from 0 *)
    ; description : string
    ; amount : Money.Amount.t (** net of tax, positive *)
    }
  [@@deriving sexp, compare, equal]
end

type t = private
  { id : Credit_note_id.t
  ; number : string
  ; invoice : Invoice_id.t
  ; customer : Customer_id.t
  ; currency : Money.Currency.t
  ; lines : Line.t list
  ; subtotal : Money.Amount.t
  ; taxes : Invoice.Tax_line.t list
  ; tax_total : Money.Amount.t
  ; total : Money.Amount.t
  ; against_due : Money.Amount.t
  ; refund : Money.Amount.t
  ; reason : Reason.t
  ; issued_on : Date.t
  }
[@@deriving sexp, compare, equal]

(** [create ~id ~number ~issued_on ~reason invoice credits] credits [(line index, net
    amount)] pairs. Refuses an invoice that is not [Open] or [Paid], an unknown line, an
    amount that is not positive or exceeds the line's amount, a line credited twice in
    one note, an empty list, and a total above what the invoice can still be credited. *)
val create
  :  id:Credit_note_id.t
  -> number:string
  -> issued_on:Date.t
  -> reason:Reason.t
  -> Invoice.t
  -> (int * Money.Amount.t) list
  -> (t, Refusal.t) Result.t

(** Credits every positive line of the invoice in full; refused once anything has been
    credited. *)
val full
  :  id:Credit_note_id.t
  -> number:string
  -> issued_on:Date.t
  -> reason:Reason.t
  -> Invoice.t
  -> (t, Refusal.t) Result.t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
