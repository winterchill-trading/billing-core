(** A receipt: proof of one payment towards an issued invoice, as data a service renders
    to PDF or e-mail. Built from the invoice, so it never disagrees with it; store it if
    the receipt must stay byte-identical after later credits. *)

open! Core

type t =
  { invoice : Invoice_id.t
  ; invoice_number : string
  ; customer : Customer_id.t
  ; payment : Payment_id.t
  ; amount : Money.Amount.t
  ; paid_at : Time_ns.Alternate_sexp.t
  ; invoice_total : Money.Amount.t
  ; paid_to_date : Money.Amount.t (** every payment up to and including this one *)
  ; credited : Money.Amount.t (** credit notes applied when the receipt was built *)
  ; balance : Money.Amount.t (** what was left to pay after this payment, at least zero *)
  ; lines : (string * Money.Amount.t) list (** the invoice lines, description and net *)
  ; taxes : (string * Money.Amount.t) list (** tax per rate name *)
  }
[@@deriving sexp, compare, equal]

(** The receipt for [payment]. [Not_found] if the invoice has no such payment;
    [Invalid_state] if it was never issued. *)
val of_payment : Invoice.t -> Payment_id.t -> (t, Refusal.t) Result.t

(** A plain-text rendering, one fact per line, for logs, e-mails and tests. *)
val to_text : t -> string

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
