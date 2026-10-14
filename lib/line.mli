(** One line of an invoice: what is charged (or credited) and why. Amounts are net of tax;
    a negative amount is a credit, such as the unused part of a plan after a downgrade. *)

open! Core

module Kind : sig
  type t =
    | Recurring (** a flat or per-seat component, billed in advance for [period] *)
    | Usage (** a metered component, billed in arrears for [period] *)
    | Proration (** a charge or credit for part of a period after a mid-period change *)
    | One_off (** anything added by hand: a setup fee, a goodwill credit *)
  [@@deriving sexp, compare, equal, enumerate]
end

type t = private
  { code : string (** the plan component's code, or a caller-chosen code *)
  ; description : string
  ; kind : Kind.t
  ; period : Period.t option (** the service period the line pays for *)
  ; quantity : int
  ; unit_price : Unit_price.t option
  ; amount : Money.Amount.t
  }
[@@deriving sexp, compare, equal]

(** Fails if [code] is not id-safe, [description] is empty, [quantity] is negative, the
    unit price is in another currency than [amount], or [amount] is negative on a
    [Recurring] or [Usage] line (only prorations and one-off lines may credit). *)
val create
  :  code:string
  -> description:string
  -> kind:Kind.t
  -> ?period:Period.t
  -> ?unit_price:Unit_price.t
  -> quantity:int
  -> Money.Amount.t
  -> t Or_error.t

val currency : t -> Money.Currency.t

(** The same line with another amount, validated again. *)
val with_amount : t -> Money.Amount.t -> t Or_error.t

module Stable : sig
  module V1 : sig
    (** Decoding validates like {!create}. *)
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
