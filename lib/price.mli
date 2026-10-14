(** How one component of a plan is priced.

    - {!Flat}: a fixed amount per period, billed in advance.
    - {!Per_seat}: a price per seat above an included number, billed in advance.
    - {!Tiered}: usage of a meter priced through tiers, billed in arrears, either
      graduated (each unit at the price of the tier it falls in) or volume (every unit at
      the price of the tier the total reaches).
    - {!Percentage}: a {!Money.Fee} schedule (fixed part, basis points, minimum and
      maximum) over the money volume a meter records, billed in arrears, either on each
      event or on the period's total.

    Every amount a price computes is rounded once, after the exact sum. *)

open! Core

(** One band of a tiered price. *)
module Tier : sig
  type t = private
    { up_to : int option
    (** the last unit (inclusive) in this tier; [None] for the open-ended last tier *)
    ; unit_price : Unit_price.t
    ; flat : Money.Amount.t (** added once when usage reaches this tier *)
    }
  [@@deriving sexp_of, compare, equal]

  (** [create ?up_to ?flat unit_price]. Fails if [up_to < 1] or [flat] is negative or in
      another currency. *)
  val create : ?up_to:int -> ?flat:Money.Amount.t -> Unit_price.t -> t Or_error.t
end

module Tier_mode : sig
  type t =
    | Graduated (** each unit priced by the tier it falls in *)
    | Volume (** every unit priced by the tier the total quantity falls in *)
  [@@deriving sexp, compare, equal, enumerate]
end

module Fee_basis : sig
  type t =
    | Per_event (** the schedule applies to each usage record, minimum and maximum too *)
    | Per_period (** the schedule applies once to the period's net volume *)
  [@@deriving sexp, compare, equal, enumerate]
end

type t = private
  | Flat of Money.Amount.t
  | Per_seat of
      { unit_price : Money.Amount.t
      ; included : int (** seats that cost nothing *)
      }
  | Tiered of
      { meter : Meter.t
      ; mode : Tier_mode.t
      ; tiers : Tier.t list (** ascending [up_to], the last one open-ended *)
      }
  | Percentage of
      { meter : Meter.t
      ; fee : Money.Fee.t
      ; basis : Fee_basis.t
      }
[@@deriving sexp, compare, equal]

(** Fails on a negative amount. *)
val flat : Money.Amount.t -> t Or_error.t

(** Fails on a negative price or a negative [included] (default 0). *)
val per_seat : ?included:int -> Money.Amount.t -> t Or_error.t

(** Fails unless there is at least one tier, every tier is in one currency, the [up_to]
    bounds strictly increase and only the last tier is open-ended. *)
val tiered : Meter.t -> Tier_mode.t -> Tier.t list -> t Or_error.t

(** [basis] defaults to [Per_period]. *)
val percentage : ?basis:Fee_basis.t -> Meter.t -> Money.Fee.t -> t

(** The currency of every amount the price computes. *)
val currency : t -> Money.Currency.t

(** The meter a usage price reads; [None] for flat and per-seat prices. *)
val meter : t -> Meter.t option

(** [true] for flat and per-seat prices, which are billed at the start of a period;
    usage prices are billed at its end. *)
val in_advance : t -> bool

(** [recurring t ~seats] is a flat price's amount, or the per-seat price times the seats
    above the included ones. Zero for usage prices. *)
val recurring : t -> seats:int -> Money.Amount.t

(** [tiered_amount mode tiers ~quantity ~rounding] prices [quantity] units through
    [tiers]. Zero units cost nothing in either mode, flat parts included. Raises if
    [quantity] is negative. *)
val tiered_amount
  :  Tier_mode.t
  -> Tier.t list
  -> quantity:int
  -> rounding:Money.Rounding.t
  -> Money.Amount.t

(** [usage t ~units ~volumes ~rounding] prices a period's usage: [units] (the sum of unit
    records) for a tiered price, [volumes] (one per money record) for a percentage price.
    A percentage price with no records costs nothing, even with a minimum. Zero for flat
    and per-seat prices. Fails if a volume is in another currency. *)
val usage
  :  t
  -> units:int
  -> volumes:Money.Amount.t list
  -> rounding:Money.Rounding.t
  -> Money.Amount.t Or_error.t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
