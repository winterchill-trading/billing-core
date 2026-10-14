(** The price of one unit of something (a seat, an API call, a gigabyte), exact and
    possibly finer than the currency's minor unit: [0.0015 EUR] per call is fine.

    A unit price is never rounded on its own. {!extend} multiplies it by a quantity and
    rounds the product once, so a million calls at [0.0015 EUR] cost exactly
    [1500.00 EUR]. *)

open! Core

type t [@@deriving sexp, compare, equal]

(** [create currency per_unit] with [per_unit] in major units ([Q.of_string "0.0015"]).
    Fails if [per_unit] is negative. *)
val create : Money.Currency.t -> Q.t -> t Or_error.t

(** [of_string currency "0.0015"]; also accepts a fraction such as ["1/3"]. *)
val of_string : Money.Currency.t -> string -> t Or_error.t

(** The amount as a price per unit. Fails on a negative amount. *)
val of_amount : Money.Amount.t -> t Or_error.t

(** The currency of the price and of every amount it extends to. *)
val currency : t -> Money.Currency.t

(** The price in major units, exactly. *)
val per_unit : t -> Q.t

(** [extend t ~quantity ~rounding] is [per_unit * quantity] rounded once to a minor unit. *)
val extend : t -> quantity:int -> rounding:Money.Rounding.t -> Money.Amount.t

(** The exact product [per_unit * quantity] in major units, for callers that add several
    products before rounding once. *)
val extend_exact : t -> quantity:int -> Q.t

(** ["0.0015 EUR"]; a non-terminating price prints as a fraction (["1/3 EUR"]). *)
val to_string : t -> string

module Stable : sig
  module V1 : sig
    (** Bin_prot: the currency and the price as text. Decoding validates. *)
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
