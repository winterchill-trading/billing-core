(** How long a billing period lasts, and where period [n] of a subscription falls.

    Periods are computed from the anchor date by index, never by adding to the previous
    period, so a monthly subscription anchored on 31 January runs 31 Jan, 28 Feb, 31 Mar,
    30 Apr: a short month does not drag every later period to the 28th. *)

open! Core

type t = private
  | Months of int (** calendar months, clamped to the month's last day *)
  | Days of int
[@@deriving sexp, compare, equal, hash]

(** [months n]; fails unless [1 <= n <= 120]. *)
val months : int -> t Or_error.t

(** [days n]; fails unless [1 <= n <= 3660]. *)
val days : int -> t Or_error.t

(** One calendar month. *)
val monthly : t

(** Three calendar months. *)
val quarterly : t

(** Twelve calendar months. *)
val yearly : t

(** [period t ~anchor ~index] is the [index]th period (from 0) of a schedule that starts
    on [anchor]. Raises if [index] is negative. *)
val period : t -> anchor:Date.t -> index:int -> Period.t

(** The index of the period containing [date], or [None] when [date] is before [anchor]. *)
val index_of : t -> anchor:Date.t -> Date.t -> int option

(** ["P1M"], ["P3M"], ["P30D"]: the ISO 8601 duration. *)
val to_string : t -> string

module Stable : sig
  module V1 : sig
    (** Decoding validates the length like {!months} and {!days}. *)
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
