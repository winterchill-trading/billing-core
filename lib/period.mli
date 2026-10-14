(** A billing period: the half-open range of dates [\[start, stop)].

    [stop] is the first day of the next period, so consecutive periods share a boundary
    and never overlap: a monthly period from 31 January is [\[2026-01-31, 2026-02-28)]. *)

open! Core

type t = private
  { start : Date.t
  ; stop : Date.t
  }
[@@deriving sexp, compare, equal, hash]

(** Fails unless [start < stop]. *)
val create : start:Date.t -> stop:Date.t -> t Or_error.t

(** [create] that raises. *)
val create_exn : start:Date.t -> stop:Date.t -> t

(** The number of days in the period, at least one. *)
val days : t -> int

(** Whether [date] falls in the period ([start <= date < stop]). *)
val contains : t -> Date.t -> bool

(** The last day inside the period, the day before [stop]. *)
val last_day : t -> Date.t

(** ["2026-01-31..2026-02-27"]: the first and the last day inside the period. *)
val to_string : t -> string

module Stable : sig
  module V1 : sig
    (** Decoding validates like {!create}. *)
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
