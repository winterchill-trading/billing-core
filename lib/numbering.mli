(** Document numbers: gapless, human-readable, per series.

    A number is only assigned when a document is finalised, so abandoned drafts leave no
    holes. The store keeps one counter per {!counter} key and hands out the next value
    atomically; this module only formats. With [Yearly] reset the counter key includes
    the year of issue, so numbering restarts at 1 every January. *)

open! Core

module Reset : sig
  type t =
    | Never
    | Yearly
  [@@deriving sexp, compare, equal, enumerate]
end

type t = private
  { prefix : string
  ; width : int (** zero-padded digits *)
  ; reset : Reset.t
  }
[@@deriving sexp, compare, equal]

(** Fails unless [prefix] is 1-16 characters of [A-Z 0-9 -] and [1 <= width <= 12]
    (default 6). [reset] defaults to [Yearly]. *)
val create : ?width:int -> ?reset:Reset.t -> string -> t Or_error.t

(** The key the store counts under: ["INV"], or ["INV-2026"] with yearly reset. *)
val counter : t -> issued_on:Date.t -> string

(** [format t ~issued_on ~seq] is ["INV-2026-000042"] (yearly) or ["INV-000042"]. A
    sequence wider than [width] is printed in full. Raises unless [seq >= 1]. *)
val format : t -> issued_on:Date.t -> seq:int -> string

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
