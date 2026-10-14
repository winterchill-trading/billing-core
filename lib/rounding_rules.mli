(** Where an invoice rounds, and which way.

    Prices extend exactly and round once per line ([line]). Tax rounds either once per
    rate over the sum of the lines it applies to ([Per_rate], the usual rule for VAT
    invoices and the default) or on each line before summing ([Per_line], which some
    jurisdictions and point-of-sale systems require). Both use [tax]. *)

open! Core

module Tax_basis : sig
  type t =
    | Per_rate
    | Per_line
  [@@deriving sexp, compare, equal, enumerate]
end

type t =
  { line : Money.Rounding.t
  ; tax : Money.Rounding.t
  ; tax_basis : Tax_basis.t
  }
[@@deriving sexp, compare, equal]

(** [Half_up] for lines and tax, tax per rate. *)
val default : t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
