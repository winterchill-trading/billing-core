open! Core

module Tax_basis = struct
  type t =
    | Per_rate
    | Per_line
  [@@deriving bin_io, sexp, compare, equal, enumerate]
end

module Repr = struct
  type t =
    { line : Money.Rounding.Stable.V1.t
    ; tax : Money.Rounding.Stable.V1.t
    ; tax_basis : Tax_basis.t
    }
  [@@deriving bin_io, sexp, compare, equal]
end

type t = Repr.t =
  { line : Money.Rounding.t
  ; tax : Money.Rounding.t
  ; tax_basis : Tax_basis.t
  }
[@@deriving sexp, compare, equal]

let default = { line = Half_up; tax = Half_up; tax_basis = Per_rate }

module Stable = struct
  module V1 = Repr
end
