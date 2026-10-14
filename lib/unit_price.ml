open! Core

type t =
  { currency : Money.Currency.t
  ; per_unit : Q.t
  }

let compare a b =
  match Money.Currency.compare a.currency b.currency with
  | 0 -> Q.compare a.per_unit b.per_unit
  | c -> c
;;

let equal a b = compare a b = 0

let create currency per_unit =
  if Q.sign per_unit < 0 || not (Q.is_real per_unit)
  then
    Or_error.error_s
      [%message
        "Billing_core.Unit_price: price must be a non-negative number"
          ~per_unit:(Q.to_string per_unit : string)]
  else Ok { currency; per_unit }
;;

let of_string currency s =
  match Money.Decimal_text.q_of_string s with
  | Some q -> create currency q
  | None ->
    Or_error.error_s [%message "Billing_core.Unit_price: malformed price" (s : string)]
;;

let of_amount amount = create (Money.Amount.currency amount) (Money.Amount.to_q amount)
let currency t = t.currency
let per_unit t = t.per_unit
let extend_exact t ~quantity = Q.mul t.per_unit (Q.of_int quantity)

let extend t ~quantity ~rounding =
  Money.Amount.of_q ~rounding t.currency (extend_exact t ~quantity)
;;

let to_string t =
  Money.Decimal_text.q_to_string t.per_unit ^ " " ^ Money.Currency.to_string t.currency
;;

module Repr = struct
  type t =
    { currency : Money.Currency.Stable.V1.t
    ; per_unit : string
    }
  [@@deriving bin_io, sexp]
end

let to_repr (t : t) : Repr.t =
  { currency = t.currency; per_unit = Money.Decimal_text.q_to_string t.per_unit }
;;

let of_repr ({ currency; per_unit } : Repr.t) =
  of_string currency per_unit |> Or_error.ok_exn
;;

let sexp_of_t t = Repr.sexp_of_t (to_repr t)
let t_of_sexp sexp = of_repr (Repr.t_of_sexp sexp)

module Stable = struct
  module V1 = struct
    type nonrec t = t

    let compare = compare
    let equal = equal
    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp

    include
      Binable.Of_binable_without_uuid [@alert "-legacy"]
        (Repr)
        (struct
          type nonrec t = t

          let to_binable = to_repr
          let of_binable = of_repr
        end)
  end
end
