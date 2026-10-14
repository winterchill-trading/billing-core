open! Core

type t =
  | Invalid_state of
      { what : string
      ; id : string
      ; state : string
      ; action : string
      }
  | Currency_mismatch of
      { expected : Money.Currency.t
      ; got : Money.Currency.t
      }
  | Overpayment of
      { due : Money.Amount.t
      ; offered : Money.Amount.t
      }
  | Over_credit of
      { creditable : Money.Amount.t
      ; requested : Money.Amount.t
      }
  | Negative_total of Money.Amount.t
  | Payment_conflict of Payment_id.t
  | Usage_conflict of Usage_id.t
  | Period_closed of
      { usage : Usage_id.t
      ; billed_through : Date.t
      }
  | Not_found of
      { what : string
      ; id : string
      }
  | Stale of
      { what : string
      ; id : string
      }
  | Invalid of Error.t
[@@deriving sexp_of]

let code = function
  | Invalid_state _ -> "INVALID_STATE"
  | Currency_mismatch _ -> "CURRENCY_MISMATCH"
  | Overpayment _ -> "OVERPAYMENT"
  | Over_credit _ -> "OVER_CREDIT"
  | Negative_total _ -> "NEGATIVE_TOTAL"
  | Payment_conflict _ -> "PAYMENT_CONFLICT"
  | Usage_conflict _ -> "USAGE_CONFLICT"
  | Period_closed _ -> "PERIOD_CLOSED"
  | Not_found _ -> "NOT_FOUND"
  | Stale _ -> "STALE"
  | Invalid _ -> "INVALID"
;;

let to_error t = Error.create_s (sexp_of_t t)
let ok_exn r = Result.map_error r ~f:to_error |> Or_error.ok_exn
let of_or_error r = Result.map_error r ~f:(fun e -> Invalid e)

let invalid_state ~what ~id ~state ~action =
  Error (Invalid_state { what; id; state; action })
;;
