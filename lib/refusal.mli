(** Why a billing operation was refused, as data a service can map to an error code and an
    HTTP status without parsing messages. Validation of new values (a plan, a line)
    returns [Or_error]; state transitions return [Refusal.t]. *)

open! Core

type t =
  | Invalid_state of
      { what : string (** ["invoice"], ["subscription"], ... *)
      ; id : string
      ; state : string
      ; action : string
      } (** the action is not allowed in the current state *)
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
  (** a draft whose lines sum below zero cannot be issued *)
  | Payment_conflict of Payment_id.t
  (** a payment id already recorded with another amount *)
  | Usage_conflict of Usage_id.t (** a usage id already recorded with another body *)
  | Period_closed of
      { usage : Usage_id.t
      ; billed_through : Date.t
      } (** usage dated in a period that was already invoiced *)
  | Not_found of
      { what : string
      ; id : string
      }
  | Stale of
      { what : string
      ; id : string
      } (** another writer saved a newer revision first; reload and retry *)
  | Invalid of Error.t (** a value failed validation *)
[@@deriving sexp_of]

(** A stable upper-case code: [INVALID_STATE], [CURRENCY_MISMATCH], [OVERPAYMENT],
    [OVER_CREDIT], [NEGATIVE_TOTAL], [PAYMENT_CONFLICT], [USAGE_CONFLICT],
    [PERIOD_CLOSED], [NOT_FOUND], [STALE], [INVALID]. *)
val code : t -> string

(** The refusal as an error, for callers that work in [Or_error]. *)
val to_error : t -> Error.t

(** The value, or raise the refusal as an error. For tests, examples and scripts. *)
val ok_exn : ('a, t) Result.t -> 'a

(** [of_or_error r] maps [Error e] to [Invalid e]. *)
val of_or_error : 'a Or_error.t -> ('a, t) Result.t

(** [invalid_state ~what ~id ~state ~action] builds an [Invalid_state] error result. *)
val invalid_state
  :  what:string
  -> id:string
  -> state:string
  -> action:string
  -> ('a, t) Result.t
