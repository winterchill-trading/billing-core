(** Dunning: collecting an invoice after a failed payment.

    {v
      start --> Waiting --tick on the retry date--> Collecting --payment failed--> Waiting
                  |                                     |  (no retries left) --> Exhausted
                  +--payment succeeded / invoice settled-+--> Recovered / Closed
    v}

    The policy lists the waits, in days, before each retry. A failure starts the process
    ([Mark_past_due], [Notify_customer]); on the retry date {!tick} asks the caller to
    collect once ([Retry_payment]) and waits for the outcome; each further failure moves
    to the next wait. When the waits run out the process is [Exhausted] and the policy's
    final actions are returned. The machine is pure: it returns actions and the caller
    performs them (see {!Engine}). *)

open! Core

module Policy : sig
  type t = private
    { retry_after_days : int list (** waits before retries 1, 2, ...; each at least 1 *)
    ; cancel_subscription : bool (** cancel the subscription when retries run out *)
    ; mark_uncollectible : bool (** write the invoice off when retries run out *)
    }
  [@@deriving sexp, compare, equal]

  (** Fails if a wait is below 1 day or there are more than 20 retries. *)
  val create
    :  retry_after_days:int list
    -> cancel_subscription:bool
    -> mark_uncollectible:bool
    -> t Or_error.t

  (** Retries after 3, 5 and 7 days, then cancel and write off. *)
  val default : t
end

module State : sig
  type t =
    | Waiting of
        { retries_done : int
        ; next_retry_on : Date.t
        }
    | Collecting of { retry : int } (** a retry was requested; waiting for its outcome *)
    | Recovered of Date.t
    | Exhausted of Date.t
    | Closed (** the invoice was settled another way: credited or voided *)
  [@@deriving sexp, compare, equal]

  (** [true] for [Recovered], [Exhausted] and [Closed]. *)
  val is_final : t -> bool
end

module Action : sig
  type t =
    | Mark_past_due
    | Notify_customer of
        { failures : int
        ; next_retry_on : Date.t option (** [None] when no retry is left *)
        }
    | Retry_payment of { retry : int }
    | Cancel_subscription
    | Mark_uncollectible
  [@@deriving sexp, compare, equal]
end

module Event : sig
  type t =
    | Payment_failed of
        { on : Date.t
        ; reason : string
        }
    | Payment_succeeded of { on : Date.t }
    | Invoice_settled (** credited in full or voided *)
  [@@deriving sexp, compare, equal]
end

type t = private
  { invoice : Invoice_id.t
  ; subscription : Subscription_id.t option
  ; state : State.t
  ; failures : (Date.t * string) list (** oldest first *)
  ; revision : int
  }
[@@deriving sexp, compare, equal]

(** A dunning process opened by the first failure. *)
val start
  :  Policy.t
  -> invoice:Invoice_id.t
  -> ?subscription:Subscription_id.t
  -> on:Date.t
  -> reason:string
  -> unit
  -> t * Action.t list

(** Applies an event. A failure reported while [Waiting] (a payment the customer tried
    themselves) restarts the current wait. Refuses any event once the state is final. *)
val apply : Policy.t -> t -> Event.t -> (t * Action.t list, Refusal.t) Result.t

(** On or after the retry date, moves [Waiting] to [Collecting] and asks for a retry;
    otherwise changes nothing. *)
val tick : t -> today:Date.t -> t * Action.t list

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
