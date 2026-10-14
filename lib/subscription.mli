(** A customer's subscription to a plan, as a pure state machine.

    {v
      Trialing --trial ends, opening invoice--> Active <--recovered-- Past_due
                                                  |  --payment failed-->  |
                                                  +--cancel--> Cancelled <+
    v}

    Periods are numbered from 0 starting at [anchor] (the day the trial ends, or the start
    date without a trial) and laid out by the plan's {!Interval}. Flat and per-seat
    components are billed in advance: the opening invoice pays period 0, and closing
    period [k] bills its usage together with the advance for period [k + 1]. [current] is
    the period whose advance was billed last; everything dated before its start is billed,
    which is [billed_through].

    A plan change for the next period waits in [pending_plan] (usage of the current period
    is still priced by the old plan). An immediate change or a seat change returns
    proration lines and also keeps them in [pending_lines] for the next invoice. Every
    transition increments [revision]. *)

open! Core

module Status : sig
  type t =
    | Trialing
    | Active
    | Past_due (** an invoice is in dunning *)
    | Cancelled
  [@@deriving sexp, compare, equal, enumerate]

  (** Lower-case name, e.g. ["past_due"]. *)
  val to_string : t -> string
end

type t = private
  { id : Subscription_id.t
  ; customer : Customer_id.t
  ; plan : Plan.t (** a copy of the plan as sold *)
  ; seats : int
  ; started_on : Date.t
  ; anchor : Date.t (** the start of period 0 *)
  ; status : Status.t
  ; opened : bool (** the opening invoice has been raised *)
  ; current : int (** the period whose advance was billed last *)
  ; pending_plan : Plan.t option (** takes effect when the current period closes *)
  ; pending_lines : Line.t list (** prorations waiting for the next invoice *)
  ; cancel_at_period_end : bool
  ; ended_on : Date.t option (** the first day without service *)
  ; revision : int
  }
[@@deriving sexp, compare, equal]

(** A new subscription. Without a trial it is [Active] and due its opening invoice at
    once; with one it is [Trialing] until [started_on + trial_days]. Fails if [seats] is
    negative. *)
val create
  :  id:Subscription_id.t
  -> customer:Customer_id.t
  -> plan:Plan.t
  -> ?seats:int (** default 1 *)
  -> started_on:Date.t
  -> unit
  -> t Or_error.t

(** The period [index]. *)
val period : t -> index:int -> Period.t

(** The current period. *)
val current_period : t -> Period.t

(** The first day not yet billed for, once the opening invoice is raised: the start of the
    current period. Usage dated before it can no longer be billed. *)
val billed_through : t -> Date.t option

(** What billing should do for this subscription on [today]. *)
module Due : sig
  type t =
    | Nothing
    | Open (** raise the opening invoice: the advance for period 0 *)
    | Close of int
    (** close this period: bill its usage and, unless the subscription ends with it, the
        advance for the next one *)
  [@@deriving sexp, compare, equal]
end

val due : t -> today:Date.t -> Due.t

(** After the opening invoice: [Active], [opened], and the pending lines (now billed)
    dropped. *)
val opened : t -> (t, Refusal.t) Result.t

(** Whether closing the current period ends the subscription. *)
val ends_with_current_period : t -> bool

(** After closing the current period: either [Cancelled] (when cancellation at the period
    end was requested) or moved to the next period with the pending plan applied and the
    pending lines dropped. *)
val closed : t -> (t, Refusal.t) Result.t

(** Changes the plan. [`Next_period] waits for the current period to close (requesting the
    plan already in force clears a pending change and changes nothing else);
    [`Immediately on] switches now and returns proration lines for the rest of the current
    period, also kept in [pending_lines]. Refuses another currency, an immediate change to
    another interval, and a cancelled subscription. *)
val change_plan
  :  t
  -> Plan.t
  -> effective:[ `Next_period | `Immediately of Date.t ]
  -> policy:Proration.Policy.t
  -> rounding:Rounding_rules.t
  -> (t * Line.t list, Refusal.t) Result.t

(** Changes the seat count on [on], with proration as for an immediate plan change. *)
val set_seats
  :  t
  -> int
  -> on:Date.t
  -> policy:Proration.Policy.t
  -> rounding:Rounding_rules.t
  -> (t * Line.t list, Refusal.t) Result.t

(** [`At_period_end] stops renewal when the current period closes (undone by {!resume});
    [`Immediately on] ends the subscription on [on], which must fall in the current period
    (or anywhere while trialing). Crediting unused time is the caller's choice:
    {!Proration.remaining} gives the share, and a credit note against the invoice that
    billed the period records it. *)
val cancel
  :  t
  -> at:[ `At_period_end | `Immediately of Date.t ]
  -> (t, Refusal.t) Result.t

(** Withdraws a cancellation at the period end. *)
val resume : t -> (t, Refusal.t) Result.t

(** An invoice went into dunning: [Active] becomes [Past_due]. *)
val mark_past_due : t -> (t, Refusal.t) Result.t

(** The overdue invoice was paid: [Past_due] becomes [Active]. *)
val mark_recovered : t -> (t, Refusal.t) Result.t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
