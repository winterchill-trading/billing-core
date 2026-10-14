(** Charges and credits for a change in the middle of a period.

    The share of a period left after a change on date [d] is [(stop - d) / (stop - start)]
    in days. A change credits that share of what was billed in advance and charges that
    share of the new price. When the plan stays the same (a seat change) the credit and
    the charge are netted into one line per component. Each line is rounded once with the
    line rounding rule. *)

open! Core

module Policy : sig
  type t =
    | Credit_and_charge (** credit unused time and charge the remaining time *)
    | Charge_only (** charge increases, never credit decreases *)
    | No_proration (** the change only affects the next period *)
  [@@deriving sexp, compare, equal, enumerate]
end

(** The remaining share of [period] from [on]: 1 on the first day, approaching 0 on the
    last. Raises if [on] is outside the period. *)
val remaining : Period.t -> on:Date.t -> Q.t

(** The proration lines for moving from [old_plan] with [old_seats] to [new_plan] with
    [new_seats] on [on], within [period]. Lines carry the period [\[on, stop)]. *)
val lines
  :  policy:Policy.t
  -> rounding:Rounding_rules.t
  -> period:Period.t
  -> on:Date.t
  -> old_plan:Plan.t
  -> old_seats:int
  -> new_plan:Plan.t
  -> new_seats:int
  -> Line.t list
