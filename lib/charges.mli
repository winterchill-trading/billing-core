(** The invoice lines a plan produces for a period: in-advance lines for its flat and
    per-seat components, in-arrears lines for its usage components. Components that come
    to zero produce no line. *)

open! Core

(** One [Recurring] line per flat or per-seat component, for [period]. These amounts are
    exact, so nothing rounds. *)
val in_advance : Plan.t -> seats:int -> period:Period.t -> Line.t list

(** One [Usage] line per usage component with at least one record of its meter dated
    (in [zone]) inside [period]. Fails if a volume record is in another currency. *)
val in_arrears
  :  Plan.t
  -> Usage.Record.t list
  -> zone:Time_float.Zone.t
  -> period:Period.t
  -> rounding:Rounding_rules.t
  -> Line.t list Or_error.t
