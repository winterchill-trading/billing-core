(** The plans a business offers, by id. A price list is a value: publishing new prices
    means building a new list, and subscriptions keep a copy of the plan they were sold,
    so a price change never rewrites an existing subscription. *)

open! Core

type t [@@deriving sexp_of]

(** Fails on a duplicate plan id. *)
val create : Plan.t list -> t Or_error.t

(** Every plan, in id order. *)
val plans : t -> Plan.t list

val find : t -> Plan_id.t -> Plan.t option

(** Fails with the unknown id. *)
val find_or_error : t -> Plan_id.t -> Plan.t Or_error.t

(** The plans priced in [currency], in id order. *)
val in_currency : t -> Money.Currency.t -> Plan.t list
