(** A plan: what a subscriber pays, in one currency, every interval.

    A plan is a list of components, each with a code (printed on invoice lines and used to
    choose the income account), a description and a {!Price.t}. A plan may also grant a
    free trial and list the features it unlocks (entitlement names the caller checks;
    billing does not interpret them). *)

open! Core

module Component : sig
  type t = private
    { code : string (** an id-safe name, unique within the plan, e.g. [platform] *)
    ; description : string
    ; price : Price.t
    }
  [@@deriving sexp, compare, equal]

  (** Fails if [code] is not 1-128 characters of [A-Z a-z 0-9 _ - . : / @ +] or
      [description] is empty. *)
  val create : code:string -> description:string -> Price.t -> t Or_error.t
end

type t = private
  { id : Plan_id.t
  ; name : string
  ; currency : Money.Currency.t
  ; interval : Interval.t
  ; components : Component.t list
  ; trial_days : int
  ; features : string list
  }
[@@deriving sexp, compare, equal]

(** Fails unless there is at least one component, every component is priced in [currency],
    component codes are unique, [name] is not empty and [trial_days] (default 0) is
    between 0 and 365. *)
val create
  :  id:Plan_id.t
  -> name:string
  -> currency:Money.Currency.t
  -> interval:Interval.t
  -> ?trial_days:int
  -> ?features:string list
  -> Component.t list
  -> t Or_error.t

(** A plan of one flat component, the common case: [flat ~id ~name ~interval amount]. *)
val flat
  :  id:Plan_id.t
  -> name:string
  -> interval:Interval.t
  -> ?trial_days:int
  -> ?features:string list
  -> Money.Amount.t
  -> t Or_error.t

(** The sum of the components billed in advance for [seats] seats: what one full period
    costs before usage and tax. *)
val recurring_total : t -> seats:int -> Money.Amount.t

module Stable : sig
  module V1 : sig
    (** Decoding validates like {!create}. *)
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
