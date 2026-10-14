(** Usage records and their idempotent ingestion.

    A producer (an API gateway, a card processor) reports usage as records with ids it
    chooses. Ingesting a record whose id is already known is a duplicate when the body is
    the same and a {!Refusal.Usage_conflict} otherwise, so a producer can resend after a
    timeout without double billing. A period's usage is the records whose date, in the
    billing time zone, falls inside it. *)

open! Core

module Value : sig
  type t =
    | Units of int (** a count: calls, messages, gigabytes; never negative *)
    | Volume of Money.Amount.t (** money processed; negative for a refund *)
  [@@deriving sexp, compare, equal]
end

module Record : sig
  type t = private
    { id : Usage_id.t
    ; subscription : Subscription_id.t
    ; meter : Meter.t
    ; value : Value.t
    ; occurred_at : Time_ns.Alternate_sexp.t
    }
  [@@deriving sexp, compare, equal]

  (** Fails on negative units. *)
  val create
    :  id:Usage_id.t
    -> subscription:Subscription_id.t
    -> meter:Meter.t
    -> occurred_at:Time_ns.t
    -> Value.t
    -> t Or_error.t

  (** The date the usage happened on, in [zone]. *)
  val date : t -> zone:Time_float.Zone.t -> Date.t

  module Stable : sig
    module V1 : sig
      (** Decoding validates like {!create}. *)
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
    end
  end
end

module Outcome : sig
  type t =
    | Accepted
    | Duplicate (** the same record was ingested before; nothing changed *)
  [@@deriving sexp, compare, equal]
end

(** [check ~existing record] decides what ingesting [record] does, given the record
    already stored under its id, if any. *)
val check : existing:Record.t option -> Record.t -> (Outcome.t, Refusal.t) Result.t

(** One meter's usage over a period. *)
module Summary : sig
  type t =
    { units : int (** the sum of [Units] records *)
    ; volumes : Money.Amount.t list (** [Volume] records, oldest first *)
    ; records : int
    }
  [@@deriving sexp_of]

  (** Sums the records of [meter] dated (in [zone]) inside [period]. *)
  val of_records
    :  Record.t list
    -> meter:Meter.t
    -> zone:Time_float.Zone.t
    -> Period.t
    -> t
end

(** An in-memory log of ingested records, one per id: the pure core of a usage store. *)
module Log : sig
  type t [@@deriving sexp_of]

  val empty : t
  val ingest : t -> Record.t -> (t * Outcome.t, Refusal.t) Result.t
  val find : t -> Usage_id.t -> Record.t option

  (** Every record, oldest [occurred_at] first, ties by id. *)
  val records : t -> Record.t list
end
