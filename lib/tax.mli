(** The tax hook. Billing does not know tax law: the caller supplies a function that says
    which rates apply to a line for a customer (by the customer's country, the
    component's code, an exemption on file), and the invoice computes, rounds and totals
    the tax. Rates are exclusive: tax is added on top of the line amounts. *)

open! Core

module Rate : sig
  type t = private
    { name : string (** an id-safe name, e.g. [vat-de-standard]; also names the account *)
    ; bps : int (** basis points: 1900 is 19 % *)
    }
  [@@deriving sexp, compare, equal, hash]

  (** Fails unless [name] is id-safe and [0 <= bps <= 10_000]. *)
  val create : name:string -> bps:int -> t Or_error.t

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end
end

type t

(** [create ~name rates_for]: [rates_for ~customer line] lists the rates to apply to
    [line]; an empty list means the line is untaxed. [name] is for logs. *)
val create : name:string -> (customer:Customer_id.t -> Line.t -> Rate.t list) -> t

(** No tax on anything. *)
val none : t

(** The same rate on every line. *)
val flat : Rate.t -> t

(** The rates for a line. *)
val rates : t -> customer:Customer_id.t -> Line.t -> Rate.t list

val name : t -> string
