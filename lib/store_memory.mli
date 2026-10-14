(** An in-memory {!Store.S} for tests, examples and offline modes. Every call completes at
    once, so it is trivially atomic within one process. *)

open! Core

module Make (M : Monad.S) : sig
  include Store.S with type 'a io = 'a M.t

  val create : unit -> t
end

(** The store over [Monad.Ident]: plain values. *)
module Direct : sig
  include Store.S with type 'a io = 'a

  val create : unit -> t
end
