(** Scenarios every {!Store.S} implementation must pass: insert-once for every record,
    compare-and-set saves by revision, listings and their filters, usage time ranges, and
    gapless idempotent numbering. A service runs them against its database store in its
    own test suite, next to {!Store_memory}.

    Give it a fresh, empty store; the scenarios use ids starting with [conformance-]. *)

open! Core

module Make (M : Monad.S) (Store : Store.S with type 'a io = 'a M.t) : sig
  (** [Ok ()] when every scenario passes, otherwise the first difference. *)
  val check : Store.t -> unit Or_error.t M.t
end
