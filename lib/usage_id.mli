(** The caller-chosen id of a usage record. Ingesting the same id twice records it once, so a producer can resend after a timeout. *)

include Ledger.Validated_id.S
