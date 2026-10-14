(** The id of one payment towards an invoice, chosen by the caller (a provider payment id, a collection attempt id). An invoice records each payment id once. *)

include Ledger.Validated_id.S
