(** An invoice's internal id. The customer-facing number is assigned separately, at finalisation (see {!Numbering}). *)

include Ledger.Validated_id.S
