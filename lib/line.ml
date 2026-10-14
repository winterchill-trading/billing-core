open! Core

module Kind = struct
  type t =
    | Recurring
    | Usage
    | Proration
    | One_off
  [@@deriving bin_io, sexp, compare, equal, enumerate]
end

module Repr = struct
  type t =
    { code : string
    ; description : string
    ; kind : Kind.t
    ; period : Period.Stable.V1.t option
    ; quantity : int
    ; unit_price : Unit_price.Stable.V1.t option
    ; amount : Money.Amount.Stable.V1.t
    }
  [@@deriving bin_io, sexp, compare, equal]
end

type t = Repr.t =
  { code : string
  ; description : string
  ; kind : Kind.t
  ; period : Period.t option
  ; quantity : int
  ; unit_price : Unit_price.t option
  ; amount : Money.Amount.t
  }
[@@deriving compare, equal, sexp_of]

let create ~code ~description ~kind ?period ?unit_price ~quantity amount =
  let error message = Or_error.error_s [%message message (code : string)] in
  match Ledger.Validated_id.check code with
  | Error err -> Error (Error.tag err ~tag:"Billing_core.Line: bad code")
  | Ok () ->
    if String.is_empty (String.strip description)
    then error "Billing_core.Line: empty description"
    else if quantity < 0
    then error "Billing_core.Line: negative quantity"
    else if
      Option.exists unit_price ~f:(fun p ->
        not (Money.Currency.equal (Unit_price.currency p) (Money.Amount.currency amount)))
    then error "Billing_core.Line: unit price in another currency"
    else if
      Money.Amount.is_negative amount
      &&
      match (kind : Kind.t) with
      | Recurring | Usage -> true
      | Proration | One_off -> false
    then error "Billing_core.Line: only prorations and one-off lines may credit"
    else Ok { code; description; kind; period; quantity; unit_price; amount }
;;

let validate (r : Repr.t) =
  create
    ~code:r.code
    ~description:r.description
    ~kind:r.kind
    ?period:r.period
    ?unit_price:r.unit_price
    ~quantity:r.quantity
    r.amount
;;

let t_of_sexp sexp = Repr.t_of_sexp sexp |> validate |> Or_error.ok_exn
let currency t = Money.Amount.currency t.amount
let with_amount t amount = validate { t with amount }

module Stable = struct
  module V1 = struct
    type nonrec t = t [@@deriving compare, equal, sexp]

    include
      Binable.Of_binable_without_uuid [@alert "-legacy"]
        (Repr)
        (struct
          type nonrec t = t

          let to_binable t = t
          let of_binable r = validate r |> Or_error.ok_exn
        end)
  end
end
