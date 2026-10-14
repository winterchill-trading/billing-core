open! Core
module Invoice_line = Line

module Reason = struct
  type t =
    | Duplicate
    | Fraudulent
    | Service_issue
    | Order_change
    | Other of string
  [@@deriving bin_io, sexp, compare, equal]
end

module Line = struct
  type t =
    { invoice_line : int
    ; description : string
    ; amount : Money.Amount.Stable.V1.t
    }
  [@@deriving bin_io, sexp, compare, equal]
end

module Repr = struct
  type t =
    { id : Credit_note_id.Stable.V1.t
    ; number : string
    ; invoice : Invoice_id.Stable.V1.t
    ; customer : Customer_id.Stable.V1.t
    ; currency : Money.Currency.Stable.V1.t
    ; lines : Line.t list
    ; subtotal : Money.Amount.Stable.V1.t
    ; taxes : Invoice.Tax_line.Stable.V1.t list
    ; tax_total : Money.Amount.Stable.V1.t
    ; total : Money.Amount.Stable.V1.t
    ; against_due : Money.Amount.Stable.V1.t
    ; refund : Money.Amount.Stable.V1.t
    ; reason : Reason.t
    ; issued_on : Date.Stable.V1.t
    }
  [@@deriving bin_io, sexp, compare, equal]
end

type t = Repr.t =
  { id : Credit_note_id.t
  ; number : string
  ; invoice : Invoice_id.t
  ; customer : Customer_id.t
  ; currency : Money.Currency.t
  ; lines : Line.t list
  ; subtotal : Money.Amount.t
  ; taxes : Invoice.Tax_line.t list
  ; tax_total : Money.Amount.t
  ; total : Money.Amount.t
  ; against_due : Money.Amount.t
  ; refund : Money.Amount.t
  ; reason : Reason.t
  ; issued_on : Date.t
  }
[@@deriving sexp, compare, equal]

let invalid message = Error (Refusal.Invalid (Error.create_s message))

let create ~id ~number ~issued_on ~reason (invoice : Invoice.t) credits =
  let currency = invoice.currency in
  let items = Array.of_list invoice.items in
  let%bind.Result () =
    match invoice.status with
    | Open | Paid -> Ok ()
    | Draft | Void | Uncollectible ->
      Refusal.invalid_state
        ~what:"invoice"
        ~id:(Invoice_id.to_string invoice.id)
        ~state:(Invoice.Status.to_string invoice.status)
        ~action:"credit_note"
  in
  let%bind.Result () =
    if List.is_empty credits
    then invalid [%message "Billing_core.Credit_note: nothing to credit"]
    else if List.contains_dup (List.map credits ~f:fst) ~compare:Int.compare
    then invalid [%message "Billing_core.Credit_note: a line is credited twice"]
    else Ok ()
  in
  let%bind.Result pairs =
    List.map credits ~f:(fun (index, amount) ->
      if index < 0 || index >= Array.length items
      then invalid [%message "Billing_core.Credit_note: no such line" (index : int)]
      else (
        let item = items.(index) in
        let got = Money.Amount.currency amount in
        if not (Money.Currency.equal got currency)
        then Error (Refusal.Currency_mismatch { expected = currency; got })
        else if not (Money.Amount.is_positive amount)
        then
          invalid
            [%message
              "Billing_core.Credit_note: amount must be positive"
                (amount : Money.Amount.t)]
        else if Money.Amount.( > ) amount item.line.amount
        then
          invalid
            [%message
              "Billing_core.Credit_note: credit above the line amount"
                (index : int)
                (amount : Money.Amount.t)]
        else
          Ok
            ( { Line.invoice_line = index; description = item.line.description; amount }
            , { Invoice.Item.line = item.line; rates = item.rates } )))
    |> Result.all
  in
  (* Tax is computed exactly as the invoice computed it: the same rates and rounding rules
     over the credited amounts. *)
  let%bind.Result items =
    List.map pairs ~f:(fun ((credit : Line.t), (item : Invoice.Item.t)) ->
      let%map.Or_error line = Invoice_line.with_amount item.line credit.amount in
      { item with line })
    |> Or_error.all
    |> Refusal.of_or_error
  in
  let mirror = Invoice.Totals.compute ~currency ~rounding:invoice.rounding items in
  let creditable = Money.Amount.sub_exn invoice.total (Invoice.amount_credited invoice) in
  if Money.Amount.( > ) mirror.total creditable
  then Error (Refusal.Over_credit { creditable; requested = mirror.total })
  else (
    let due = Invoice.amount_due invoice in
    let against_due = if Money.Amount.( < ) mirror.total due then mirror.total else due in
    Ok
      { id
      ; number
      ; invoice = invoice.id
      ; customer = invoice.customer
      ; currency
      ; lines = List.map pairs ~f:fst
      ; subtotal = mirror.subtotal
      ; taxes = mirror.taxes
      ; tax_total = mirror.tax_total
      ; total = mirror.total
      ; against_due
      ; refund = Money.Amount.sub_exn mirror.total against_due
      ; reason
      ; issued_on
      })
;;

let full ~id ~number ~issued_on ~reason (invoice : Invoice.t) =
  if not (List.is_empty invoice.credits)
  then
    Error
      (Refusal.Over_credit
         { creditable =
             Money.Amount.sub_exn invoice.total (Invoice.amount_credited invoice)
         ; requested = invoice.total
         })
  else
    create
      ~id
      ~number
      ~issued_on
      ~reason
      invoice
      (List.filter_mapi invoice.items ~f:(fun i item ->
         Option.some_if (Money.Amount.is_positive item.line.amount) (i, item.line.amount)))
;;

module Stable = struct
  module V1 = Repr
end
