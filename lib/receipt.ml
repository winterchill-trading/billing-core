open! Core

module Repr = struct
  type t =
    { invoice : Invoice_id.Stable.V1.t
    ; invoice_number : string
    ; customer : Customer_id.Stable.V1.t
    ; payment : Payment_id.Stable.V1.t
    ; amount : Money.Amount.Stable.V1.t
    ; paid_at : Time_ns.Stable.Alternate_sexp.V1.t
    ; invoice_total : Money.Amount.Stable.V1.t
    ; paid_to_date : Money.Amount.Stable.V1.t
    ; credited : Money.Amount.Stable.V1.t
    ; balance : Money.Amount.Stable.V1.t
    ; lines : (string * Money.Amount.Stable.V1.t) list
    ; taxes : (string * Money.Amount.Stable.V1.t) list
    }
  [@@deriving bin_io, sexp, compare, equal]
end

type t = Repr.t =
  { invoice : Invoice_id.t
  ; invoice_number : string
  ; customer : Customer_id.t
  ; payment : Payment_id.t
  ; amount : Money.Amount.t
  ; paid_at : Time_ns.Alternate_sexp.t
  ; invoice_total : Money.Amount.t
  ; paid_to_date : Money.Amount.t
  ; credited : Money.Amount.t
  ; balance : Money.Amount.t
  ; lines : (string * Money.Amount.t) list
  ; taxes : (string * Money.Amount.t) list
  }
[@@deriving sexp, compare, equal]

let of_payment (invoice : Invoice.t) payment_id =
  match invoice.number with
  | None ->
    Refusal.invalid_state
      ~what:"invoice"
      ~id:(Invoice_id.to_string invoice.id)
      ~state:(Invoice.Status.to_string invoice.status)
      ~action:"receipt"
  | Some invoice_number ->
    (match
       List.findi invoice.payments ~f:(fun _ (p : Invoice.Payment.t) ->
         Payment_id.equal p.id payment_id)
     with
     | None -> Error (Refusal.Not_found { what = "payment"; id = Payment_id.to_string payment_id })
     | Some (index, payment) ->
       let sum amounts = Money.Amount.sum invoice.currency amounts |> Or_error.ok_exn in
       let paid_to_date =
         sum
           (List.filteri invoice.payments ~f:(fun i _ -> i <= index)
            |> List.map ~f:(fun (p : Invoice.Payment.t) -> p.amount))
       in
       let credited = Invoice.amount_credited invoice in
       let balance =
         Money.Amount.sub_exn (Money.Amount.sub_exn invoice.total paid_to_date) credited
       in
       let balance =
         if Money.Amount.is_negative balance then Money.Amount.zero invoice.currency else balance
       in
       Ok
         { invoice = invoice.id
         ; invoice_number
         ; customer = invoice.customer
         ; payment = payment.id
         ; amount = payment.amount
         ; paid_at = payment.paid_at
         ; invoice_total = invoice.total
         ; paid_to_date
         ; credited
         ; balance
         ; lines =
             List.map invoice.items ~f:(fun item -> item.line.description, item.line.amount)
         ; taxes =
             List.map invoice.taxes ~f:(fun (tax : Invoice.Tax_line.t) ->
               tax.rate.name, tax.amount)
         })
;;

let to_text t =
  let money = Money.Amount.to_string in
  List.concat
    [ [ sprintf "Receipt for invoice %s" t.invoice_number
      ; sprintf "Payment %s" (Payment_id.to_string t.payment)
      ; sprintf "Paid at %s UTC" (Time_ns.to_sec_string t.paid_at ~zone:Time_float.Zone.utc)
      ]
    ; List.map t.lines ~f:(fun (description, amount) ->
        sprintf "  %s: %s" description (money amount))
    ; List.map t.taxes ~f:(fun (name, amount) -> sprintf "  tax %s: %s" name (money amount))
    ; [ sprintf "Invoice total: %s" (money t.invoice_total)
      ; sprintf "This payment: %s" (money t.amount)
      ; sprintf "Paid to date: %s" (money t.paid_to_date)
      ]
    ; (if Money.Amount.is_zero t.credited
       then []
       else [ sprintf "Credited: %s" (money t.credited) ])
    ; [ sprintf "Balance: %s" (money t.balance) ]
    ]
  |> String.concat ~sep:"\n"
;;

module Stable = struct
  module V1 = Repr
end
