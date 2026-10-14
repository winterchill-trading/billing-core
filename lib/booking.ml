open! Core

module Chart = struct
  type t =
    { receivable : Customer_id.t -> Ledger.Account_id.t
    ; income : string -> Ledger.Account_id.t
    ; tax_payable : Tax.Rate.t -> Ledger.Account_id.t
    ; bad_debt : Ledger.Account_id.t
    }

  let default =
    { receivable =
        (fun customer ->
          Ledger.Account_id.of_string ("receivable:billing/" ^ Customer_id.to_string customer))
    ; income = (fun _ -> Ledger.Account_id.of_string "income:billing")
    ; tax_payable =
        (fun rate -> Ledger.Account_id.of_string ("liability:tax/" ^ rate.Tax.Rate.name))
    ; bad_debt = Ledger.Account_id.of_string "expense:bad-debt"
    }
  ;;

  let accounts t ~customers ~codes ~rates =
    let open Ledger in
    List.concat
      [ List.map customers ~f:(fun c -> Account.create (t.receivable c) Asset)
      ; List.map codes ~f:(fun code ->
          Account.create ~purpose:Fee_income (t.income code) Income)
      ; List.map rates ~f:(fun rate -> Account.create (t.tax_payable rate) Liability)
      ; [ Account.create t.bad_debt Expense ]
      ]
    |> List.dedup_and_sort ~compare:(fun (a : Account.t) (b : Account.t) ->
      Account_id.compare a.id b.id)
  ;;
end

let key fmt = Printf.ksprintf Ledger.Idempotency_key.of_string_or_error fmt
let issue_key id = key "billing:invoice:%s:issue" (Invoice_id.to_string id)

(* Postings from signed amounts: positive means credit. Amounts on one account are
   netted first, and zero nets post nothing. *)
let postings (signed : (Ledger.Account_id.t * Money.Amount.t) list) =
  List.fold signed ~init:Ledger.Account_id.Map.empty ~f:(fun acc (account, amount) ->
    Map.update acc account ~f:(function
      | None -> amount
      | Some sum -> Money.Amount.add_exn sum amount))
  |> Map.to_alist
  |> List.filter_map ~f:(fun (account, amount) ->
    if Money.Amount.is_zero amount
    then None
    else if Money.Amount.is_positive amount
    then Some (Ledger.Posting.credit account amount)
    else Some (Ledger.Posting.debit account (Money.Amount.neg amount)))
;;

let entry ~key ~code ~tag ~reference ~at postings =
  Ledger.Entry.create
    ~idempotency_key:key
    ~kind:(Transfer tag)
    ~code
    ~reference
    ~effective_at:at
    ~recorded_at:at
    postings
;;

let reference (invoice : Invoice.t) =
  Option.value invoice.number ~default:(Invoice_id.to_string invoice.id)
;;

let issue_postings (chart : Chart.t) (invoice : Invoice.t) =
  postings
    (List.concat
       [ [ chart.receivable invoice.customer, Money.Amount.neg invoice.total ]
       ; List.map invoice.items ~f:(fun item -> chart.income item.line.code, item.line.amount)
       ; List.map invoice.taxes ~f:(fun tax -> chart.tax_payable tax.rate, tax.amount)
       ])
;;

let issued_status (invoice : Invoice.t) ~action =
  match invoice.status with
  | Open | Paid | Uncollectible -> Ok ()
  | Draft | Void ->
    Or_error.error_s
      [%message
        "Billing_core.Booking: invoice not issued"
          (action : string)
          (invoice.id : Invoice_id.t)
          (invoice.status : Invoice.Status.t)]
;;

let issue chart (invoice : Invoice.t) ~at =
  let%bind.Or_error () = issued_status invoice ~action:"issue" in
  if Money.Amount.is_zero invoice.total
  then Ok None
  else
    Or_error.try_with_join (fun () ->
      let%bind.Or_error key = issue_key invoice.id in
      entry
        ~key
        ~code:Fee
        ~tag:"billing_invoice"
        ~reference:(reference invoice)
        ~at
        (issue_postings chart invoice)
      |> Or_error.map ~f:Option.some)
;;

let payment (chart : Chart.t) (invoice : Invoice.t) payment_id ~from =
  let%bind.Or_error () = issued_status invoice ~action:"payment" in
  match
    List.find invoice.payments ~f:(fun (p : Invoice.Payment.t) ->
      Payment_id.equal p.id payment_id)
  with
  | None ->
    Or_error.error_s
      [%message "Billing_core.Booking.payment: no such payment" (payment_id : Payment_id.t)]
  | Some payment ->
    Or_error.try_with_join (fun () ->
      let%bind.Or_error key =
        key
          "billing:invoice:%s:payment:%s"
          (Invoice_id.to_string invoice.id)
          (Payment_id.to_string payment.id)
      in
      let settles =
        if Invoice.is_recovery invoice payment.id
        then chart.bad_debt
        else chart.receivable invoice.customer
      in
      entry
        ~key
        ~code:Fee
        ~tag:"billing_payment"
        ~reference:(reference invoice)
        ~at:payment.paid_at
        [ Ledger.Posting.debit from payment.amount
        ; Ledger.Posting.credit settles payment.amount
        ])
;;

let credit_note (chart : Chart.t) (invoice : Invoice.t) (note : Credit_note.t) ~refund_to ~at =
  if Money.Amount.is_zero note.total
  then Ok None
  else
    Or_error.try_with_join (fun () ->
      let%bind.Or_error key = key "billing:credit-note:%s" (Credit_note_id.to_string note.id) in
      let items = Array.of_list invoice.items in
      let%bind.Or_error lines =
        List.map note.lines ~f:(fun line ->
          if line.invoice_line < 0 || line.invoice_line >= Array.length items
          then
            Or_error.error_s
              [%message "Billing_core.Booking: credit note line not on the invoice"]
          else (
            let item = items.(line.invoice_line) in
            Ok (chart.income item.line.code, Money.Amount.neg line.amount)))
        |> Or_error.all
      in
      entry
        ~key
        ~code:Refund
        ~tag:"billing_credit_note"
        ~reference:note.number
        ~at
        (postings
           (List.concat
              [ lines
              ; List.map note.taxes ~f:(fun tax ->
                  chart.tax_payable tax.rate, Money.Amount.neg tax.amount)
              ; [ chart.receivable note.customer, note.against_due; refund_to, note.refund ]
              ]))
      |> Or_error.map ~f:Option.some)
;;

let write_off (chart : Chart.t) (invoice : Invoice.t) ~at =
  match Invoice.written_off invoice with
  | None ->
    Or_error.error_s
      [%message
        "Billing_core.Booking.write_off: invoice never marked uncollectible"
          (invoice.id : Invoice_id.t)]
  | Some due when Money.Amount.is_zero due -> Ok None
  | Some due ->
    Or_error.try_with_join (fun () ->
      let%bind.Or_error key =
        key "billing:invoice:%s:write-off" (Invoice_id.to_string invoice.id)
      in
      entry
        ~key
        ~code:Fee
        ~tag:"billing_write_off"
        ~reference:(reference invoice)
        ~at
        [ Ledger.Posting.debit chart.bad_debt due
        ; Ledger.Posting.credit (chart.receivable invoice.customer) due
        ]
      |> Or_error.map ~f:Option.some)
;;

let void (chart : Chart.t) (invoice : Invoice.t) ~at =
  match invoice.status with
  | Void when Option.is_some invoice.number && not (Money.Amount.is_zero invoice.total) ->
    Or_error.try_with_join (fun () ->
      let%bind.Or_error reverses = issue_key invoice.id in
      let%bind.Or_error key = key "billing:invoice:%s:void" (Invoice_id.to_string invoice.id) in
      Ledger.Entry.create
        ~idempotency_key:key
        ~kind:(Reversal { reverses })
        ~reference:(reference invoice)
        ~effective_at:at
        ~recorded_at:at
        (List.map (issue_postings chart invoice) ~f:Ledger.Posting.flip)
      |> Or_error.map ~f:Option.some)
  | Void -> Ok None
  | Draft | Open | Paid | Uncollectible ->
    Or_error.error_s
      [%message "Billing_core.Booking.void: invoice not void" (invoice.id : Invoice_id.t)]
;;

let apply balances entries =
  List.fold_result entries ~init:balances ~f:(fun b (entry : Ledger.Entry.t) ->
    Ledger.Balances.apply b { seq = Ledger.Balances.next_seq b; event = Posted entry })
;;
