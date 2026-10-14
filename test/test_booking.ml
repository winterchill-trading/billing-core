open! Core
open Billing_core

let eur = Money.Currency.eur
let amount s = Money.Amount.of_string (s ^ " EUR")
let at = Time_ns.of_string_with_utc_offset "2026-03-01 09:00:00Z"
let customer = Customer_id.of_string "c-1"
let vat = Tax.Rate.create ~name:"vat-standard" ~bps:1900 |> Or_error.ok_exn
let chart = Booking.Chart.default
let wallet = Ledger.Account_id.of_string "customer:c-1/eur"
let funding = Ledger.Account_id.of_string "external:sepa"

let opened =
  let accounts =
    Booking.Chart.accounts chart ~customers:[ customer ] ~codes:[ "platform" ] ~rates:[ vat ]
    @ [ Ledger.Account.create ~purpose:Customer_funds wallet Liability
      ; Ledger.Account.create ~allow_negative:true funding Asset
      ]
  in
  let balances =
    List.fold accounts ~init:Ledger.Balances.empty ~f:(fun b account ->
      Ledger.Balances.apply
        b
        { seq = Ledger.Balances.next_seq b; event = Account_opened account }
      |> Or_error.ok_exn)
  in
  let deposit =
    Ledger.Entry.create
      ~idempotency_key:(Ledger.Idempotency_key.of_string "deposit-1")
      ~kind:(Transfer "deposit")
      ~effective_at:at
      ~recorded_at:at
      [ Ledger.Posting.debit funding (amount "1000.00")
      ; Ledger.Posting.credit wallet (amount "1000.00")
      ]
    |> Or_error.ok_exn
  in
  Booking.apply balances [ deposit ] |> Or_error.ok_exn
;;

let issued lines =
  Invoice.draft
    ~id:(Invoice_id.of_string "inv-1")
    ~customer
    ~currency:eur
    ~tax:(Tax.flat vat)
    ~rounding:Rounding_rules.default
    (List.map lines ~f:(fun (kind, s) ->
       Line.create ~code:"platform" ~description:"Platform" ~kind ~quantity:1 (amount s)
       |> Or_error.ok_exn))
  |> Or_error.ok_exn
  |> Invoice.finalize
       ~number:"INV-2026-000001"
       ~issued_on:(Date.of_string "2026-03-01")
       ~due_on:(Date.of_string "2026-03-15")
  |> Result.ok
  |> Option.value_exn
;;

let show balances =
  List.iter
    [ chart.receivable customer
    ; chart.income "platform"
    ; chart.tax_payable vat
    ; chart.bad_debt
    ; wallet
    ]
    ~f:(fun account ->
      printf
        "%-30s %s\n"
        (Ledger.Account_id.to_string account)
        (Money.Amount.to_string (Ledger.Balances.balance balances account eur)))
;;

let some r = Or_error.ok_exn r |> Option.value_exn
let ok r = Result.ok r |> Option.value_exn

let%expect_test "issue, part payment, credit note with refund" =
  let invoice = issued [ Recurring, "100.00" ] in
  let b = Booking.apply opened [ some (Booking.issue chart invoice ~at) ] |> Or_error.ok_exn in
  show b;
  print_endline "-- pay 100.00";
  let p = { Invoice.Payment.id = Payment_id.of_string "p1"; amount = amount "100.00"; paid_at = at } in
  let invoice, _ = Invoice.pay invoice p |> ok in
  let entry = Booking.payment chart invoice p.id ~from:wallet |> Or_error.ok_exn in
  let b = Booking.apply b [ entry ] |> Or_error.ok_exn in
  show b;
  print_endline "-- credit 30.00 net";
  let note =
    Credit_note.create
      ~id:(Credit_note_id.of_string "cn-1")
      ~number:"CN-2026-000001"
      ~issued_on:(Date.of_string "2026-03-05")
      ~reason:Service_issue
      invoice
      [ 0, amount "30.00" ]
    |> ok
  in
  let entry = some (Booking.credit_note chart invoice note ~refund_to:wallet ~at) in
  let b = Booking.apply b [ entry ] |> Or_error.ok_exn in
  show b;
  print_s [%sexp (entry.idempotency_key : Ledger.Idempotency_key.t)];
  [%expect
    {|
    receivable:billing/c-1         119.00 EUR
    income:billing                 100.00 EUR
    liability:tax/vat-standard     19.00 EUR
    expense:bad-debt               0.00 EUR
    customer:c-1/eur               1000.00 EUR
    -- pay 100.00
    receivable:billing/c-1         19.00 EUR
    income:billing                 100.00 EUR
    liability:tax/vat-standard     19.00 EUR
    expense:bad-debt               0.00 EUR
    customer:c-1/eur               900.00 EUR
    -- credit 30.00 net
    receivable:billing/c-1         0.00 EUR
    income:billing                 70.00 EUR
    liability:tax/vat-standard     13.30 EUR
    expense:bad-debt               0.00 EUR
    customer:c-1/eur               916.70 EUR
    billing:credit-note:cn-1
    |}]
;;

let%expect_test "write-off, recovery and void" =
  let invoice = issued [ Recurring, "100.00"; Proration, "-20.00" ] in
  let b = Booking.apply opened [ some (Booking.issue chart invoice ~at) ] |> Or_error.ok_exn in
  let lost = Invoice.mark_uncollectible invoice |> ok in
  let b = Booking.apply b [ some (Booking.write_off chart lost ~at) ] |> Or_error.ok_exn in
  show b;
  print_endline "-- late payment";
  let p = { Invoice.Payment.id = Payment_id.of_string "late"; amount = amount "95.20"; paid_at = at } in
  let recovered, _ = Invoice.pay lost p |> ok in
  let b =
    Booking.apply b [ Booking.payment chart recovered p.id ~from:wallet |> Or_error.ok_exn ]
    |> Or_error.ok_exn
  in
  print_s [%sexp (recovered.status : Invoice.Status.t)];
  show b;
  print_endline "-- void another invoice";
  let other = issued [ Recurring, "10.00" ] in
  let b = Booking.apply opened [ some (Booking.issue chart other ~at) ] |> Or_error.ok_exn in
  let voided = Invoice.void other |> ok in
  let b = Booking.apply b [ some (Booking.void chart voided ~at) ] |> Or_error.ok_exn in
  show b;
  [%expect
    {|
    receivable:billing/c-1         0.00 EUR
    income:billing                 80.00 EUR
    liability:tax/vat-standard     15.20 EUR
    expense:bad-debt               95.20 EUR
    customer:c-1/eur               1000.00 EUR
    -- late payment
    Paid
    receivable:billing/c-1         0.00 EUR
    income:billing                 80.00 EUR
    liability:tax/vat-standard     15.20 EUR
    expense:bad-debt               0.00 EUR
    customer:c-1/eur               904.80 EUR
    -- void another invoice
    receivable:billing/c-1         0.00 EUR
    income:billing                 0.00 EUR
    liability:tax/vat-standard     0.00 EUR
    expense:bad-debt               0.00 EUR
    customer:c-1/eur               1000.00 EUR
    |}]
;;

let%test_unit "the receivable always equals the amount due" =
  let gen =
    let open Quickcheck.Let_syntax in
    let%bind cents = Int.gen_incl 1 100_000 in
    let%bind payments = List.gen_with_length 4 (Int.gen_incl 1 60_000) in
    let%map credit = Int.gen_incl 0 cents in
    cents, payments, credit
  in
  Quickcheck.test gen ~sexp_of:[%sexp_of: int * int list * int] ~f:(fun (cents, payments, credit) ->
    let net = Money.Amount.of_minor_units_int eur cents in
    let invoice = issued [ Recurring, Money.Amount.to_decimal_string net ] in
    let b = Booking.apply opened [ some (Booking.issue chart invoice ~at) ] |> Or_error.ok_exn in
    let check b (invoice : Invoice.t) =
      [%test_eq: Money.Amount.t]
        (Ledger.Balances.balance b (chart.receivable customer) eur)
        (Invoice.amount_due invoice)
    in
    let b, invoice =
      List.foldi payments ~init:(b, invoice) ~f:(fun i (b, invoice) c ->
        let p =
          { Invoice.Payment.id = Payment_id.of_string (sprintf "p%d" i)
          ; amount = Money.Amount.of_minor_units_int eur c
          ; paid_at = at
          }
        in
        match Invoice.pay invoice p with
        | Error _ -> b, invoice
        | Ok (after, _) ->
          let b =
            (* Collected by bank transfer, so the payer's balance is not ours to check. *)
            Booking.apply b [ Booking.payment chart after p.id ~from:funding |> Or_error.ok_exn ]
            |> Or_error.ok_exn
          in
          check b after;
          b, after)
    in
    if credit > 0
    then (
      match
        Credit_note.create
          ~id:(Credit_note_id.of_string "cn")
          ~number:"CN-1"
          ~issued_on:(Date.of_string "2026-03-02")
          ~reason:Order_change
          invoice
          [ 0, Money.Amount.of_minor_units_int eur credit ]
      with
      | Error _ -> ()
      | Ok note ->
        let after, _ = Invoice.credit invoice ~credit_note:note.id ~amount:note.total |> ok in
        let b =
          Booking.apply b [ some (Booking.credit_note chart invoice note ~refund_to:wallet ~at) ]
          |> Or_error.ok_exn
        in
        check b after))
;;
