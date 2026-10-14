open! Core
open Billing_core
module Store = Store_memory.Direct
module Billing = Engine.Make (Monad.Ident) (Store)

let eur = Money.Currency.eur
let amount s = Money.Amount.of_string (s ^ " EUR")
let d = Date.of_string

let noon date =
  Time_ns.of_date_ofday ~zone:Time_float.Zone.utc date (Time_ns.Ofday.of_string "12:00")
;;

let vat = Tax.Rate.create ~name:"vat-standard" ~bps:2000 |> Or_error.ok_exn
let calls = Meter.of_string "api_calls"
let customer = Customer_id.of_string "acme"
let wallet = Ledger.Account_id.of_string "customer:acme/eur"
let ok r = Result.ok r |> Option.value_exn

let plan id price =
  Plan.create
    ~id:(Plan_id.of_string id)
    ~name:id
    ~currency:eur
    ~interval:Interval.monthly
    [ Plan.Component.create
        ~code:"platform"
        ~description:(id ^ " plan")
        (Price.flat (amount price) |> Or_error.ok_exn)
      |> Or_error.ok_exn
    ; Plan.Component.create
        ~code:"api"
        ~description:"API calls"
        (Price.tiered
           calls
           Graduated
           [ Price.Tier.create
               ~up_to:1000
               (Unit_price.of_string eur "0" |> Or_error.ok_exn)
             |> Or_error.ok_exn
           ; Price.Tier.create (Unit_price.of_string eur "0.01" |> Or_error.ok_exn)
             |> Or_error.ok_exn
           ]
         |> Or_error.ok_exn)
      |> Or_error.ok_exn
    ]
  |> Or_error.ok_exn
;;

let starter = plan "starter" "50.00"
let growth = plan "growth" "150.00"
let config = { Engine.Config.default with tax = Tax.flat vat }

let show_invoice (i : Invoice.t) =
  printf
    "%s %s %s total %s due %s\n"
    (Option.value i.number ~default:"(draft)")
    (Invoice.Status.to_string i.status)
    (Option.value_map i.period ~default:"" ~f:Period.to_string)
    (Money.Amount.to_string i.total)
    (Money.Amount.to_string (Invoice.amount_due i));
  List.iter i.items ~f:(fun item ->
    printf
      "  %-38s %4d  %s\n"
      item.line.description
      item.line.quantity
      (Money.Amount.to_string item.line.amount))
;;

let show_outcome (o : Engine.Outcome.t) =
  List.iter o.invoices ~f:show_invoice;
  List.iter o.credit_notes ~f:(fun (n : Credit_note.t) ->
    printf
      "credit note %s total %s against due %s refund %s\n"
      n.number
      (Money.Amount.to_string n.total)
      (Money.Amount.to_string n.against_due)
      (Money.Amount.to_string n.refund));
  List.iter o.entries ~f:(fun (e : Ledger.Entry.t) ->
    printf "  entry %s\n" (Ledger.Idempotency_key.to_string e.idempotency_key));
  List.iter o.actions ~f:(fun a -> print_s [%sexp (a : Dunning.Action.t)])
;;

let usage store id ~on n =
  Billing.record_usage
    store
    config
    (Usage.Record.create
       ~id:(Usage_id.of_string id)
       ~subscription:(Subscription_id.of_string "sub-acme")
       ~meter:calls
       ~occurred_at:(noon on)
       (Units n)
     |> Or_error.ok_exn)
;;

let subscribed () =
  let store = Store.create () in
  let sub =
    Subscription.create
      ~id:(Subscription_id.of_string "sub-acme")
      ~customer
      ~plan:starter
      ~started_on:(d "2026-01-15")
      ()
    |> Or_error.ok_exn
  in
  let sub, outcome =
    Billing.subscribe
      store
      config
      sub
      ~today:(d "2026-01-15")
      ~now:(noon (d "2026-01-15"))
    |> ok
  in
  store, sub, outcome
;;

let%expect_test "a subscription's first months" =
  let store, _, outcome = subscribed () in
  show_outcome outcome;
  print_s
    [%sexp
      (usage store "u1" ~on:(d "2026-01-20") 800 : (Usage.Outcome.t, Refusal.t) Result.t)];
  print_s
    [%sexp
      (usage store "u1" ~on:(d "2026-01-20") 800 : (Usage.Outcome.t, Refusal.t) Result.t)];
  print_s
    [%sexp
      (usage store "u2" ~on:(d "2026-02-10") 700 : (Usage.Outcome.t, Refusal.t) Result.t)];
  print_endline "-- nothing is due before the period ends";
  let _, early =
    Billing.bill
      store
      config
      (Subscription_id.of_string "sub-acme")
      ~today:(d "2026-02-14")
      ~now:(noon (d "2026-02-14"))
    |> ok
  in
  show_outcome early;
  print_endline "-- upgrade from the next period, then close";
  let (_ : Subscription.t * Line.t list) =
    Billing.change_plan
      store
      config
      (Subscription_id.of_string "sub-acme")
      growth
      ~effective:`Next_period
    |> ok
  in
  let results =
    Billing.run store config ~today:(d "2026-02-15") ~now:(noon (d "2026-02-15"))
  in
  List.iter results ~f:(fun (_, r) -> show_outcome (snd (ok r)));
  print_endline "-- late usage is refused";
  print_s
    [%sexp
      (usage store "u3" ~on:(d "2026-02-14") 5 |> Result.map_error ~f:Refusal.code
       : (Usage.Outcome.t, string) Result.t)];
  print_endline "-- running again changes nothing";
  let again =
    Billing.run store config ~today:(d "2026-02-15") ~now:(noon (d "2026-02-15"))
  in
  List.iter again ~f:(fun (_, r) -> show_outcome (snd (ok r)));
  [%expect
    {|
    INV-2026-000001 open 2026-01-15..2026-02-14 total 60.00 EUR due 60.00 EUR
      starter plan                              1  50.00 EUR
      entry billing:invoice:sub-acme@2026-01-15:issue
    (Ok Accepted)
    (Ok Duplicate)
    (Ok Accepted)
    -- nothing is due before the period ends
    -- upgrade from the next period, then close
    INV-2026-000002 open 2026-01-15..2026-02-14 total 186.00 EUR due 186.00 EUR
      API calls                              1500  5.00 EUR
      growth plan                               1  150.00 EUR
      entry billing:invoice:sub-acme@2026-02-15:issue
    -- late usage is refused
    (Error PERIOD_CLOSED)
    -- running again changes nothing
    |}]
;;

let%expect_test "failed payments go through dunning to cancellation and write-off" =
  let store, _, outcome = subscribed () in
  let invoice = (List.hd_exn outcome.invoices).id in
  let fail on =
    let dunning, outcome =
      Billing.payment_failed
        store
        config
        invoice
        ~on:(d on)
        ~reason:"insufficient_funds"
        ~now:(noon (d on))
      |> ok
    in
    print_s [%sexp (dunning.state : Dunning.State.t)];
    show_outcome outcome
  in
  fail "2026-01-16";
  let status () =
    let sub =
      Store.find_subscription store (Subscription_id.of_string "sub-acme")
      |> Option.value_exn
    in
    print_s [%sexp (sub.status : Subscription.Status.t)]
  in
  status ();
  List.iter [ "2026-01-19"; "2026-01-24"; "2026-01-31" ] ~f:(fun day ->
    print_s
      [%sexp
        (Billing.dunning_due store config ~today:(d day) : (Invoice_id.t * int) list)];
    fail day);
  status ();
  [%expect
    {|
    (Waiting (retries_done 0) (next_retry_on 2026-01-19))
    Mark_past_due
    (Notify_customer (failures 1) (next_retry_on (2026-01-19)))
    Past_due
    ((sub-acme@2026-01-15 1))
    (Waiting (retries_done 1) (next_retry_on 2026-01-24))
    (Notify_customer (failures 2) (next_retry_on (2026-01-24)))
    ((sub-acme@2026-01-15 2))
    (Waiting (retries_done 2) (next_retry_on 2026-01-31))
    (Notify_customer (failures 3) (next_retry_on (2026-01-31)))
    ((sub-acme@2026-01-15 3))
    (Exhausted 2026-01-31)
    INV-2026-000001 uncollectible 2026-01-15..2026-02-14 total 60.00 EUR due 60.00 EUR
      starter plan                              1  50.00 EUR
      entry billing:invoice:sub-acme@2026-01-15:write-off
    (Notify_customer (failures 4) (next_retry_on ()))
    Mark_uncollectible
    Cancel_subscription
    Cancelled
    |}]
;;

let%expect_test "a payment ends dunning; a credit note and an immediate cancellation" =
  let store, _, outcome = subscribed () in
  let invoice = (List.hd_exn outcome.invoices).id in
  let (_ : Dunning.t * Engine.Outcome.t) =
    Billing.payment_failed
      store
      config
      invoice
      ~on:(d "2026-01-16")
      ~reason:"x"
      ~now:(noon (d "2026-01-16"))
    |> ok
  in
  let paid, receipt, outcome =
    Billing.pay
      store
      config
      invoice
      { id = Payment_id.of_string "pay-1"
      ; amount = amount "60.00"
      ; paid_at = noon (d "2026-01-18")
      }
      ~from:wallet
    |> ok
  in
  show_outcome outcome;
  print_endline (Receipt.to_text receipt);
  print_s
    [%sexp
      (Store.find_dunning store paid.id |> Option.map ~f:(fun d -> d.Dunning.state)
       : Dunning.State.t option)
      , ((Store.find_subscription store (Subscription_id.of_string "sub-acme")
          |> Option.value_exn)
           .status
         : Subscription.Status.t)];
  print_endline
    "-- usage, then cancel on 2026-01-31: final usage invoice and credit for 15 of 31 \
     days";
  ignore
    (usage store "u1" ~on:(d "2026-01-20") 1500 : (Usage.Outcome.t, Refusal.t) Result.t);
  let sub, outcome =
    Billing.cancel
      store
      config
      (Subscription_id.of_string "sub-acme")
      ~at:(`Immediately (d "2026-01-31"))
      ~refund_to:wallet
      ~now:(noon (d "2026-01-31"))
    |> ok
  in
  show_outcome outcome;
  print_s [%sexp (sub.status : Subscription.Status.t), (sub.ended_on : Date.t option)];
  print_s
    [%sexp
      (usage store "u9" ~on:(d "2026-02-01") 1 |> Result.map_error ~f:Refusal.code
       : (Usage.Outcome.t, string) Result.t)];
  [%expect
    {|
    INV-2026-000001 paid 2026-01-15..2026-02-14 total 60.00 EUR due 0.00 EUR
      starter plan                              1  50.00 EUR
      entry billing:invoice:sub-acme@2026-01-15:payment:pay-1
    Receipt for invoice INV-2026-000001
    Payment pay-1
    Paid at 2026-01-18 12:00:00 UTC
      starter plan: 50.00 EUR
      tax vat-standard: 10.00 EUR
    Invoice total: 60.00 EUR
    This payment: 60.00 EUR
    Paid to date: 60.00 EUR
    Balance: 0.00 EUR
    (((Recovered 2026-01-18)) Active)
    -- usage, then cancel on 2026-01-31: final usage invoice and credit for 15 of 31 days
    INV-2026-000002 open 2026-01-15..2026-02-14 total 6.00 EUR due 6.00 EUR
      API calls                              1500  5.00 EUR
    INV-2026-000001 paid 2026-01-15..2026-02-14 total 60.00 EUR due 0.00 EUR
      starter plan                              1  50.00 EUR
    credit note CN-2026-000001 total 29.03 EUR against due 0.00 EUR refund 29.03 EUR
      entry billing:invoice:sub-acme@2026-01-31:issue
      entry billing:credit-note:sub-acme@2026-01-15/cancel
    (Cancelled (2026-01-31))
    (Error PERIOD_CLOSED)
    |}]
;;

let%expect_test "month-end anchors: the advance line covers the period the schedule uses" =
  let store = Store.create () in
  let id = Subscription_id.of_string "sub-eom" in
  let sub =
    Subscription.create ~id ~customer ~plan:starter ~started_on:(d "2026-01-31") ()
    |> Or_error.ok_exn
  in
  let _ =
    Billing.subscribe
      store
      config
      sub
      ~today:(d "2026-01-31")
      ~now:(noon (d "2026-01-31"))
    |> ok
  in
  let _, outcome =
    Billing.bill store config id ~today:(d "2026-03-31") ~now:(noon (d "2026-03-31"))
    |> ok
  in
  List.iter outcome.invoices ~f:(fun (i : Invoice.t) ->
    List.iter i.items ~f:(fun item ->
      print_endline (Option.value_map item.line.period ~default:"-" ~f:Period.to_string)));
  let sub, outcome =
    Billing.cancel
      store
      config
      id
      ~at:(`Immediately (d "2026-04-15"))
      ~refund_to:wallet
      ~now:(noon (d "2026-04-15"))
    |> ok
  in
  print_s [%sexp (Subscription.current_period sub : Period.t)];
  List.iter outcome.credit_notes ~f:(fun n -> print_s [%sexp (n.total : Money.Amount.t)]);
  [%expect
    {|
    2026-02-28..2026-03-30
    2026-03-31..2026-04-29
    ((start 2026-03-31) (stop 2026-04-30))
    "30.00 EUR"
    |}]
;;
