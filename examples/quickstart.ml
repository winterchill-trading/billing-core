(* A month of billing: a plan with a flat fee and metered API calls, a subscription,
   usage, the period close, a payment and its receipt, and the ledger entries. *)

open! Core
open Billing_core
module Store = Store_memory.Direct
module Billing = Engine.Make (Monad.Ident) (Store)

let eur = Money.Currency.eur
let calls = Meter.of_string "api_calls"
let noon day = Time_ns.of_date_ofday ~zone:Time_float.Zone.utc day (Time_ns.Ofday.of_string "12:00")

let plan =
  Plan.create
    ~id:(Plan_id.of_string "growth")
    ~name:"Growth"
    ~currency:eur
    ~interval:Interval.monthly
    [ Plan.Component.create
        ~code:"platform"
        ~description:"Growth plan"
        (Price.flat (Money.Amount.of_string "49.00 EUR") |> Or_error.ok_exn)
      |> Or_error.ok_exn
    ; Plan.Component.create
        ~code:"api"
        ~description:"API calls above 10,000"
        (Price.tiered
           calls
           Graduated
           [ Price.Tier.create ~up_to:10_000 (Unit_price.of_string eur "0" |> Or_error.ok_exn)
             |> Or_error.ok_exn
           ; Price.Tier.create (Unit_price.of_string eur "0.0008" |> Or_error.ok_exn)
             |> Or_error.ok_exn
           ]
         |> Or_error.ok_exn)
      |> Or_error.ok_exn
    ]
  |> Or_error.ok_exn
;;

let print_invoice (invoice : Invoice.t) =
  printf
    "%s (%s), due %s\n"
    (Option.value invoice.number ~default:"draft")
    (Invoice.Status.to_string invoice.status)
    (Money.Amount.to_string (Invoice.amount_due invoice));
  List.iter invoice.items ~f:(fun { line; _ } ->
    printf "  %-24s %8d  %s\n" line.description line.quantity (Money.Amount.to_string line.amount));
  List.iter invoice.taxes ~f:(fun tax ->
    printf "  tax %-20s %8s  %s\n" tax.rate.name "" (Money.Amount.to_string tax.amount))
;;

let print_entries (outcome : Engine.Outcome.t) =
  List.iter outcome.entries ~f:(fun entry ->
    printf "entry %s\n" (Ledger.Idempotency_key.to_string entry.idempotency_key);
    List.iter entry.postings ~f:(fun posting ->
      printf
        "  %-6s %-34s %s\n"
        (Ledger.Side.sexp_of_t posting.side |> Sexp.to_string)
        (Ledger.Account_id.to_string posting.account)
        (Money.Amount.to_string posting.amount)))
;;

let () =
  let vat = Tax.Rate.create ~name:"vat-standard" ~bps:2000 |> Or_error.ok_exn in
  let config = { Engine.Config.default with tax = Tax.flat vat } in
  let store = Store.create () in
  let subscription =
    Subscription.create
      ~id:(Subscription_id.of_string "sub-1")
      ~customer:(Customer_id.of_string "customer-1")
      ~plan
      ~started_on:(Date.of_string "2026-03-01")
      ()
    |> Or_error.ok_exn
  in
  let day = Date.of_string "2026-03-01" in
  let _, opening =
    Billing.subscribe store config subscription ~today:day ~now:(noon day) |> Refusal.ok_exn
  in
  List.iter opening.invoices ~f:print_invoice;
  (* 25,000 calls reported in March, one record sent twice. *)
  List.iter [ "batch-1", 15_000; "batch-2", 10_000; "batch-1", 15_000 ] ~f:(fun (id, n) ->
    let outcome =
      Billing.record_usage
        store
        config
        (Usage.Record.create
           ~id:(Usage_id.of_string id)
           ~subscription:subscription.id
           ~meter:calls
           ~occurred_at:(noon (Date.of_string "2026-03-20"))
           (Units n)
         |> Or_error.ok_exn)
      |> Refusal.ok_exn
    in
    print_s [%sexp (id : string), (outcome : Usage.Outcome.t)]);
  let day = Date.of_string "2026-04-01" in
  let _, closing = Billing.bill store config subscription.id ~today:day ~now:(noon day) |> Refusal.ok_exn in
  List.iter closing.invoices ~f:print_invoice;
  print_entries closing;
  let invoice = List.hd_exn closing.invoices in
  let _, receipt, paid =
    Billing.pay
      store
      config
      invoice.id
      { id = Payment_id.of_string "payment-1"
      ; amount = Invoice.amount_due invoice
      ; paid_at = noon (Date.of_string "2026-04-03")
      }
      ~from:(Ledger.Account_id.of_string "customer:customer-1/eur")
    |> Refusal.ok_exn
  in
  print_entries paid;
  print_endline (Receipt.to_text receipt)
;;
