(* Random days of billing against the in-memory store, checked against the ledger. *)

open! Core
open Billing_core
module Store = Store_memory.Direct
module Billing = Engine.Make (Monad.Ident) (Store)

let eur = Money.Currency.eur
let calls = Meter.of_string "api_calls"
let wallet = Ledger.Account_id.of_string "customer:pool/eur"
let funding = Ledger.Account_id.of_string "external:collections"
let vat = Tax.Rate.create ~name:"vat" ~bps:1900 |> Or_error.ok_exn
let config = { Engine.Config.default with tax = Tax.flat vat }

let plan =
  Plan.create
    ~id:(Plan_id.of_string "metered")
    ~name:"Metered"
    ~currency:eur
    ~interval:Interval.monthly
    [ Plan.Component.create
        ~code:"platform"
        ~description:"Platform"
        (Price.flat (Money.Amount.of_string "20.00 EUR") |> Or_error.ok_exn)
      |> Or_error.ok_exn
    ; Plan.Component.create
        ~code:"api"
        ~description:"API"
        (Price.tiered
           calls
           Volume
           [ Price.Tier.create
               ~up_to:100
               (Unit_price.of_string eur "0.013" |> Or_error.ok_exn)
             |> Or_error.ok_exn
           ; Price.Tier.create (Unit_price.of_string eur "0.007" |> Or_error.ok_exn)
             |> Or_error.ok_exn
           ]
         |> Or_error.ok_exn)
      |> Or_error.ok_exn
    ]
  |> Or_error.ok_exn
;;

type op =
  | Usage of int * int (* subscription index, units *)
  | Pay of int * int (* invoice pick, percent of the amount due *)
  | Fail of int
  | Credit of int
  | Cancel of int
[@@deriving sexp_of]

let op_gen =
  let open Quickcheck.Generator in
  let small = Int.gen_incl 0 2 in
  union
    [ map2 small (Int.gen_incl 0 300) ~f:(fun s u -> Usage (s, u))
    ; map2 (Int.gen_incl 0 20) (Int.gen_incl 1 100) ~f:(fun i p -> Pay (i, p))
    ; map (Int.gen_incl 0 20) ~f:(fun i -> Fail i)
    ; map (Int.gen_incl 0 20) ~f:(fun i -> Credit i)
    ; map small ~f:(fun s -> Cancel s)
    ]
;;

let day_gen = List.gen_with_length 3 op_gen

let%test_unit "the ledger agrees with the invoices after random days" =
  Quickcheck.test
    (List.gen_with_length 70 day_gen)
    ~trials:40
    ~sexp_of:[%sexp_of: op list list]
    ~f:(fun days ->
      let store = Store.create () in
      let start = Date.of_string "2026-01-01" in
      let customers = List.init 3 ~f:(fun i -> Customer_id.of_string (sprintf "c%d" i)) in
      let subs = List.init 3 ~f:(fun i -> Subscription_id.of_string (sprintf "s%d" i)) in
      let accounts =
        Booking.Chart.accounts
          config.chart
          ~customers
          ~codes:[ "platform"; "api" ]
          ~rates:[ vat ]
        @ [ Ledger.Account.create ~allow_negative:true funding Asset
          ; Ledger.Account.create ~allow_negative:true wallet Liability
          ]
      in
      let balances =
        ref
          (List.fold accounts ~init:Ledger.Balances.empty ~f:(fun b account ->
             Ledger.Balances.apply
               b
               { seq = Ledger.Balances.next_seq b; event = Account_opened account }
             |> Or_error.ok_exn))
      in
      let booked = Ledger.Idempotency_key.Table.create () in
      let book (outcome : Engine.Outcome.t) =
        List.iter outcome.entries ~f:(fun entry ->
          match Hashtbl.find booked entry.idempotency_key with
          | Some first ->
            (* A retried operation derives the very same entry. *)
            assert (Ledger.Entry.same_request first entry)
          | None ->
            Hashtbl.set booked ~key:entry.idempotency_key ~data:entry;
            balances := Booking.apply !balances [ entry ] |> Or_error.ok_exn)
      in
      List.iteri subs ~f:(fun i id ->
        let sub =
          Subscription.create
            ~id
            ~customer:(List.nth_exn customers i)
            ~plan
            ~started_on:(Date.add_days start (i * 9))
            ()
          |> Or_error.ok_exn
        in
        let _, outcome =
          Billing.subscribe store config sub ~today:start ~now:Time_ns.epoch
          |> Result.ok
          |> Option.value_exn
        in
        book outcome);
      let check_all where =
        let invoices = List.concat_map customers ~f:(Store.invoices_of_customer store) in
        List.iter customers ~f:(fun customer ->
          let mine =
            List.filter invoices ~f:(fun (i : Invoice.t) ->
              Customer_id.equal i.customer customer && Invoice.Status.equal i.status Open)
          in
          let expected =
            List.map mine ~f:Invoice.amount_due |> Money.Amount.sum eur |> Or_error.ok_exn
          in
          let actual =
            Ledger.Balances.balance !balances (config.chart.receivable customer) eur
          in
          if not (Money.Amount.equal actual expected)
          then
            raise_s
              [%message
                "receivable diverged"
                  (where : Sexp.t)
                  (customer : Customer_id.t)
                  (actual : Money.Amount.t)
                  (expected : Money.Amount.t)
                  ~invoices:
                    (List.filter invoices ~f:(fun (i : Invoice.t) ->
                       Customer_id.equal i.customer customer)
                     : Invoice.t list)])
      in
      let usage_seq = ref 0 in
      List.iteri days ~f:(fun n ops ->
        let today = Date.add_days start n in
        let now =
          Time_ns.of_date_ofday
            ~zone:Time_float.Zone.utc
            today
            (Time_ns.Ofday.of_string "12:00")
        in
        let open_invoices () =
          List.concat_map customers ~f:(Store.invoices_of_customer store)
          |> List.filter ~f:(fun (i : Invoice.t) ->
            match i.status with
            | Open -> true
            | _ -> false)
        in
        let pick k =
          match open_invoices () with
          | [] -> None
          | l -> Some (List.nth_exn l (k % List.length l))
        in
        List.iter ops ~f:(fun op ->
          (match op with
           | Usage (s, units) ->
             incr usage_seq;
             ignore
               (Billing.record_usage
                  store
                  config
                  (Usage.Record.create
                     ~id:(Usage_id.of_string (sprintf "u%d" !usage_seq))
                     ~subscription:(List.nth_exn subs s)
                     ~meter:calls
                     ~occurred_at:now
                     (Units units)
                   |> Or_error.ok_exn)
                : (Usage.Outcome.t, Refusal.t) Result.t)
           | Pay (k, percent) ->
             Option.iter (pick k) ~f:(fun invoice ->
               let due = Invoice.amount_due invoice in
               let amount =
                 Money.Amount.mul_ratio ~rounding:Up due (Q.of_ints percent 100)
               in
               if Money.Amount.is_positive amount
               then (
                 let payment =
                   { Invoice.Payment.id = Payment_id.of_string (sprintf "p%d-%d" n k)
                   ; amount
                   ; paid_at = now
                   }
                 in
                 (* Paying twice is a retry: same entry, nothing new. *)
                 List.iter [ 1; 2 ] ~f:(fun _ ->
                   match Billing.pay store config invoice.id payment ~from:funding with
                   | Ok (_, _, outcome) -> book outcome
                   | Error refusal -> raise_s [%sexp (refusal : Refusal.t)])))
           | Fail k ->
             Option.iter (pick k) ~f:(fun invoice ->
               (* The second report of the same failure is a retry. *)
               List.iter [ 1; 2 ] ~f:(fun _ ->
                 match
                   Billing.payment_failed
                     store
                     config
                     invoice.id
                     ~on:today
                     ~reason:"declined"
                     ~now
                 with
                 | Ok (_, outcome) -> book outcome
                 | Error _ -> ()))
           | Credit k ->
             Option.iter (pick k) ~f:(fun invoice ->
               match
                 Billing.credit
                   store
                   config
                   invoice.id
                   ~id:(Credit_note_id.of_string (sprintf "cn-%d-%d" n k))
                   ~reason:Service_issue
                   `Full
                   ~issued_on:today
                   ~refund_to:wallet
                   ~now
               with
               | Ok (_, _, outcome) -> book outcome
               | Error _ -> ())
           | Cancel s ->
             let cancel () =
               Billing.cancel
                 store
                 config
                 (List.nth_exn subs s)
                 ~at:(`Immediately today)
                 ~refund_to:wallet
                 ~now
             in
             (match cancel () with
              | Ok (_, first) ->
                book first;
                (* A retry returns the same invoices and entries. *)
                let _, again = cancel () |> Result.ok |> Option.value_exn in
                [%test_eq: Invoice_id.t list]
                  (List.map again.invoices ~f:(fun i -> i.id))
                  (List.map first.invoices ~f:(fun i -> i.id));
                book again
              | Error _ -> ()));
          check_all [%sexp (n : int), (op : op)]);
        List.iter (Billing.run store config ~today ~now) ~f:(fun (_, result) ->
          match result with
          | Ok (_, outcome) -> book outcome
          | Error refusal -> raise_s [%sexp (refusal : Refusal.t)]);
        (* Running twice on one day raises nothing new. *)
        List.iter (Billing.run store config ~today ~now) ~f:(fun (_, result) ->
          match result with
          | Ok (_, outcome) -> assert (List.is_empty outcome.invoices)
          | Error refusal -> raise_s [%sexp (refusal : Refusal.t)]);
        ignore (Billing.dunning_due store config ~today : (Invoice_id.t * int) list));
      let invoices = List.concat_map customers ~f:(Store.invoices_of_customer store) in
      List.iter customers ~f:(fun customer ->
        let expected =
          List.filter invoices ~f:(fun (i : Invoice.t) ->
            Customer_id.equal i.customer customer && Invoice.Status.equal i.status Open)
          |> List.map ~f:Invoice.amount_due
          |> Money.Amount.sum eur
          |> Or_error.ok_exn
        in
        [%test_eq: Money.Amount.t]
          (Ledger.Balances.balance !balances (config.chart.receivable customer) eur)
          expected);
      let numbers =
        List.filter_map invoices ~f:(fun i -> i.number)
        |> List.sort ~compare:String.compare
      in
      [%test_eq: string list]
        numbers
        (List.mapi numbers ~f:(fun i _ -> sprintf "INV-2026-%06d" (i + 1))))
;;
