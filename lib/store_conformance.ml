open! Core

module Make (M : Monad.S) (Store : Store.S with type 'a io = 'a M.t) = struct
  open M.Let_syntax

  let eur = Money.Currency.eur
  let day = Date.of_string "2026-01-01"
  let at = Time_ns.of_date_ofday ~zone:Time_float.Zone.utc day Time_ns.Ofday.start_of_day

  let plan =
    Plan.flat
      ~id:(Plan_id.of_string "conformance-plan")
      ~name:"Conformance"
      ~interval:Interval.monthly
      (Money.Amount.of_string "10.00 EUR")
    |> Or_error.ok_exn
  ;;

  let subscription id =
    Subscription.create
      ~id:(Subscription_id.of_string id)
      ~customer:(Customer_id.of_string "conformance-customer")
      ~plan
      ~started_on:day
      ()
    |> Or_error.ok_exn
  ;;

  let usage id ~sub ~minutes =
    Usage.Record.create
      ~id:(Usage_id.of_string id)
      ~subscription:(Subscription_id.of_string sub)
      ~meter:(Meter.of_string "conformance-meter")
      ~occurred_at:(Time_ns.add at (Time_ns.Span.of_int_sec (60 * minutes)))
      (Units 1)
    |> Or_error.ok_exn
  ;;

  let invoice id =
    Invoice.draft
      ~id:(Invoice_id.of_string id)
      ~customer:(Customer_id.of_string "conformance-customer")
      ~currency:eur
      ~tax:Tax.none
      ~rounding:Rounding_rules.default
      [ Line.create
          ~code:"plan"
          ~description:"Conformance"
          ~kind:One_off
          ~quantity:1
          (Money.Amount.of_string "10.00 EUR")
        |> Or_error.ok_exn
      ]
    |> Or_error.ok_exn
  ;;

  let fail message sexp = Or_error.error_s [%message message ~_:(sexp : Sexp.t)]
  let expect cond message sexp = if cond then Ok () else fail message sexp

  let ( >>? ) (first : unit Or_error.t M.t) (rest : unit -> unit Or_error.t M.t) =
    match%bind first with
    | Ok () -> rest ()
    | Error _ as e -> return e
  ;;

  let subscriptions store =
    let s = subscription "conformance-sub-1" in
    let%bind first = Store.insert_subscription store s in
    let%bind again = Store.insert_subscription store (subscription "conformance-sub-1") in
    let%bind found = Store.find_subscription store s.id in
    let opened = Subscription.opened s |> Result.ok |> Option.value_exn in
    let%bind saved = Store.save_subscription store opened in
    let%bind stale = Store.save_subscription store opened in
    let cancelled =
      Subscription.cancel opened ~at:(`Immediately day) |> Result.ok |> Option.value_exn
    in
    let%bind (_ : [ `Saved | `Stale of Subscription.t ]) =
      Store.save_subscription store cancelled
    in
    let%bind (_ : [ `Inserted | `Exists of Subscription.t ]) =
      Store.insert_subscription store (subscription "conformance-sub-2")
    in
    let%map live = Store.live_subscriptions store in
    let open Or_error.Let_syntax in
    let%bind () =
      expect
        (match first, again with
         | `Inserted, `Exists e -> Subscription.equal e s
         | _ -> false)
        "insert_subscription is insert-once"
        [%sexp ()]
    in
    let%bind () =
      expect
        ([%equal: Subscription.t option] found (Some s))
        "find_subscription"
        [%sexp (found : Subscription.t option)]
    in
    let%bind () =
      expect
        (match saved, stale with
         | `Saved, `Stale e -> Subscription.equal e opened
         | _ -> false)
        "save_subscription is compare-and-set on the revision"
        [%sexp ()]
    in
    expect
      ([%equal: string list]
         (List.map live ~f:(fun (s : Subscription.t) -> Subscription_id.to_string s.id))
         [ "conformance-sub-2" ])
      "live_subscriptions leaves out cancelled ones"
      [%sexp (live : Subscription.t list)]
  ;;

  let usage_records store =
    let r = usage "conformance-u1" ~sub:"conformance-sub-1" ~minutes:0 in
    let%bind first = Store.insert_usage store r in
    let%bind again =
      Store.insert_usage
        store
        (usage "conformance-u1" ~sub:"conformance-sub-1" ~minutes:5)
    in
    let%bind (_ : [ `Inserted | `Exists of Usage.Record.t ]) =
      Store.insert_usage
        store
        (usage "conformance-u2" ~sub:"conformance-sub-1" ~minutes:60)
    in
    let%bind (_ : [ `Inserted | `Exists of Usage.Record.t ]) =
      Store.insert_usage
        store
        (usage "conformance-u3" ~sub:"conformance-sub-2" ~minutes:1)
    in
    let%map range =
      Store.usage
        store
        (Subscription_id.of_string "conformance-sub-1")
        ~from:at
        ~until:(Time_ns.add at (Time_ns.Span.of_int_sec 3600))
    in
    let open Or_error.Let_syntax in
    let%bind () =
      expect
        (match first, again with
         | `Inserted, `Exists e -> Usage.Record.equal e r
         | _ -> false)
        "insert_usage is insert-once and returns the first body"
        [%sexp ()]
    in
    expect
      ([%equal: string list]
         (List.map range ~f:(fun (r : Usage.Record.t) -> Usage_id.to_string r.id))
         [ "conformance-u1" ])
      "usage is per subscription with a half-open time range"
      [%sexp (range : Usage.Record.t list)]
  ;;

  let invoices store =
    let i = invoice "conformance-inv-1" in
    let%bind first = Store.insert_invoice store i in
    let%bind again = Store.insert_invoice store i in
    let issued =
      Invoice.finalize i ~number:"C-1" ~issued_on:day ~due_on:day
      |> Result.ok
      |> Option.value_exn
    in
    let%bind saved = Store.save_invoice store issued in
    let%bind stale = Store.save_invoice store issued in
    let%bind found = Store.find_invoice store i.id in
    let%bind of_customer =
      Store.invoices_of_customer store (Customer_id.of_string "conformance-customer")
    in
    let note =
      Credit_note.full
        ~id:(Credit_note_id.of_string "conformance-cn-1")
        ~number:"CN-1"
        ~issued_on:day
        ~reason:Duplicate
        issued
      |> Result.ok
      |> Option.value_exn
    in
    let%bind note_first = Store.insert_credit_note store note in
    let%bind note_again = Store.insert_credit_note store note in
    let%map notes = Store.credit_notes_of_invoice store i.id in
    let open Or_error.Let_syntax in
    let%bind () =
      expect
        (match first, again, saved, stale with
         | `Inserted, `Exists _, `Saved, `Stale e -> Invoice.equal e issued
         | _ -> false)
        "invoices: insert-once and compare-and-set"
        [%sexp ()]
    in
    let%bind () =
      expect
        ([%equal: Invoice.t option] found (Some issued) && List.length of_customer = 1)
        "find_invoice and invoices_of_customer"
        [%sexp (found : Invoice.t option)]
    in
    expect
      (match note_first, note_again with
       | `Inserted, `Exists _ -> [%equal: Credit_note.t list] notes [ note ]
       | _ -> false)
      "credit notes: insert-once and listing by invoice"
      [%sexp (notes : Credit_note.t list)]
  ;;

  let dunning store =
    let d, _ =
      Dunning.start
        Dunning.Policy.default
        ~invoice:(Invoice_id.of_string "conformance-inv-1")
        ~on:day
        ~reason:"conformance"
        ()
    in
    let%bind created = Store.save_dunning store d in
    let%bind again = Store.save_dunning store d in
    let ticked, _ = Dunning.tick d ~today:(Date.add_days day 30) in
    let%bind saved = Store.save_dunning store ticked in
    let%bind open_ = Store.open_dunning store in
    let recovered, _ =
      Dunning.apply Dunning.Policy.default ticked (Payment_succeeded { on = day })
      |> Result.ok
      |> Option.value_exn
    in
    let%bind (_ : [ `Saved | `Stale of Dunning.t ]) =
      Store.save_dunning store recovered
    in
    let%bind open_after = Store.open_dunning store in
    let%map found = Store.find_dunning store d.invoice in
    let open Or_error.Let_syntax in
    let%bind () =
      expect
        (match created, again, saved with
         | `Saved, `Stale _, `Saved -> true
         | _ -> false)
        "save_dunning creates at revision 0 and then compares and sets"
        [%sexp ()]
    in
    expect
      (List.length open_ = 1
       && List.is_empty open_after
       && [%equal: Dunning.t option] found (Some recovered))
      "open_dunning leaves out final states"
      [%sexp (open_after : Dunning.t list)]
  ;;

  let numbering store =
    let%bind a = Store.number store ~counter:"conformance-A" ~document:"x" in
    let%bind b = Store.number store ~counter:"conformance-A" ~document:"y" in
    let%bind a' = Store.number store ~counter:"conformance-A" ~document:"x" in
    let%bind c = Store.number store ~counter:"conformance-B" ~document:"x" in
    let%map d = Store.number store ~counter:"conformance-A" ~document:"z" in
    expect
      ([%equal: int list] [ a; b; a'; c; d ] [ 1; 2; 1; 1; 3 ])
      "number is gapless per counter and idempotent per document"
      [%sexp ([ a; b; a'; c; d ] : int list)]
  ;;

  let check store =
    subscriptions store
    >>? fun () ->
    usage_records store
    >>? fun () -> invoices store >>? fun () -> dunning store >>? fun () -> numbering store
  ;;
end
