open! Core

module Config = struct
  type t =
    { tax : Tax.t
    ; rounding : Rounding_rules.t
    ; invoice_numbers : Numbering.t
    ; credit_note_numbers : Numbering.t
    ; payment_terms_days : int
    ; close_after_days : int
    ; proration : Proration.Policy.t
    ; dunning : Dunning.Policy.t
    ; chart : Booking.Chart.t
    ; zone : Time_float.Zone.t
    }

  let default =
    { tax = Tax.none
    ; rounding = Rounding_rules.default
    ; invoice_numbers = Numbering.create "INV" |> Or_error.ok_exn
    ; credit_note_numbers = Numbering.create "CN" |> Or_error.ok_exn
    ; payment_terms_days = 14
    ; close_after_days = 0
    ; proration = Credit_and_charge
    ; dunning = Dunning.Policy.default
    ; chart = Booking.Chart.default
    ; zone = Time_float.Zone.utc
    }
  ;;
end

module Outcome = struct
  type t =
    { invoices : Invoice.t list
    ; credit_notes : Credit_note.t list
    ; entries : Ledger.Entry.t list
    ; actions : Dunning.Action.t list
    }
  [@@deriving sexp_of]

  let empty = { invoices = []; credit_notes = []; entries = []; actions = [] }

  let append a b =
    { invoices = a.invoices @ b.invoices
    ; credit_notes = a.credit_notes @ b.credit_notes
    ; entries = a.entries @ b.entries
    ; actions = a.actions @ b.actions
    }
  ;;
end

(* Readable when it fits the id length, a digest otherwise. Either way it depends only on
   the subscription and the boundary, so every runner derives the same id. *)
let derived_invoice_id subscription boundary =
  let readable =
    sprintf "%s@%s" (Subscription_id.to_string subscription) (Date.to_string boundary)
  in
  match Invoice_id.of_string_or_error readable with
  | Ok id -> id
  | Error _ -> Invoice_id.of_string ("inv-" ^ Md5.to_hex (Md5.digest_string readable))
;;

module Make (M : Monad.S) (Store : Store.S with type 'a io = 'a M.t) = struct
  open M.Let_syntax

  type 'a result = ('a, Refusal.t) Result.t M.t

  let ok x : _ result = return (Ok x)
  let fail e : _ result = return (Error e)

  let ( >>=? ) (x : 'a result) (f : 'a -> 'b result) : 'b result =
    match%bind x with
    | Ok v -> f v
    | Error e -> return (Error e)
  ;;

  let lift (r : ('a, Refusal.t) Result.t) : 'a result = return r
  let of_or_error r = lift (Refusal.of_or_error r)
  let retries = 5

  let start_of (config : Config.t) date =
    Time_ns.of_date_ofday ~zone:config.zone date Time_ns.Ofday.start_of_day
  ;;

  (* Load, apply a pure step, save with compare-and-set; on a stale save, redo the step
     on what is stored. *)
  let update_subscription store id ~(f : Subscription.t -> (Subscription.t * 'a) result) =
    let rec attempt n (current : Subscription.t) =
      f current
      >>=? fun (next, extra) ->
      if Subscription.equal next current
      then ok (next, extra)
      else (
        match%bind Store.save_subscription store next with
        | `Saved -> ok (next, extra)
        | `Stale stored ->
          if n >= retries
          then fail (Stale { what = "subscription"; id = Subscription_id.to_string id })
          else attempt (n + 1) stored)
    in
    match%bind Store.find_subscription store id with
    | None -> fail (Not_found { what = "subscription"; id = Subscription_id.to_string id })
    | Some current -> attempt 0 current
  ;;

  let update_invoice store id ~(f : Invoice.t -> (Invoice.t * 'a) result) =
    let rec attempt n (current : Invoice.t) =
      f current
      >>=? fun (next, extra) ->
      if Invoice.equal next current
      then ok (next, extra)
      else (
        match%bind Store.save_invoice store next with
        | `Saved -> ok (next, extra)
        | `Stale stored ->
          if n >= retries
          then fail (Stale { what = "invoice"; id = Invoice_id.to_string id })
          else attempt (n + 1) stored)
    in
    match%bind Store.find_invoice store id with
    | None -> fail (Not_found { what = "invoice"; id = Invoice_id.to_string id })
    | Some current -> attempt 0 current
  ;;

  let issue_entries (config : Config.t) invoice ~now =
    of_or_error (Booking.issue config.chart invoice ~at:now) >>=? fun e -> ok (Option.to_list e)
  ;;

  (* Finalises a stored draft with its number; an invoice already issued is left alone.
     Either way the issue entry is returned, so a retry after a crash still books it. *)
  let issue store (config : Config.t) (invoice : Invoice.t) ~issued_on ~now =
    (match invoice.status with
     | Draft ->
       update_invoice store invoice.id ~f:(fun current ->
         match current.status with
         | Draft ->
           let counter = Numbering.counter config.invoice_numbers ~issued_on in
           let%bind seq =
             Store.number store ~counter ~document:(Invoice_id.to_string current.id)
           in
           lift
             (Invoice.finalize
                current
                ~number:(Numbering.format config.invoice_numbers ~issued_on ~seq)
                ~issued_on
                ~due_on:(Date.add_days issued_on config.payment_terms_days))
           >>=? fun issued -> ok (issued, ())
         | Open | Paid | Void | Uncollectible -> ok (current, ()))
       >>=? fun (issued, ()) -> ok issued
     | Open | Paid | Void | Uncollectible -> ok invoice)
    >>=? fun issued ->
    match issued.status with
    | Void -> ok (issued, [])
    | Draft | Open | Paid | Uncollectible ->
      issue_entries config issued ~now >>=? fun entries -> ok (issued, entries)
  ;;

  (* Finds or creates the draft for one boundary of a subscription, then issues it. *)
  let raise_invoice
    store
    (config : Config.t)
    (sub : Subscription.t)
    ~boundary
    ~period
    ~now
    (lines : Line.t list)
    =
    let id = derived_invoice_id sub.id boundary in
    (match%bind Store.find_invoice store id with
     | Some existing -> ok existing
     | None ->
       of_or_error
         (Invoice.draft
            ~id
            ~customer:sub.customer
            ~subscription:sub.id
            ~period
            ~currency:sub.plan.currency
            ~tax:config.tax
            ~rounding:config.rounding
            lines)
       >>=? fun draft ->
       (match%bind Store.insert_invoice store draft with
        | `Inserted -> ok draft
        | `Exists existing -> ok existing))
    >>=? fun invoice ->
    issue store config invoice ~issued_on:boundary ~now
    >>=? fun (invoice, entries) ->
    ok { Outcome.empty with invoices = [ invoice ]; entries }
  ;;

  let save_step store (next : Subscription.t) =
    match%bind Store.save_subscription store next with
    | `Saved -> return next
    | `Stale stored -> return stored
  ;;

  let rec bill_from store (config : Config.t) (sub : Subscription.t) ~today ~now acc ~fuel =
    let due : Subscription.Due.t =
      match Subscription.due sub ~today with
      | Close _ as close ->
        let stop = (Subscription.current_period sub).stop in
        if Date.( >= ) today (Date.add_days stop config.close_after_days)
        then close
        else Nothing
      | (Nothing | Open) as due -> due
    in
    if fuel = 0
    then ok (sub, acc)
    else (
      match due with
      | Nothing -> ok (sub, acc)
      | Open ->
        let period = Subscription.current_period sub in
        let lines =
          Charges.in_advance sub.plan ~seats:sub.seats ~period @ sub.pending_lines
        in
        (if List.is_empty lines
         then ok Outcome.empty
         else raise_invoice store config sub ~boundary:period.start ~period ~now lines)
        >>=? fun outcome ->
        lift (Subscription.opened sub)
        >>=? fun next ->
        let%bind next = save_step store next in
        bill_from store config next ~today ~now (Outcome.append acc outcome) ~fuel:(fuel - 1)
      | Close _ ->
        let period = Subscription.current_period sub in
        let%bind records =
          Store.usage
            store
            sub.id
            ~from:(start_of config period.start)
            ~until:(start_of config period.stop)
        in
        of_or_error
          (Charges.in_arrears
             sub.plan
             records
             ~zone:config.zone
             ~period
             ~rounding:config.rounding)
        >>=? fun usage_lines ->
        let advance =
          if Subscription.ends_with_current_period sub
          then []
          else (
            let plan = Option.value sub.pending_plan ~default:sub.plan in
            let next_period =
              Interval.period plan.interval ~anchor:period.stop ~index:0
            in
            Charges.in_advance plan ~seats:sub.seats ~period:next_period)
        in
        let lines = usage_lines @ sub.pending_lines @ advance in
        (if List.is_empty lines
         then ok Outcome.empty
         else raise_invoice store config sub ~boundary:period.stop ~period ~now lines)
        >>=? fun outcome ->
        lift (Subscription.closed sub)
        >>=? fun next ->
        let%bind next = save_step store next in
        bill_from store config next ~today ~now (Outcome.append acc outcome) ~fuel:(fuel - 1))
  ;;

  (* Bounds the catch-up of one call; a later call continues where this one stopped. *)
  let max_steps = 1_000

  let bill store config id ~today ~now =
    match%bind Store.find_subscription store id with
    | None -> fail (Not_found { what = "subscription"; id = Subscription_id.to_string id })
    | Some sub -> bill_from store config sub ~today ~now Outcome.empty ~fuel:max_steps
  ;;

  let subscribe store config (sub : Subscription.t) ~today ~now =
    let%bind (_ : [ `Inserted | `Exists of Subscription.t ]) =
      Store.insert_subscription store sub
    in
    bill store config sub.id ~today ~now
  ;;

  let run store config ~today ~now =
    let%bind subs = Store.live_subscriptions store in
    List.fold subs ~init:(return []) ~f:(fun acc (sub : Subscription.t) ->
      let%bind acc = acc in
      let%map result = bill store config sub.id ~today ~now in
      (sub.id, result) :: acc)
    >>| List.rev
  ;;

  let record_usage store (config : Config.t) (record : Usage.Record.t) =
    match%bind Store.find_subscription store record.subscription with
    | None ->
      fail
        (Not_found
           { what = "subscription"; id = Subscription_id.to_string record.subscription })
    | Some sub ->
      let limit =
        match sub.status with
        | Cancelled -> sub.ended_on
        | Trialing | Active | Past_due -> Subscription.billed_through sub
      in
      let date = Usage.Record.date record ~zone:config.zone in
      let closed =
        match sub.status, limit with
        | Cancelled, Some ended_on -> Date.( >= ) date ended_on
        | (Trialing | Active | Past_due), Some billed_through -> Date.( < ) date billed_through
        | _, None -> false
      in
      if closed
      then
        fail
          (Period_closed
             { usage = record.id
             ; billed_through = Option.value limit ~default:date
             })
      else (
        match%bind Store.insert_usage store record with
        | `Inserted -> ok Usage.Outcome.Accepted
        | `Exists existing -> lift (Usage.check ~existing:(Some existing) record))
  ;;

  let change_plan store (config : Config.t) id plan ~effective =
    update_subscription store id ~f:(fun sub ->
      lift
        (Subscription.change_plan
           sub
           plan
           ~effective
           ~policy:config.proration
           ~rounding:config.rounding))
  ;;

  let set_seats store (config : Config.t) id seats ~on =
    update_subscription store id ~f:(fun sub ->
      lift
        (Subscription.set_seats sub seats ~on ~policy:config.proration ~rounding:config.rounding))
  ;;

  let credit_number store (config : Config.t) (id : Credit_note_id.t) ~issued_on =
    let counter = Numbering.counter config.credit_note_numbers ~issued_on in
    let%map seq = Store.number store ~counter ~document:(Credit_note_id.to_string id) in
    Numbering.format config.credit_note_numbers ~issued_on ~seq
  ;;

  let end_dunning store (config : Config.t) (invoice : Invoice.t) event =
    match%bind Store.find_dunning store invoice.id with
    | None -> ok ()
    | Some dunning when Dunning.State.is_final dunning.state -> ok ()
    | Some dunning ->
      lift (Dunning.apply config.dunning dunning event)
      >>=? fun (next, _) ->
      let%bind (_ : [ `Saved | `Stale of Dunning.t ]) = Store.save_dunning store next in
      (match invoice.subscription with
       | None -> ok ()
       | Some sub ->
         update_subscription store sub ~f:(fun s ->
           lift (Subscription.mark_recovered s) >>=? fun s -> ok (s, ()))
         >>=? fun _ -> ok ())
  ;;

  (* Issues and applies a credit note, idempotently: an id already stored is reused. *)
  let issue_credit_note
    store
    (config : Config.t)
    invoice_id
    ~id
    ~reason
    which
    ~issued_on
    ~refund_to
    ~now
    =
    (match%bind Store.find_invoice store invoice_id with
     | None ->
       fail (Not_found { what = "invoice"; id = Invoice_id.to_string invoice_id })
     | Some invoice -> ok invoice)
    >>=? fun (invoice : Invoice.t) ->
    let%bind existing =
      Store.credit_notes_of_invoice store invoice_id
      >>| List.find ~f:(fun (n : Credit_note.t) -> Credit_note_id.equal n.id id)
    in
    (match existing with
     | Some note -> ok note
     | None ->
       let%bind number = credit_number store config id ~issued_on in
       lift
         (match which with
          | `Full -> Credit_note.full ~id ~number ~issued_on ~reason invoice
          | `Lines lines -> Credit_note.create ~id ~number ~issued_on ~reason invoice lines)
       >>=? fun note ->
       (match%bind Store.insert_credit_note store note with
        | `Inserted -> ok note
        | `Exists stored -> ok stored))
    >>=? fun (note : Credit_note.t) ->
    (* The entry is derived from the invoice as it was when the note was computed: the
       note's split between amount due and refund already records that state. *)
    of_or_error (Booking.credit_note config.chart invoice note ~refund_to ~at:now)
    >>=? fun entry ->
    update_invoice store invoice_id ~f:(fun current ->
      lift (Invoice.credit current ~credit_note:note.id ~amount:note.total)
      >>=? fun (next, (_ : [ `Applied | `Duplicate ])) -> ok (next, ()))
    >>=? fun (invoice, ()) ->
    (match invoice.status with
     | Paid -> end_dunning store config invoice Invoice_settled
     | Draft | Open | Void | Uncollectible -> ok ())
    >>=? fun () ->
    ok
      ( note
      , invoice
      , { Outcome.empty with
          invoices = [ invoice ]
        ; credit_notes = [ note ]
        ; entries = Option.to_list entry
        } )
  ;;

  let credit store config invoice_id ~id ~reason which ~issued_on ~refund_to ~now =
    issue_credit_note store config invoice_id ~id ~reason which ~issued_on ~refund_to ~now
  ;;

  let cancel_note_id invoice =
    let readable = sprintf "%s/cancel" (Invoice_id.to_string invoice) in
    match Credit_note_id.of_string_or_error readable with
    | Ok id -> id
    | Error _ -> Credit_note_id.of_string ("cn-" ^ Md5.to_hex (Md5.digest_string readable))
  ;;

  let cancel store (config : Config.t) id ~at ~refund_to ~now =
    (match%bind Store.find_subscription store id with
     | None -> fail (Not_found { what = "subscription"; id = Subscription_id.to_string id })
     | Some before -> ok before)
    >>=? fun (before : Subscription.t) ->
    update_subscription store id ~f:(fun sub ->
      match at, sub.status, sub.ended_on with
      | `Immediately on, Cancelled, Some ended_on when Date.equal on ended_on ->
        (* A retry: the cancellation is stored, the invoice and credit steps below are
           idempotent and return the same results again. *)
        ok (sub, ())
      | `At_period_end, (Trialing | Active | Past_due), _ when sub.cancel_at_period_end ->
        ok (sub, ())
      | _ -> lift (Subscription.cancel sub ~at) >>=? fun next -> ok (next, ()))
    >>=? fun (sub, ()) ->
    match at with
    | `At_period_end -> ok (sub, Outcome.empty)
    | `Immediately _ when not before.opened -> ok (sub, Outcome.empty)
    | `Immediately on ->
      let period = Subscription.current_period before in
      (* Usage and pending lines of the cut-short period, billed now. *)
      let%bind records =
        Store.usage store id ~from:(start_of config period.start) ~until:(start_of config on)
      in
      (if Date.( > ) on period.start
       then (
         let served = Period.create_exn ~start:period.start ~stop:on in
         of_or_error
           (Charges.in_arrears
              before.plan
              records
              ~zone:config.zone
              ~period:served
              ~rounding:config.rounding))
       else ok [])
      >>=? fun usage_lines ->
      let lines = usage_lines @ before.pending_lines in
      (if List.is_empty lines
       then ok Outcome.empty
       else raise_invoice store config before ~boundary:on ~period ~now lines)
      >>=? fun final ->
      (* The unused share of the advance billed for this period, as a credit note on the
         invoice that billed it. *)
      let share = Proration.remaining period ~on in
      let advance_invoice = derived_invoice_id id period.start in
      (match config.proration with
       | Charge_only | No_proration -> ok Outcome.empty
       | Credit_and_charge ->
         (match%bind Store.find_invoice store advance_invoice with
          | None -> ok Outcome.empty
          | Some invoice ->
            let lines =
              List.filter_mapi invoice.items ~f:(fun i item ->
                match item.line.kind, item.line.period with
                | Recurring, Some p when Period.equal p period ->
                  let amount =
                    Money.Amount.mul_ratio ~rounding:config.rounding.line item.line.amount share
                  in
                  Option.some_if (Money.Amount.is_positive amount) (i, amount)
                | _ -> None)
            in
            let creditable =
              Money.Amount.sub_exn invoice.total (Invoice.amount_credited invoice)
            in
            (* An invoice already credited (in full or in part) by hand is left alone:
               the unused share was settled that way. *)
            (match invoice.status, lines with
             | (Open | Paid), _ :: _
               when Money.Amount.equal creditable invoice.total
                    || List.exists invoice.credits ~f:(fun c ->
                      Credit_note_id.equal c.credit_note (cancel_note_id invoice.id)) ->
               issue_credit_note
                 store
                 config
                 invoice.id
                 ~id:(cancel_note_id invoice.id)
                 ~reason:Order_change
                 (`Lines lines)
                 ~issued_on:on
                 ~refund_to
                 ~now
               >>=? fun (_, _, outcome) -> ok outcome
             | _ -> ok Outcome.empty)))
      >>=? fun credited -> ok (sub, Outcome.append final credited)
  ;;

  let pay store (config : Config.t) invoice_id (payment : Invoice.Payment.t) ~from =
    update_invoice store invoice_id ~f:(fun invoice ->
      lift (Invoice.pay invoice payment) >>=? fun (next, _) -> ok (next, ()))
    >>=? fun (invoice, ()) ->
    of_or_error (Booking.payment config.chart invoice payment.id ~from)
    >>=? fun entry ->
    lift (Receipt.of_payment invoice payment.id)
    >>=? fun receipt ->
    (match invoice.status with
     | Paid ->
       end_dunning
         store
         config
         invoice
         (Payment_succeeded
            { on = Time_ns.to_date payment.paid_at ~zone:config.zone })
     | Draft | Open | Void | Uncollectible -> ok ())
    >>=? fun () ->
    ok (invoice, receipt, { Outcome.empty with invoices = [ invoice ]; entries = [ entry ] })
  ;;

  let perform store (config : Config.t) (invoice : Invoice.t) actions ~on ~now =
    List.fold actions ~init:(ok Outcome.empty) ~f:(fun acc (action : Dunning.Action.t) ->
      acc
      >>=? fun acc ->
      match action, invoice.subscription with
      | Mark_past_due, Some sub ->
        update_subscription store sub ~f:(fun s ->
          match s.status with
          | Active -> lift (Subscription.mark_past_due s) >>=? fun s -> ok (s, ())
          | Trialing | Past_due | Cancelled -> ok (s, ()))
        >>=? fun _ -> ok acc
      | Cancel_subscription, Some sub ->
        update_subscription store sub ~f:(fun s ->
          match s.status with
          | Cancelled -> ok (s, ())
          | Trialing | Active | Past_due ->
            (* Service ends on the day retries run out; the overdue invoice stays as it
               is (written off or still open, as the policy says). A date outside the
               current period, possible when dunning lags behind billing, ends the
               subscription with its current period instead. *)
            (match Subscription.cancel s ~at:(`Immediately on) with
             | Ok s -> ok (s, ())
             | Error _ -> lift (Subscription.cancel s ~at:`At_period_end) >>=? fun s -> ok (s, ())))
        >>=? fun _ -> ok acc
      | Mark_uncollectible, _ ->
        update_invoice store invoice.id ~f:(fun current ->
          match current.status with
          | Open -> lift (Invoice.mark_uncollectible current) >>=? fun i -> ok (i, ())
          | Draft | Paid | Void | Uncollectible -> ok (current, ()))
        >>=? fun (current, ()) ->
        (match current.status with
         | Uncollectible ->
           of_or_error (Booking.write_off config.chart current ~at:now)
           >>=? fun entry ->
           ok
             (Outcome.append
                acc
                { Outcome.empty with
                  invoices = [ current ]
                ; entries = Option.to_list entry
                })
         | Draft | Open | Paid | Void -> ok acc)
      | (Mark_past_due | Cancel_subscription), None
      | (Notify_customer _ | Retry_payment _), _ -> ok acc)
    >>=? fun outcome -> ok { outcome with actions }
  ;;

  let payment_failed store (config : Config.t) invoice_id ~on ~reason ~now =
    (match%bind Store.find_invoice store invoice_id with
     | None -> fail (Not_found { what = "invoice"; id = Invoice_id.to_string invoice_id })
     | Some invoice -> ok invoice)
    >>=? fun (invoice : Invoice.t) ->
    let must_be_open () =
      match invoice.status with
      | Open -> ok ()
      | Draft | Paid | Void | Uncollectible ->
        Refusal.invalid_state
          ~what:"invoice"
          ~id:(Invoice_id.to_string invoice.id)
          ~state:(Invoice.Status.to_string invoice.status)
          ~action:"payment_failed"
        |> lift
    in
    let rec attempt n =
      let%bind existing = Store.find_dunning store invoice_id in
      (match existing with
       | Some dunning
         when [%equal: (Date.t * string) option]
                (List.last dunning.failures)
                (Some (on, reason)) ->
         (* The same failure reported again: a retry. Redo the actions the current state
            calls for; each of them is idempotent. *)
         ok (dunning, `Retry (Dunning.standing_actions config.dunning dunning))
       | None ->
         must_be_open ()
         >>=? fun () ->
         let dunning, actions =
           Dunning.start
             config.dunning
             ~invoice:invoice_id
             ?subscription:invoice.subscription
             ~on
             ~reason
             ()
         in
         ok (dunning, `New actions)
       | Some dunning ->
         must_be_open ()
         >>=? fun () ->
         lift (Dunning.apply config.dunning dunning (Payment_failed { on; reason }))
         >>=? fun (next, actions) -> ok (next, `New actions))
      >>=? fun (next, actions) ->
      match actions with
      | `Retry actions -> ok (next, actions)
      | `New actions ->
      match%bind Store.save_dunning store next with
      | `Saved -> ok (next, actions)
      | `Stale _ ->
        if n >= retries
        then fail (Stale { what = "dunning"; id = Invoice_id.to_string invoice_id })
        else attempt (n + 1)
    in
    attempt 0
    >>=? fun (dunning, actions) ->
    perform store config invoice actions ~on ~now
    >>=? fun outcome -> ok (dunning, outcome)
  ;;

  let dunning_due store (_ : Config.t) ~today =
    let%bind open_ = Store.open_dunning store in
    List.fold open_ ~init:(return []) ~f:(fun acc dunning ->
      let%bind acc = acc in
      let next, actions = Dunning.tick dunning ~today in
      match actions with
      | [] -> return acc
      | _ :: _ ->
        (match%map Store.save_dunning store next with
         | `Saved ->
           List.filter_map actions ~f:(function
             | Retry_payment { retry } -> Some (dunning.invoice, retry)
             | _ -> None)
           @ acc
         | `Stale _ ->
           (* Another runner ticked it first and owns this retry. *)
           acc))
    >>| List.rev
  ;;

  let void store (config : Config.t) invoice_id ~now =
    update_invoice store invoice_id ~f:(fun invoice ->
      lift (Invoice.void invoice) >>=? fun next -> ok (next, ()))
    >>=? fun (invoice, ()) ->
    of_or_error (Booking.void config.chart invoice ~at:now)
    >>=? fun entry ->
    ok (invoice, { Outcome.empty with invoices = [ invoice ]; entries = Option.to_list entry })
  ;;
end
