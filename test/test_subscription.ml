open! Core
open Billing_core

let amount s = Money.Amount.of_string (s ^ " EUR")
let d = Date.of_string

let plan ?(trial_days = 0) id price seat =
  Plan.create
    ~id:(Plan_id.of_string id)
    ~name:id
    ~currency:Money.Currency.eur
    ~interval:Interval.monthly
    ~trial_days
    [ Plan.Component.create
        ~code:"platform"
        ~description:"Platform"
        (Price.flat (amount price) |> Or_error.ok_exn)
      |> Or_error.ok_exn
    ; Plan.Component.create
        ~code:"seats"
        ~description:"Seats"
        (Price.per_seat ~included:1 (amount seat) |> Or_error.ok_exn)
      |> Or_error.ok_exn
    ]
  |> Or_error.ok_exn
;;

let starter = plan "starter" "30.00" "10.00"
let growth = plan "growth" "90.00" "15.00"
let ok r = Result.ok r |> Option.value_exn

let show (t : Subscription.t) ~today =
  print_s
    [%sexp
      { status : Subscription.Status.t = t.status
      ; plan : Plan_id.t = t.plan.id
      ; current : Period.t = Subscription.current_period t
      ; due : Subscription.Due.t = Subscription.due t ~today
      ; billed_through : Date.t option = Subscription.billed_through t
      }]
;;

let show_lines lines =
  List.iter lines ~f:(fun (l : Line.t) ->
    printf "%-45s %s\n" l.description (Money.Amount.to_string l.amount))
;;

let%expect_test "trial, opening, closing with a pending plan, cancel at period end" =
  let t =
    Subscription.create
      ~id:(Subscription_id.of_string "sub-1")
      ~customer:(Customer_id.of_string "c-1")
      ~plan:(plan ~trial_days:14 "starter" "30.00" "10.00")
      ~seats:3
      ~started_on:(d "2026-01-17")
      ()
    |> Or_error.ok_exn
  in
  show t ~today:(d "2026-01-20");
  show t ~today:(d "2026-01-31");
  let t = Subscription.opened t |> ok in
  let t, lines =
    Subscription.change_plan
      t
      growth
      ~effective:`Next_period
      ~policy:Credit_and_charge
      ~rounding:Rounding_rules.default
    |> ok
  in
  show_lines lines;
  show t ~today:(d "2026-02-28");
  let t = Subscription.closed t |> ok in
  show t ~today:(d "2026-03-01");
  let t = Subscription.cancel t ~at:`At_period_end |> ok in
  print_s [%sexp (Subscription.ends_with_current_period t : bool)];
  let t = Subscription.closed t |> ok in
  show t ~today:(d "2026-04-01");
  print_s [%sexp (t.ended_on : Date.t option)];
  [%expect
    {|
    ((status Trialing) (plan starter)
     (current ((start 2026-01-31) (stop 2026-02-28))) (due Nothing)
     (billed_through ()))
    ((status Trialing) (plan starter)
     (current ((start 2026-01-31) (stop 2026-02-28))) (due Open)
     (billed_through ()))
    ((status Active) (plan starter)
     (current ((start 2026-01-31) (stop 2026-02-28))) (due (Close 0))
     (billed_through (2026-01-31)))
    ((status Active) (plan growth)
     (current ((start 2026-02-28) (stop 2026-03-31))) (due Nothing)
     (billed_through (2026-02-28)))
    true
    ((status Cancelled) (plan growth)
     (current ((start 2026-02-28) (stop 2026-03-31))) (due Nothing)
     (billed_through (2026-02-28)))
    (2026-03-31)
    |}]
;;

let opened ?(seats = 3) plan =
  Subscription.create
    ~id:(Subscription_id.of_string "sub-2")
    ~customer:(Customer_id.of_string "c-2")
    ~plan
    ~seats
    ~started_on:(d "2026-04-01")
    ()
  |> Or_error.ok_exn
  |> Subscription.opened
  |> ok
;;

let%expect_test "mid-period changes prorate by day" =
  (* April has 30 days; from the 16th, 15 days remain: half the period. *)
  let t = opened starter in
  let t, lines =
    Subscription.set_seats
      t
      5
      ~on:(d "2026-04-16")
      ~policy:Credit_and_charge
      ~rounding:Rounding_rules.default
    |> ok
  in
  show_lines lines;
  print_endline "--";
  let _, lines =
    Subscription.change_plan
      t
      growth
      ~effective:(`Immediately (d "2026-04-21"))
      ~policy:Credit_and_charge
      ~rounding:Rounding_rules.default
    |> ok
  in
  show_lines lines;
  print_endline "--";
  let _, lines =
    Subscription.set_seats
      (opened ~seats:5 starter)
      2
      ~on:(d "2026-04-16")
      ~policy:Charge_only
      ~rounding:Rounding_rules.default
    |> ok
  in
  show_lines lines;
  print_s
    [%sexp
      (Subscription.set_seats
         t
         5
         ~on:(d "2026-05-01")
         ~policy:Credit_and_charge
         ~rounding:Rounding_rules.default
       |> Result.map ~f:snd
       : (Line.t list, Refusal.t) Result.t)];
  [%expect
    {|
    Seats, change for remaining time              10.00 EUR
    --
    Unused time on Platform                       -10.00 EUR
    Unused time on Seats                          -13.33 EUR
    Remaining time on Platform                    30.00 EUR
    Remaining time on Seats                       20.00 EUR
    --
    (Error
     (Invalid
      ("Billing_core.Subscription: date outside the current period"
       (on 2026-05-01) (period ((start 2026-04-01) (stop 2026-05-01))))))
    |}]
;;

let%test_unit "proration of a seat change never exceeds the full-period difference" =
  Quickcheck.test
    (Quickcheck.Generator.tuple3 (Int.gen_incl 0 30) (Int.gen_incl 0 30) (Int.gen_incl 0 29))
    ~sexp_of:[%sexp_of: int * int * int]
    ~f:(fun (before, after, day) ->
      let t = opened ~seats:before starter in
      let on = Date.add_days (d "2026-04-01") day in
      let _, lines =
        Subscription.set_seats
          t
          after
          ~on
          ~policy:Credit_and_charge
          ~rounding:Rounding_rules.default
        |> ok
      in
      let prorated =
        Money.Amount.sum Money.Currency.eur (List.map lines ~f:(fun l -> l.amount))
        |> Or_error.ok_exn
      in
      let full =
        Money.Amount.sub_exn
          (Plan.recurring_total starter ~seats:after)
          (Plan.recurring_total starter ~seats:before)
      in
      assert (Money.Amount.( <= ) (Money.Amount.abs prorated) (Money.Amount.abs full));
      assert (Money.Amount.is_zero prorated || Money.Amount.sign prorated = Money.Amount.sign full))
;;

let%expect_test "stable digest" =
  print_endline (Bin_prot.Shape.eval_to_digest_string Subscription.Stable.V1.bin_shape_t);
  [%expect {| 0150f6bcd934f1bc087ea21abeb653cd |}]
;;
