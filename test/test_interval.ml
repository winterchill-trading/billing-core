open! Core
open Billing_core

let%expect_test "monthly periods from the 31st clamp per month and come back" =
  let anchor = Date.of_string "2026-01-31" in
  List.iter (List.range 0 5) ~f:(fun index ->
    print_endline (Period.to_string (Interval.period Interval.monthly ~anchor ~index)));
  [%expect
    {|
    2026-01-31..2026-02-27
    2026-02-28..2026-03-30
    2026-03-31..2026-04-29
    2026-04-30..2026-05-30
    2026-05-31..2026-06-29
    |}]
;;

let%expect_test "leap day anchors and day intervals" =
  let anchor = Date.of_string "2028-02-29" in
  print_endline (Period.to_string (Interval.period Interval.yearly ~anchor ~index:1));
  let thirty = Interval.days 30 |> Or_error.ok_exn in
  print_endline (Period.to_string (Interval.period thirty ~anchor ~index:2));
  print_s [%sexp (Interval.months 0 : Interval.t Or_error.t)];
  [%expect
    {|
    2029-02-28..2030-02-27
    2028-04-29..2028-05-28
    (Error ("Billing_core.Interval: months must be 1..120" (n 0)))
    |}]
;;

let interval_gen =
  Quickcheck.Generator.union
    [ Quickcheck.Generator.map (Int.gen_incl 1 24) ~f:(fun n ->
        Interval.months n |> Or_error.ok_exn)
    ; Quickcheck.Generator.map (Int.gen_incl 1 400) ~f:(fun n ->
        Interval.days n |> Or_error.ok_exn)
    ]
;;

let date_gen =
  Quickcheck.Generator.map (Int.gen_incl 0 20_000) ~f:(fun d ->
    Date.add_days (Date.of_string "2000-01-01") d)
;;

let%test_unit "periods tile the calendar and index_of finds them" =
  Quickcheck.test
    (Quickcheck.Generator.tuple4
       interval_gen
       date_gen
       (Int.gen_incl 0 60)
       (Int.gen_incl 0 400))
    ~sexp_of:[%sexp_of: Interval.t * Date.t * int * int]
    ~f:(fun (interval, anchor, index, offset) ->
      let p = Interval.period interval ~anchor ~index in
      let next = Interval.period interval ~anchor ~index:(index + 1) in
      [%test_eq: Date.t] p.stop next.start;
      let date = Date.add_days p.start (offset % Period.days p) in
      [%test_eq: int option] (Interval.index_of interval ~anchor date) (Some index))
;;

let%expect_test "stable digest" =
  print_endline (Bin_prot.Shape.eval_to_digest_string Interval.Stable.V1.bin_shape_t);
  print_endline (Bin_prot.Shape.eval_to_digest_string Period.Stable.V1.bin_shape_t);
  [%expect
    {|
    3e375b1870d4fd9ec526c6afedf1b55b
    841aa06981a02439ac52c8420215cb13
    |}]
;;
