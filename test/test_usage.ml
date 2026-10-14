open! Core
open Billing_core

let subscription = Subscription_id.of_string "sub-1"
let calls = Meter.of_string "api_calls"
let volume = Meter.of_string "card_volume"
let zone = Time_float.Zone.utc
let base = Time_ns.of_string_with_utc_offset "2026-03-01 00:00:00Z"

let record ?(meter = calls) id ~minutes value =
  Usage.Record.create
    ~id:(Usage_id.of_string id)
    ~subscription
    ~meter
    ~occurred_at:(Time_ns.add base (Time_ns.Span.of_min (Float.of_int minutes)))
    value
  |> Or_error.ok_exn
;;

let%expect_test "duplicates and conflicts" =
  let r = record "u1" ~minutes:5 (Units 10) in
  let log, o1 = Usage.Log.ingest Usage.Log.empty r |> Result.ok |> Option.value_exn in
  let _, o2 = Usage.Log.ingest log r |> Result.ok |> Option.value_exn in
  let conflict = Usage.Log.ingest log (record "u1" ~minutes:5 (Units 11)) in
  print_s
    [%sexp
      (o1 : Usage.Outcome.t)
      , (o2 : Usage.Outcome.t)
      , (Result.map conflict ~f:snd : (Usage.Outcome.t, Refusal.t) Result.t)];
  print_s
    [%sexp
      (Usage.Record.create
         ~id:(Usage_id.of_string "u2")
         ~subscription
         ~meter:calls
         ~occurred_at:base
         (Units (-1))
       : Usage.Record.t Or_error.t)];
  [%expect
    {|
    (Accepted Duplicate (Error (Usage_conflict u1)))
    (Error ("Billing_core.Usage: negative units" (id u2) (n -1)))
    |}]
;;

let%expect_test "summaries by meter and period" =
  let records =
    [ record "u1" ~minutes:5 (Units 10)
    ; record "u2" ~minutes:(60 * 24 * 31) (Units 7) (* 1 April: next period *)
    ; record "u3" ~minutes:20 (Units 5)
    ; record ~meter:volume "v1" ~minutes:30 (Volume (Money.Amount.of_string "120.00 EUR"))
    ; record ~meter:volume "v2" ~minutes:40 (Volume (Money.Amount.of_string "-20.00 EUR"))
    ]
  in
  let march =
    Period.create_exn
      ~start:(Date.of_string "2026-03-01")
      ~stop:(Date.of_string "2026-04-01")
  in
  print_s
    [%sexp (Usage.Summary.of_records records ~meter:calls ~zone march : Usage.Summary.t)];
  print_s
    [%sexp (Usage.Summary.of_records records ~meter:volume ~zone march : Usage.Summary.t)];
  [%expect
    {|
    ((units 15) (volumes ()) (records 2))
    ((units 0) (volumes ("120.00 EUR" "-20.00 EUR")) (records 2))
    |}]
;;

let%test_unit "ingesting with resends in any order gives the same log" =
  let gen =
    let open Quickcheck.Let_syntax in
    let%bind n = Int.gen_incl 1 20 in
    let records =
      List.init n ~f:(fun i -> record (sprintf "u%d" i) ~minutes:(i * 7) (Units (i + 1)))
    in
    let%bind resends = List.gen_with_length (2 * n) (Int.gen_incl 0 (n - 1)) in
    let%map order =
      List.gen_permutations (records @ List.map resends ~f:(List.nth_exn records))
    in
    records, order
  in
  Quickcheck.test
    gen
    ~trials:200
    ~sexp_of:[%sexp_of: Usage.Record.t list * Usage.Record.t list]
    ~f:(fun (records, order) ->
      let log =
        List.fold order ~init:Usage.Log.empty ~f:(fun log r ->
          Usage.Log.ingest log r |> Result.ok |> Option.value_exn |> fst)
      in
      [%test_eq: Usage.Record.t list] (Usage.Log.records log) records)
;;

let%expect_test "stable digest" =
  print_endline (Bin_prot.Shape.eval_to_digest_string Usage.Record.Stable.V1.bin_shape_t);
  [%expect {| 3cfaa7c99a0c4707f44ec9b8afea4bfd |}]
;;
