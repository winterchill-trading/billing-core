open! Core
open Billing_core

let eur = Money.Currency.eur
let amount s = Money.Amount.of_string (s ^ " EUR")
let up s = Unit_price.of_string eur s |> Or_error.ok_exn
let meter = Meter.of_string "api_calls"

let tiers =
  [ Price.Tier.create ~up_to:1_000 (up "0") |> Or_error.ok_exn
  ; Price.Tier.create ~up_to:10_000 ~flat:(amount "5.00") (up "0.002") |> Or_error.ok_exn
  ; Price.Tier.create (up "0.0015") |> Or_error.ok_exn
  ]
;;

let%expect_test "graduated and volume tiers" =
  List.iter [ 0; 1; 1_000; 1_001; 10_000; 25_000 ] ~f:(fun quantity ->
    let price mode =
      Price.tiered_amount mode tiers ~quantity ~rounding:Half_up |> Money.Amount.to_string
    in
    printf "%6d  graduated %-10s volume %s\n" quantity (price Graduated) (price Volume));
  [%expect
    {|
         0  graduated 0.00 EUR   volume 0.00 EUR
         1  graduated 0.00 EUR   volume 0.00 EUR
      1000  graduated 0.00 EUR   volume 0.00 EUR
      1001  graduated 5.00 EUR   volume 7.00 EUR
     10000  graduated 23.00 EUR  volume 25.00 EUR
     25000  graduated 45.50 EUR  volume 37.50 EUR
    |}]
;;

let%expect_test "invalid tier lists" =
  let show tiers =
    print_s [%sexp (Price.tiered meter Graduated tiers : Price.t Or_error.t)]
  in
  show [];
  show [ Price.Tier.create ~up_to:10 (up "1") |> Or_error.ok_exn ];
  show
    [ Price.Tier.create ~up_to:10 (up "1") |> Or_error.ok_exn
    ; Price.Tier.create ~up_to:10 (up "1") |> Or_error.ok_exn
    ; Price.Tier.create (up "1") |> Or_error.ok_exn
    ];
  show
    [ Price.Tier.create ~up_to:10 (up "1") |> Or_error.ok_exn
    ; Price.Tier.create (Unit_price.of_string Money.Currency.usd "1" |> Or_error.ok_exn)
      |> Or_error.ok_exn
    ];
  [%expect
    {|
    (Error "Billing_core.Price: a tiered price needs a tier")
    (Error "Billing_core.Price: the last tier must be open-ended")
    (Error ("Billing_core.Price: tier bounds must increase" (up_to 10)))
    (Error "Billing_core.Price: tiers in different currencies")
    |}]
;;

let%expect_test "percentage fees per event and per period" =
  let fee =
    Money.Fee.create
      ~bps_q:(Q.of_string "25")
      ~min:(amount "0.10")
      ~max:(amount "50.00")
      eur
    |> Or_error.ok_exn
  in
  let volumes = [ amount "10.00"; amount "1000.00"; amount "30000.00" ] in
  List.iter Price.Fee_basis.all ~f:(fun basis ->
    let price = Price.percentage ~basis (Meter.of_string "card_volume") fee in
    let charged =
      Price.usage price ~units:0 ~volumes ~rounding:Half_up |> Or_error.ok_exn
    in
    let nothing =
      Price.usage price ~units:0 ~volumes:[] ~rounding:Half_up |> Or_error.ok_exn
    in
    print_s
      [%sexp
        (basis : Price.Fee_basis.t)
        , (charged : Money.Amount.t)
        , (nothing : Money.Amount.t)]);
  [%expect
    {|
    (Per_event "52.60 EUR" "0.00 EUR")
    (Per_period "50.00 EUR" "0.00 EUR")
    |}]
;;

let%expect_test "per seat above the included seats" =
  let price = Price.per_seat ~included:3 (amount "12.00") |> Or_error.ok_exn in
  List.iter [ 0; 3; 4; 10 ] ~f:(fun seats ->
    print_endline (Money.Amount.to_string (Price.recurring price ~seats)));
  [%expect {|
    0.00 EUR
    0.00 EUR
    12.00 EUR
    84.00 EUR
    |}]
;;

let%test_unit "graduated pricing is monotone and never negative" =
  Quickcheck.test
    (Quickcheck.Generator.tuple2 (Int.gen_incl 0 50_000) (Int.gen_incl 0 50_000))
    ~sexp_of:[%sexp_of: int * int]
    ~f:(fun (a, b) ->
      let lo = Int.min a b
      and hi = Int.max a b in
      let g q = Price.tiered_amount Graduated tiers ~quantity:q ~rounding:Half_up in
      assert (Money.Amount.( <= ) (g lo) (g hi));
      assert (not (Money.Amount.is_negative (g lo))))
;;

let%expect_test "stable digest and round trip" =
  let price = Price.tiered meter Graduated tiers |> Or_error.ok_exn in
  let bin = Bin_prot.Writer.to_string Price.Stable.V1.bin_writer_t price in
  assert (Price.equal price (Bin_prot.Reader.of_string Price.Stable.V1.bin_reader_t bin));
  assert (Price.equal price (Price.t_of_sexp (Price.sexp_of_t price)));
  print_endline (Bin_prot.Shape.eval_to_digest_string Price.Stable.V1.bin_shape_t);
  [%expect {| 80fc1522fd0e269c0e5d8bab3df12ea1 |}]
;;
