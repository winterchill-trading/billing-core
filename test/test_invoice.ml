open! Core
open Billing_core

let eur = Money.Currency.eur
let amount s = Money.Amount.of_string (s ^ " EUR")
let customer = Customer_id.of_string "customer-1"
let vat = Tax.Rate.create ~name:"vat-standard" ~bps:1900 |> Or_error.ok_exn
let at = Time_ns.of_string_with_utc_offset "2026-03-01 09:00:00Z"

let line ?(kind = Line.Kind.One_off) code s =
  Line.create ~code ~description:code ~kind ~quantity:1 (amount s) |> Or_error.ok_exn
;;

let lines = [ line "a" "0.05"; line "b" "0.05"; line "c" "0.05" ]

let draft ?(tax = Tax.flat vat) ?(rounding = Rounding_rules.default) lines =
  Invoice.draft
    ~id:(Invoice_id.of_string "inv-1")
    ~customer
    ~currency:eur
    ~tax
    ~rounding
    lines
  |> Or_error.ok_exn
;;

let show_totals (t : Invoice.t) =
  print_s
    [%sexp
      { subtotal : Money.Amount.t = t.subtotal
      ; taxes : Invoice.Tax_line.t list = t.taxes
      ; total : Money.Amount.t = t.total
      }]
;;

let%expect_test "tax rounds once per rate or on every line" =
  show_totals (draft lines);
  show_totals
    (draft ~rounding:{ Rounding_rules.default with tax_basis = Per_line } lines);
  [%expect
    {|
    ((subtotal "0.15 EUR")
     (taxes
      (((rate ((name vat-standard) (bps 1900))) (taxable "0.15 EUR")
        (amount "0.03 EUR"))))
     (total "0.18 EUR"))
    ((subtotal "0.15 EUR")
     (taxes
      (((rate ((name vat-standard) (bps 1900))) (taxable "0.15 EUR")
        (amount "0.03 EUR"))))
     (total "0.18 EUR"))
    |}]
;;

let reduced = Tax.Rate.create ~name:"vat-reduced" ~bps:700 |> Or_error.ok_exn

let mixed_tax =
  Tax.create ~name:"by-code" (fun ~customer:_ (line : Line.t) ->
    match line.code with
    | "books" -> [ reduced ]
    | "exempt" -> []
    | _ -> [ vat ])
;;

let%expect_test "a tax hook chooses rates per line" =
  let t =
    draft
      ~tax:mixed_tax
      [ line "platform" "100.00"; line "books" "10.00"; line "exempt" "5.00" ]
  in
  show_totals t;
  [%expect
    {|
    ((subtotal "115.00 EUR")
     (taxes
      (((rate ((name vat-standard) (bps 1900))) (taxable "100.00 EUR")
        (amount "19.00 EUR"))
       ((rate ((name vat-reduced) (bps 700))) (taxable "10.00 EUR")
        (amount "0.70 EUR"))))
     (total "134.70 EUR"))
    |}]
;;

let open_invoice () =
  let t = draft [ line "platform" "100.00" ] in
  Invoice.finalize
    t
    ~number:"INV-2026-000001"
    ~issued_on:(Date.of_string "2026-03-01")
    ~due_on:(Date.of_string "2026-03-15")
  |> Result.ok
  |> Option.value_exn
;;

let show_result r =
  match r with
  | Ok ((t : Invoice.t), outcome) ->
    let amount_due = Invoice.amount_due t in
    print_s
      [%sexp
        (outcome : [ `Applied | `Duplicate ])
      , (t.status : Invoice.Status.t)
      , (amount_due : Money.Amount.t)]
  | Error refusal -> print_s [%sexp (Refusal.code refusal : string), (refusal : Refusal.t)]
;;

let payment id s = { Invoice.Payment.id = Payment_id.of_string id; amount = amount s; paid_at = at }

let%expect_test "payments: partial, duplicate, conflict, overpayment, settled" =
  let t = open_invoice () in
  print_s [%sexp (t.status : Invoice.Status.t), (Invoice.amount_due t : Money.Amount.t)];
  let r1 = Invoice.pay t (payment "p1" "50.00") in
  show_result r1;
  let t1, _ = Result.ok r1 |> Option.value_exn in
  show_result (Invoice.pay t1 (payment "p1" "50.00"));
  show_result (Invoice.pay t1 (payment "p1" "40.00"));
  show_result (Invoice.pay t1 (payment "p2" "70.00"));
  show_result (Invoice.pay t1 (payment "p2" "69.00"));
  let t2, _ = Invoice.pay t1 (payment "p2" "69.00") |> Result.ok |> Option.value_exn in
  show_result (Invoice.pay t2 (payment "p3" "1.00"));
  print_s [%sexp (Invoice.void t2 |> Result.map_error ~f:Refusal.code : (Invoice.t, string) Result.t)];
  [%expect
    {|
    (Open "119.00 EUR")
    (Applied Open "69.00 EUR")
    (Duplicate Open "69.00 EUR")
    (PAYMENT_CONFLICT (Payment_conflict p1))
    (OVERPAYMENT (Overpayment (due "69.00 EUR") (offered "70.00 EUR")))
    (Applied Paid "0.00 EUR")
    (INVALID_STATE
     (Invalid_state (what invoice) (id inv-1) (state paid) (action pay)))
    (Error INVALID_STATE)
    |}]
;;

let%expect_test "credits settle an open invoice and are capped by the total" =
  let t = open_invoice () in
  let cn id = Credit_note_id.of_string id in
  let t1, _ =
    Invoice.credit t ~credit_note:(cn "cn-1") ~amount:(amount "19.00")
    |> Result.ok
    |> Option.value_exn
  in
  let t2, _ = Invoice.pay t1 (payment "p1" "100.00") |> Result.ok |> Option.value_exn in
  print_s [%sexp (t2.status : Invoice.Status.t)];
  show_result (Invoice.credit t2 ~credit_note:(cn "cn-2") ~amount:(amount "100.01"));
  show_result (Invoice.credit t2 ~credit_note:(cn "cn-2") ~amount:(amount "100.00"));
  let t3, _ =
    Invoice.credit t2 ~credit_note:(cn "cn-2") ~amount:(amount "100.00")
    |> Result.ok
    |> Option.value_exn
  in
  print_s [%sexp (Invoice.overpaid t3 : Money.Amount.t)];
  [%expect
    {|
    Paid
    (OVER_CREDIT
     (Over_credit (creditable "100.00 EUR") (requested "100.01 EUR")))
    (Applied Paid "0.00 EUR")
    "100.00 EUR"
    |}]
;;

let%expect_test "drafts: negative totals cannot be issued, zero totals are paid at once" =
  let issue t =
    Invoice.finalize
      t
      ~number:"N-1"
      ~issued_on:(Date.of_string "2026-03-01")
      ~due_on:(Date.of_string "2026-03-01")
    |> Result.map ~f:(fun (t : Invoice.t) -> t.status)
    |> Result.map_error ~f:Refusal.code
  in
  let credit = line ~kind:Proration "unused" "-30.00" in
  print_s [%sexp (issue (draft [ credit ]) : (Invoice.Status.t, string) Result.t)];
  print_s [%sexp (issue (draft [ credit; line "plan" "30.00" ]) : (Invoice.Status.t, string) Result.t)];
  print_s
    [%sexp
      (Line.create ~code:"x" ~description:"x" ~kind:Recurring ~quantity:1 (amount "-1.00")
       : Line.t Or_error.t)];
  [%expect
    {|
    (Error NEGATIVE_TOTAL)
    (Ok Paid)
    (Error
     ("Billing_core.Line: only prorations and one-off lines may credit" (code x)))
    |}]
;;

let%test_unit "totals identity and amount due over random payments" =
  let gen =
    let open Quickcheck.Let_syntax in
    let%bind lines = List.gen_with_length 4 (Int.gen_incl (-5_000) 50_000) in
    let%map payments = List.gen_with_length 6 (Int.gen_incl 1 40_000) in
    lines, payments
  in
  Quickcheck.test gen ~sexp_of:[%sexp_of: int list * int list] ~f:(fun (cents, payments) ->
    let lines =
      List.mapi cents ~f:(fun i c ->
        Line.create
          ~code:(sprintf "l%d" i)
          ~description:"line"
          ~kind:Proration
          ~quantity:1
          (Money.Amount.of_minor_units_int eur c)
        |> Or_error.ok_exn)
    in
    let t = draft ~tax:mixed_tax lines in
    [%test_eq: Money.Amount.t]
      t.total
      (Money.Amount.add_exn t.subtotal t.tax_total);
    match
      Invoice.finalize
        t
        ~number:"N-1"
        ~issued_on:(Date.of_string "2026-01-01")
        ~due_on:(Date.of_string "2026-01-10")
    with
    | Error _ -> assert (Money.Amount.is_negative t.total)
    | Ok t ->
      let final =
        List.foldi payments ~init:t ~f:(fun i t c ->
          match
            Invoice.pay t (payment (sprintf "p%d" i) (Money.Amount.to_decimal_string (Money.Amount.of_minor_units_int eur c)))
          with
          | Ok (t, _) -> t
          | Error _ -> t)
      in
      [%test_eq: Money.Amount.t]
        (Money.Amount.add_exn (Invoice.amount_paid final) (Invoice.amount_due final))
        final.total;
      [%test_eq: bool]
        (Invoice.Status.equal final.status Paid)
        (Money.Amount.is_zero (Invoice.amount_due final));
      let round_trip =
        Bin_prot.Reader.of_string
          Invoice.Stable.V1.bin_reader_t
          (Bin_prot.Writer.to_string Invoice.Stable.V1.bin_writer_t final)
      in
      [%test_eq: Invoice.t] round_trip final)
;;

let%expect_test "stable digest" =
  print_endline (Bin_prot.Shape.eval_to_digest_string Invoice.Stable.V1.bin_shape_t);
  [%expect {| 4e6dbe7cffa223d16f5f21797d531fac |}]
;;
