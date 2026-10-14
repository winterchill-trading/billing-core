open! Core
open Billing_core

let amount s = Money.Amount.of_string (s ^ " EUR")
let vat = Tax.Rate.create ~name:"vat-standard" ~bps:2000 |> Or_error.ok_exn

let invoice =
  Invoice.draft
    ~id:(Invoice_id.of_string "inv-9")
    ~customer:(Customer_id.of_string "customer-9")
    ~currency:Money.Currency.eur
    ~tax:(Tax.flat vat)
    ~rounding:Rounding_rules.default
    [ Line.create
        ~code:"platform"
        ~description:"Growth plan, March"
        ~kind:Recurring
        ~quantity:1
        (amount "50.00")
      |> Or_error.ok_exn
    ]
  |> Or_error.ok_exn
;;

let%expect_test "receipts per payment" =
  print_s
    [%sexp
      (Receipt.of_payment invoice (Payment_id.of_string "p1")
       |> Result.map_error ~f:Refusal.code
       : (Receipt.t, string) Result.t)];
  let issued =
    Invoice.finalize
      invoice
      ~number:"INV-2026-000009"
      ~issued_on:(Date.of_string "2026-03-01")
      ~due_on:(Date.of_string "2026-03-15")
    |> Result.ok
    |> Option.value_exn
  in
  let pay (t : Invoice.t) id s =
    Invoice.pay
      t
      { id = Payment_id.of_string id
      ; amount = amount s
      ; paid_at = Time_ns.of_string_with_utc_offset "2026-03-05 12:00:00Z"
      }
    |> Result.ok
    |> Option.value_exn
    |> fst
  in
  let paid = pay (pay issued "p1" "20.00") "p2" "40.00" in
  List.iter [ "p1"; "p2" ] ~f:(fun id ->
    print_endline
      (Receipt.of_payment paid (Payment_id.of_string id)
       |> Result.ok
       |> Option.value_exn
       |> Receipt.to_text);
    print_endline "--");
  print_s
    [%sexp
      (Receipt.of_payment paid (Payment_id.of_string "p3")
       |> Result.map_error ~f:Refusal.code
       : (Receipt.t, string) Result.t)];
  [%expect
    {|
    (Error INVALID_STATE)
    Receipt for invoice INV-2026-000009
    Payment p1
    Paid at 2026-03-05 12:00:00 UTC
      Growth plan, March: 50.00 EUR
      tax vat-standard: 10.00 EUR
    Invoice total: 60.00 EUR
    This payment: 20.00 EUR
    Paid to date: 20.00 EUR
    Balance: 40.00 EUR
    --
    Receipt for invoice INV-2026-000009
    Payment p2
    Paid at 2026-03-05 12:00:00 UTC
      Growth plan, March: 50.00 EUR
      tax vat-standard: 10.00 EUR
    Invoice total: 60.00 EUR
    This payment: 40.00 EUR
    Paid to date: 60.00 EUR
    Balance: 0.00 EUR
    --
    (Error NOT_FOUND)
    |}]
;;

let%expect_test "stable digest" =
  print_endline (Bin_prot.Shape.eval_to_digest_string Receipt.Stable.V1.bin_shape_t);
  [%expect {| c7d5bb6c1f28842bc6c5b967eafc8ad4 |}]
;;
