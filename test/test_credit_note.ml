open! Core
open Billing_core

let eur = Money.Currency.eur
let amount s = Money.Amount.of_string (s ^ " EUR")
let vat = Tax.Rate.create ~name:"vat-standard" ~bps:1900 |> Or_error.ok_exn
let reduced = Tax.Rate.create ~name:"vat-reduced" ~bps:700 |> Or_error.ok_exn

let tax =
  Tax.create ~name:"by-code" (fun ~customer:_ (line : Line.t) ->
    if String.equal line.code "books" then [ reduced ] else [ vat ])
;;

let line code s =
  Line.create ~code ~description:code ~kind:One_off ~quantity:1 (amount s)
  |> Or_error.ok_exn
;;

let issued () =
  Invoice.draft
    ~id:(Invoice_id.of_string "inv-7")
    ~customer:(Customer_id.of_string "customer-1")
    ~currency:eur
    ~tax
    ~rounding:Rounding_rules.default
    [ line "platform" "100.00"; line "books" "10.00" ]
  |> Or_error.ok_exn
  |> Invoice.finalize
       ~number:"INV-2026-000007"
       ~issued_on:(Date.of_string "2026-03-01")
       ~due_on:(Date.of_string "2026-03-31")
  |> Result.ok
  |> Option.value_exn
;;

let pay (t : Invoice.t) id s =
  Invoice.pay
    t
    { id = Payment_id.of_string id
    ; amount = amount s
    ; paid_at = Time_ns.of_string_with_utc_offset "2026-03-02 10:00:00Z"
    }
  |> Result.ok
  |> Option.value_exn
  |> fst
;;

let note ?(id = "cn-1") invoice credits =
  Credit_note.create
    ~id:(Credit_note_id.of_string id)
    ~number:"CN-2026-000001"
    ~issued_on:(Date.of_string "2026-03-10")
    ~reason:Service_issue
    invoice
    credits
;;

let show = function
  | Ok (cn : Credit_note.t) ->
    print_s
      [%sexp
        { subtotal : Money.Amount.t = cn.subtotal
        ; tax_total : Money.Amount.t = cn.tax_total
        ; total : Money.Amount.t = cn.total
        ; against_due : Money.Amount.t = cn.against_due
        ; refund : Money.Amount.t = cn.refund
        }]
  | Error refusal -> print_s [%sexp (refusal : Refusal.t)]
;;

let%expect_test "a partial credit on a part-paid invoice reduces the amount due" =
  let invoice = pay (issued ()) "p1" "100.00" in
  print_s [%sexp (Invoice.amount_due invoice : Money.Amount.t)];
  show (note invoice [ 0, amount "30.00" ]);
  [%expect
    {|
    "29.70 EUR"
    ((subtotal "30.00 EUR") (tax_total "5.70 EUR") (total "35.70 EUR")
     (against_due "29.70 EUR") (refund "6.00 EUR"))
    |}]
;;

let%expect_test "a full credit on a paid invoice is all refund; nothing more can be \
                 credited"
  =
  let invoice = pay (issued ()) "p1" "129.70" in
  let cn =
    Credit_note.full
      ~id:(Credit_note_id.of_string "cn-1")
      ~number:"CN-1"
      ~issued_on:(Date.of_string "2026-03-10")
      ~reason:Duplicate
      invoice
  in
  show cn;
  let cn = Result.ok cn |> Option.value_exn in
  let invoice, _ =
    Invoice.credit invoice ~credit_note:cn.id ~amount:cn.total
    |> Result.ok
    |> Option.value_exn
  in
  print_s [%sexp (Invoice.overpaid invoice : Money.Amount.t)];
  show (note ~id:"cn-2" invoice [ 1, amount "0.01" ]);
  [%expect
    {|
    ((subtotal "110.00 EUR") (tax_total "19.70 EUR") (total "129.70 EUR")
     (against_due "0.00 EUR") (refund "129.70 EUR"))
    "129.70 EUR"
    (Over_credit (creditable "0.00 EUR") (requested "0.01 EUR"))
    |}]
;;

let%expect_test "refusals" =
  let invoice = issued () in
  show (note invoice []);
  show (note invoice [ 5, amount "1.00" ]);
  show (note invoice [ 0, amount "100.01" ]);
  show (note invoice [ 0, amount "1.00"; 0, amount "2.00" ]);
  show (note invoice [ 0, Money.Amount.of_string "1.00 USD" ]);
  [%expect
    {|
    (Invalid "Billing_core.Credit_note: nothing to credit")
    (Invalid ("Billing_core.Credit_note: no such line" (index 5)))
    (Invalid
     ("Billing_core.Credit_note: credit above the line amount" (index 0)
      (amount "100.01 EUR")))
    (Invalid "Billing_core.Credit_note: a line is credited twice")
    (Currency_mismatch (expected EUR) (got USD))
    |}]
;;

let%expect_test "stable digest" =
  print_endline (Bin_prot.Shape.eval_to_digest_string Credit_note.Stable.V1.bin_shape_t);
  [%expect {| d18c8919640ffd493a93b2c62ab8649d |}]
;;
