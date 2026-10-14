(* Every persisted type's bin_prot digest, in one place. A change here means a new
   [Stable] version, never an edited expectation. *)

open! Core
open Billing_core

let%expect_test "stable digests" =
  let show name shape =
    printf "%-22s %s\n" name (Bin_prot.Shape.eval_to_digest_string shape)
  in
  show "Period" Period.Stable.V1.bin_shape_t;
  show "Interval" Interval.Stable.V1.bin_shape_t;
  show "Unit_price" Unit_price.Stable.V1.bin_shape_t;
  show "Price" Price.Stable.V1.bin_shape_t;
  show "Plan" Plan.Stable.V1.bin_shape_t;
  show "Line" Line.Stable.V1.bin_shape_t;
  show "Tax.Rate" Tax.Rate.Stable.V1.bin_shape_t;
  show "Rounding_rules" Rounding_rules.Stable.V1.bin_shape_t;
  show "Numbering" Numbering.Stable.V1.bin_shape_t;
  show "Invoice" Invoice.Stable.V1.bin_shape_t;
  show "Credit_note" Credit_note.Stable.V1.bin_shape_t;
  show "Receipt" Receipt.Stable.V1.bin_shape_t;
  show "Usage.Record" Usage.Record.Stable.V1.bin_shape_t;
  show "Subscription" Subscription.Stable.V1.bin_shape_t;
  show "Dunning" Dunning.Stable.V1.bin_shape_t;
  [%expect
    {|
    Period                 841aa06981a02439ac52c8420215cb13
    Interval               3e375b1870d4fd9ec526c6afedf1b55b
    Unit_price             63a82ed596495471f1666a130b7afd04
    Price                  80fc1522fd0e269c0e5d8bab3df12ea1
    Plan                   70f8287a49904fecf11b4e3cb30a5694
    Line                   d045ea7045c6cad777a67350ca2c13c1
    Tax.Rate               54b6b7375d44cf916c99a0a8699e63d2
    Rounding_rules         cea4fbc15407bf53afe630f0b582c04b
    Numbering              0d106f4fa009a96eb60459cca71fddc4
    Invoice                304f19046448ab1c640fc5c61963f5e8
    Credit_note            d18c8919640ffd493a93b2c62ab8649d
    Receipt                c7d5bb6c1f28842bc6c5b967eafc8ad4
    Usage.Record           3cfaa7c99a0c4707f44ec9b8afea4bfd
    Subscription           0150f6bcd934f1bc087ea21abeb653cd
    Dunning                a4f6383359107613304dc19ae2cbf0d1
    |}]
;;

let round_trip (type a) (module M : Binable.S with type t = a) ~equal (value : a) =
  assert (
    equal
      value
      (Bin_prot.Reader.of_string
         M.bin_reader_t
         (Bin_prot.Writer.to_string M.bin_writer_t value)))
;;

let%expect_test "plans, numbering and validation on decode" =
  let plan =
    Plan.flat
      ~id:(Plan_id.of_string "basic")
      ~name:"Basic"
      ~interval:Interval.quarterly
      ~trial_days:30
      ~features:[ "exports" ]
      (Money.Amount.of_string "90.00 EUR")
    |> Or_error.ok_exn
  in
  round_trip (module Plan.Stable.V1) ~equal:Plan.equal plan;
  print_s [%sexp (plan : Plan.t)];
  let series = Numbering.create ~width:4 ~reset:Never "RCPT" |> Or_error.ok_exn in
  print_endline
    (Numbering.format series ~issued_on:(Date.of_string "2026-05-01") ~seq:12_345);
  print_s [%sexp (Numbering.create "inv" : Numbering.t Or_error.t)];
  (* A hand-edited record that breaks a rule does not decode. *)
  print_s
    [%sexp
      (Or_error.try_with (fun () ->
         Period.t_of_sexp (Sexp.of_string "((start 2026-02-01) (stop 2026-01-01))"))
       : Period.t Or_error.t)];
  [%expect
    {|
    ((id basic) (name Basic) (currency EUR) (interval (Months 3))
     (components (((code plan) (description Basic) (price (Flat "90.00 EUR")))))
     (trial_days 30) (features (exports)))
    RCPT-12345
    (Error
     ("Billing_core.Numbering: prefix must be 1-16 of A-Z 0-9 -" (prefix inv)))
    (Error
     ("Billing_core.Period: start must be before stop" (start 2026-02-01)
      (stop 2026-01-01)))
    |}]
;;
