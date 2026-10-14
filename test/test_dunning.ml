open! Core
open Billing_core

let d = Date.of_string
let invoice = Invoice_id.of_string "inv-1"
let subscription = Subscription_id.of_string "sub-1"

let%expect_test "retries, then exhaustion with the policy's final actions" =
  let policy = Dunning.Policy.default in
  let t, actions =
    Dunning.start
      policy
      ~invoice
      ~subscription
      ~on:(d "2026-03-01")
      ~reason:"insufficient_funds"
      ()
  in
  let step (t, actions) =
    print_s [%sexp (t.Dunning.state : Dunning.State.t), (actions : Dunning.Action.t list)];
    t
  in
  let t = step (t, actions) in
  let t = step (Dunning.tick t ~today:(d "2026-03-03")) in
  let t = step (Dunning.tick t ~today:(d "2026-03-04")) in
  let fail t on =
    Dunning.apply policy t (Payment_failed { on = d on; reason = "insufficient_funds" })
    |> Result.ok
    |> Option.value_exn
  in
  let t = step (fail t "2026-03-04") in
  let t = step (Dunning.tick t ~today:(d "2026-03-09")) in
  let t = step (fail t "2026-03-09") in
  let t = step (Dunning.tick t ~today:(d "2026-03-16")) in
  let t = step (fail t "2026-03-16") in
  print_s
    [%sexp
      (Dunning.apply policy t (Payment_succeeded { on = d "2026-03-17" })
       |> Result.map_error ~f:Refusal.code
       |> Result.is_error
       : bool)];
  [%expect
    {|
    ((Waiting (retries_done 0) (next_retry_on 2026-03-04))
     (Mark_past_due (Notify_customer (failures 1) (next_retry_on (2026-03-04)))))
    ((Waiting (retries_done 0) (next_retry_on 2026-03-04)) ())
    ((Collecting (retry 1)) ((Retry_payment (retry 1))))
    ((Waiting (retries_done 1) (next_retry_on 2026-03-09))
     ((Notify_customer (failures 2) (next_retry_on (2026-03-09)))))
    ((Collecting (retry 2)) ((Retry_payment (retry 2))))
    ((Waiting (retries_done 2) (next_retry_on 2026-03-16))
     ((Notify_customer (failures 3) (next_retry_on (2026-03-16)))))
    ((Collecting (retry 3)) ((Retry_payment (retry 3))))
    ((Exhausted 2026-03-16)
     ((Notify_customer (failures 4) (next_retry_on ())) Mark_uncollectible
      Cancel_subscription))
    true
    |}]
;;

let%test_unit "dunning terminates within the policy's retries" =
  let gen =
    let open Quickcheck.Let_syntax in
    let%bind n = Int.gen_incl 0 4 in
    let%bind waits = List.gen_with_length n (Int.gen_incl 1 10) in
    let%map events =
      List.gen_non_empty
        (Quickcheck.Generator.of_list [ `Tick; `Fail; `Fail; `Tick; `Tick; `Succeed ])
    in
    waits, events
  in
  Quickcheck.test
    gen
    ~sexp_of:[%sexp_of: int list * [ `Tick | `Fail | `Succeed ] list]
    ~f:(fun (waits, events) ->
      let policy =
        Dunning.Policy.create
          ~retry_after_days:waits
          ~cancel_subscription:true
          ~mark_uncollectible:false
        |> Or_error.ok_exn
      in
      let t, _ = Dunning.start policy ~invoice ~on:(d "2026-01-01") ~reason:"x" () in
      let today = ref (d "2026-01-01") in
      let retries = ref 0 in
      let final =
        List.fold events ~init:t ~f:(fun t event ->
          today := Date.add_days !today 5;
          match event with
          | `Tick ->
            let t, actions = Dunning.tick t ~today:!today in
            retries
            := !retries
               + List.count actions ~f:(function
                 | Retry_payment _ -> true
                 | _ -> false);
            t
          | `Fail ->
            (match
               Dunning.apply policy t (Payment_failed { on = !today; reason = "x" })
             with
             | Ok (t, actions) ->
               (* No subscription was given, so cancelling is never asked for. *)
               assert (
                 not (List.mem actions Cancel_subscription ~equal:Dunning.Action.equal));
               t
             | Error _ ->
               assert (Dunning.State.is_final t.state);
               t)
          | `Succeed ->
            (match Dunning.apply policy t (Payment_succeeded { on = !today }) with
             | Ok (t, _) -> t
             | Error _ ->
               assert (Dunning.State.is_final t.state);
               t))
      in
      assert (!retries <= List.length waits);
      match final.state with
      | Exhausted _ -> [%test_eq: int] !retries (List.length waits)
      | _ -> ())
;;

let%expect_test "stable digest" =
  print_endline (Bin_prot.Shape.eval_to_digest_string Dunning.Stable.V1.bin_shape_t);
  [%expect {| a4f6383359107613304dc19ae2cbf0d1 |}]
;;
