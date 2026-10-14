open! Core

module Policy = struct
  type t =
    { retry_after_days : int list
    ; cancel_subscription : bool
    ; mark_uncollectible : bool
    }
  [@@deriving sexp, compare, equal]

  let create ~retry_after_days ~cancel_subscription ~mark_uncollectible =
    if List.exists retry_after_days ~f:(fun d -> d < 1)
    then Or_error.error_s [%message "Billing_core.Dunning.Policy: waits must be >= 1 day"]
    else if List.length retry_after_days > 20
    then Or_error.error_s [%message "Billing_core.Dunning.Policy: at most 20 retries"]
    else Ok { retry_after_days; cancel_subscription; mark_uncollectible }
  ;;

  let default =
    { retry_after_days = [ 3; 5; 7 ]; cancel_subscription = true; mark_uncollectible = true }
  ;;
end

module State = struct
  type t =
    | Waiting of
        { retries_done : int
        ; next_retry_on : Date.Stable.V1.t
        }
    | Collecting of { retry : int }
    | Recovered of Date.Stable.V1.t
    | Exhausted of Date.Stable.V1.t
    | Closed
  [@@deriving bin_io, sexp, compare, equal]

  let is_final = function
    | Waiting _ | Collecting _ -> false
    | Recovered _ | Exhausted _ | Closed -> true
  ;;
end

module Action = struct
  type t =
    | Mark_past_due
    | Notify_customer of
        { failures : int
        ; next_retry_on : Date.t option
        }
    | Retry_payment of { retry : int }
    | Cancel_subscription
    | Mark_uncollectible
  [@@deriving sexp, compare, equal]
end

module Event = struct
  type t =
    | Payment_failed of
        { on : Date.t
        ; reason : string
        }
    | Payment_succeeded of { on : Date.t }
    | Invoice_settled
  [@@deriving sexp, compare, equal]
end

module Repr = struct
  type t =
    { invoice : Invoice_id.Stable.V1.t
    ; subscription : Subscription_id.Stable.V1.t option
    ; state : State.t
    ; failures : (Date.Stable.V1.t * string) list
    ; revision : int
    }
  [@@deriving bin_io, sexp, compare, equal]
end

type t = Repr.t =
  { invoice : Invoice_id.t
  ; subscription : Subscription_id.t option
  ; state : State.t
  ; failures : (Date.t * string) list
  ; revision : int
  }
[@@deriving sexp, compare, equal]

(* After [retries_done] retries have failed (0 for the first failure), either the next
   wait or exhaustion. *)
let after_failure (policy : Policy.t) t ~on ~retries_done =
  let failures = List.length t.failures in
  match List.nth policy.retry_after_days retries_done with
  | Some wait ->
    let next_retry_on = Date.add_days on wait in
    ( { t with state = Waiting { retries_done; next_retry_on } }
    , [ Action.Notify_customer { failures; next_retry_on = Some next_retry_on } ] )
  | None ->
    ( { t with state = Exhausted on }
    , List.concat
        [ [ Action.Notify_customer { failures; next_retry_on = None } ]
        ; (if policy.mark_uncollectible then [ Action.Mark_uncollectible ] else [])
        ; (if policy.cancel_subscription && Option.is_some t.subscription
           then [ Action.Cancel_subscription ]
           else [])
        ] )
;;

let start policy ~invoice ?subscription ~on ~reason () =
  let t =
    { invoice
    ; subscription
    ; state = Closed (* replaced just below *)
    ; failures = [ on, reason ]
    ; revision = 0
    }
  in
  let t, actions = after_failure policy t ~on ~retries_done:0 in
  t, Action.Mark_past_due :: actions
;;

let bump t = { t with revision = t.revision + 1 }

let apply policy t (event : Event.t) =
  if State.is_final t.state
  then
    Refusal.invalid_state
      ~what:"dunning"
      ~id:(Invoice_id.to_string t.invoice)
      ~state:(Sexp.to_string (State.sexp_of_t t.state))
      ~action:(Sexp.to_string (Event.sexp_of_t event))
  else (
    match event with
    | Payment_succeeded { on } -> Ok (bump { t with state = Recovered on }, [])
    | Invoice_settled -> Ok (bump { t with state = Closed }, [])
    | Payment_failed { on; reason } ->
      let retries_done =
        match t.state with
        | Collecting { retry } -> retry
        | Waiting { retries_done; _ } -> retries_done
        | Recovered _ | Exhausted _ | Closed -> (* excluded by [is_final] above *) 0
      in
      let t = { t with failures = t.failures @ [ on, reason ] } in
      let t, actions = after_failure policy t ~on ~retries_done in
      Ok (bump t, actions))
;;

let tick t ~today =
  match t.state with
  | Waiting { retries_done; next_retry_on } when Date.( >= ) today next_retry_on ->
    let retry = retries_done + 1 in
    bump { t with state = Collecting { retry } }, [ Action.Retry_payment { retry } ]
  | Waiting _ | Collecting _ | Recovered _ | Exhausted _ | Closed -> t, []
;;

module Stable = struct
  module V1 = Repr
end
