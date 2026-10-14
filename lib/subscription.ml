open! Core

module Status = struct
  type t =
    | Trialing
    | Active
    | Past_due
    | Cancelled
  [@@deriving bin_io, sexp, compare, equal, enumerate]

  let to_string t = Sexp.to_string (sexp_of_t t) |> String.lowercase
end

module Repr = struct
  type t =
    { id : Subscription_id.Stable.V1.t
    ; customer : Customer_id.Stable.V1.t
    ; plan : Plan.Stable.V1.t
    ; seats : int
    ; started_on : Date.Stable.V1.t
    ; anchor : Date.Stable.V1.t
    ; status : Status.t
    ; opened : bool
    ; current : int
    ; pending_plan : Plan.Stable.V1.t option
    ; pending_lines : Line.Stable.V1.t list
    ; cancel_at_period_end : bool
    ; ended_on : Date.Stable.V1.t option
    ; revision : int
    }
  [@@deriving bin_io, sexp, compare, equal]
end

type t = Repr.t =
  { id : Subscription_id.t
  ; customer : Customer_id.t
  ; plan : Plan.t
  ; seats : int
  ; started_on : Date.t
  ; anchor : Date.t
  ; status : Status.t
  ; opened : bool
  ; current : int
  ; pending_plan : Plan.t option
  ; pending_lines : Line.t list
  ; cancel_at_period_end : bool
  ; ended_on : Date.t option
  ; revision : int
  }
[@@deriving sexp, compare, equal]

let create ~id ~customer ~(plan : Plan.t) ?(seats = 1) ~started_on () =
  if seats < 0
  then
    Or_error.error_s [%message "Billing_core.Subscription: negative seats" (seats : int)]
  else (
    let anchor = Date.add_days started_on plan.trial_days in
    Ok
      { id
      ; customer
      ; plan
      ; seats
      ; started_on
      ; anchor
      ; status = (if plan.trial_days > 0 then Trialing else Active)
      ; opened = false
      ; current = 0
      ; pending_plan = None
      ; pending_lines = []
      ; cancel_at_period_end = false
      ; ended_on = None
      ; revision = 0
      })
;;

let period t ~index = Interval.period t.plan.interval ~anchor:t.anchor ~index
let current_period t = period t ~index:t.current
let billed_through t = if t.opened then Some (current_period t).start else None
let bump t = { t with revision = t.revision + 1 }

let refuse t ~action =
  Refusal.invalid_state
    ~what:"subscription"
    ~id:(Subscription_id.to_string t.id)
    ~state:(Status.to_string t.status)
    ~action
;;

module Due = struct
  type t =
    | Nothing
    | Open
    | Close of int
  [@@deriving sexp, compare, equal]
end

let due t ~today : Due.t =
  match t.status with
  | Cancelled -> Nothing
  | Trialing | Active | Past_due ->
    if not t.opened
    then if Date.( >= ) today t.anchor then Open else Nothing
    else if Date.( >= ) today (current_period t).stop
    then Close t.current
    else Nothing
;;

let opened t =
  match t.status, t.opened with
  | (Trialing | Active | Past_due), false ->
    Ok
      (bump
         { t with
           opened = true
         ; pending_lines = []
         ; status =
             (match t.status with
              | Trialing -> Active
              | s -> s)
         })
  | _ -> refuse t ~action:"open"
;;

let ends_with_current_period t = t.cancel_at_period_end

let closed t =
  match t.status with
  | Cancelled | Trialing -> refuse t ~action:"close_period"
  | Active | Past_due ->
    if not t.opened
    then refuse t ~action:"close_period"
    else if t.cancel_at_period_end
    then
      Ok
        (bump
           { t with
             status = Cancelled
           ; ended_on = Some (current_period t).stop
           ; pending_lines = []
           ; pending_plan = None
           })
    else (
      let plan = Option.value t.pending_plan ~default:t.plan in
      let next = { t with current = t.current + 1 } in
      (* A pending plan with another interval restarts the period schedule where the next
         period starts, so the new interval counts from that day. *)
      let next =
        if Interval.equal plan.interval t.plan.interval
        then next
        else { next with anchor = (current_period t).stop; current = 0 }
      in
      Ok (bump { next with plan; pending_plan = None; pending_lines = [] }))
;;

let check_change t (plan : Plan.t) =
  if not (Money.Currency.equal plan.currency t.plan.currency)
  then
    Error (Refusal.Currency_mismatch { expected = t.plan.currency; got = plan.currency })
  else Ok ()
;;

let in_current t on =
  if Period.contains (current_period t) on
  then Ok ()
  else
    Error
      (Refusal.Invalid
         (Error.create_s
            [%message
              "Billing_core.Subscription: date outside the current period"
                (on : Date.t)
                ~period:(current_period t : Period.t)]))
;;

let prorate t ~on ~policy ~rounding ~new_plan ~new_seats =
  if not t.opened
  then Ok []
  else (
    let%map.Result () = in_current t on in
    Proration.lines
      ~policy
      ~rounding
      ~period:(current_period t)
      ~on
      ~old_plan:t.plan
      ~old_seats:t.seats
      ~new_plan
      ~new_seats)
;;

let change_plan t (plan : Plan.t) ~effective ~policy ~rounding =
  match t.status with
  | Cancelled -> refuse t ~action:"change_plan"
  | Trialing | Active | Past_due ->
    let%bind.Result () = check_change t plan in
    (match effective with
     | `Next_period ->
       let pending_plan = if Plan.equal plan t.plan then None else Some plan in
       Ok (bump { t with pending_plan }, [])
     | `Immediately on ->
       if t.opened && not (Interval.equal plan.interval t.plan.interval)
       then
         Error
           (Refusal.Invalid
              (Error.of_string
                 "Billing_core.Subscription: an immediate change cannot change the \
                  interval"))
       else (
         let%map.Result lines =
           prorate t ~on ~policy ~rounding ~new_plan:plan ~new_seats:t.seats
         in
         ( bump
             { t with
               plan
             ; pending_plan = None
             ; pending_lines = t.pending_lines @ lines
             ; anchor =
                 (if t.opened
                  then t.anchor
                  else Date.add_days t.started_on plan.trial_days)
             ; status =
                 (match t.status with
                  | Trialing when plan.trial_days = 0 -> Active
                  | s -> s)
             }
         , lines )))
;;

let set_seats t seats ~on ~policy ~rounding =
  match t.status with
  | Cancelled -> refuse t ~action:"set_seats"
  | Trialing | Active | Past_due ->
    if seats < 0
    then
      Error
        (Refusal.Invalid
           (Error.create_s
              [%message "Billing_core.Subscription: negative seats" (seats : int)]))
    else (
      let%map.Result lines =
        prorate t ~on ~policy ~rounding ~new_plan:t.plan ~new_seats:seats
      in
      bump { t with seats; pending_lines = t.pending_lines @ lines }, lines)
;;

let cancel t ~at =
  match t.status, at with
  | Cancelled, _ -> refuse t ~action:"cancel"
  | (Trialing | Active | Past_due), `At_period_end ->
    if t.opened
    then Ok (bump { t with cancel_at_period_end = true })
    else
      (* Nothing billed yet: ending at the "period end" means ending with the trial. *)
      Ok (bump { t with status = Cancelled; ended_on = Some t.anchor })
  | (Trialing | Active | Past_due), `Immediately on ->
    let%map.Result () = if t.opened then in_current t on else Ok () in
    bump { t with status = Cancelled; ended_on = Some on; pending_plan = None }
;;

let resume t =
  match t.status with
  | (Trialing | Active | Past_due) when t.cancel_at_period_end ->
    Ok (bump { t with cancel_at_period_end = false })
  | Trialing | Active | Past_due | Cancelled -> refuse t ~action:"resume"
;;

let mark_past_due t =
  match t.status with
  | Active -> Ok (bump { t with status = Past_due })
  | Past_due -> Ok t
  | Trialing | Cancelled -> refuse t ~action:"mark_past_due"
;;

let mark_recovered t =
  match t.status with
  | Past_due -> Ok (bump { t with status = Active })
  | Active | Cancelled -> Ok t
  | Trialing -> refuse t ~action:"mark_recovered"
;;

module Stable = struct
  module V1 = Repr
end
