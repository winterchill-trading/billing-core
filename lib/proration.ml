open! Core

module Policy = struct
  type t =
    | Credit_and_charge
    | Charge_only
    | No_proration
  [@@deriving bin_io, sexp, compare, equal, enumerate]
end

let remaining (period : Period.t) ~on =
  if not (Period.contains period on)
  then
    raise_s
      [%message "Billing_core.Proration.remaining: date outside period" (on : Date.t) (period : Period.t)];
  Q.of_ints (Date.diff period.stop on) (Period.days period)
;;

let line ~(rounding : Rounding_rules.t) ~period ~share ~code ~description full =
  let amount = Money.Amount.mul_ratio ~rounding:rounding.line full share in
  if Money.Amount.is_zero amount
  then None
  else
    Some
      (Line.create ~code ~description ~kind:Proration ~period ~quantity:1 amount
       |> Or_error.ok_exn)
;;

let lines
  ~policy
  ~rounding
  ~(period : Period.t)
  ~on
  ~(old_plan : Plan.t)
  ~old_seats
  ~(new_plan : Plan.t)
  ~new_seats
  =
  match (policy : Policy.t) with
  | No_proration -> []
  | Credit_and_charge | Charge_only ->
    let share = remaining period ~on in
    let rest = Period.create_exn ~start:on ~stop:period.stop in
    let advance (plan : Plan.t) =
      List.filter plan.components ~f:(fun c -> Price.in_advance c.price)
    in
    let all =
      if Plan_id.equal old_plan.id new_plan.id
      then
        List.filter_map (advance new_plan) ~f:(fun component ->
          let before =
            List.find (advance old_plan) ~f:(fun c -> String.equal c.code component.code)
            |> Option.value_map
                 ~default:(Money.Amount.zero new_plan.currency)
                 ~f:(fun c -> Price.recurring c.price ~seats:old_seats)
          in
          let after = Price.recurring component.price ~seats:new_seats in
          line
            ~rounding
            ~period:rest
            ~share
            ~code:component.code
            ~description:(sprintf "%s, change for remaining time" component.description)
            (Money.Amount.sub_exn after before))
      else
        List.filter_map (advance old_plan) ~f:(fun c ->
          line
            ~rounding
            ~period:rest
            ~share
            ~code:c.code
            ~description:(sprintf "Unused time on %s" c.description)
            (Money.Amount.neg (Price.recurring c.price ~seats:old_seats)))
        @ List.filter_map (advance new_plan) ~f:(fun c ->
          line
            ~rounding
            ~period:rest
            ~share
            ~code:c.code
            ~description:(sprintf "Remaining time on %s" c.description)
            (Price.recurring c.price ~seats:new_seats))
    in
    (match policy with
     | Charge_only -> List.filter all ~f:(fun l -> Money.Amount.is_positive l.amount)
     | Credit_and_charge | No_proration -> all)
;;
