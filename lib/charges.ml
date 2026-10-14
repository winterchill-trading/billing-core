open! Core

let in_advance (plan : Plan.t) ~seats ~period =
  List.filter_map plan.components ~f:(fun component ->
    if not (Price.in_advance component.price)
    then None
    else (
      let amount = Price.recurring component.price ~seats in
      if Money.Amount.is_zero amount
      then None
      else (
        let quantity, unit_price =
          match component.price with
          | Per_seat { unit_price; included } ->
            ( Int.max 0 (seats - included)
            , Some (Unit_price.of_amount unit_price |> Or_error.ok_exn) )
          | Flat _ | Tiered _ | Percentage _ -> 1, None
        in
        Some
          (Line.create
             ~code:component.code
             ~description:component.description
             ~kind:Recurring
             ~period
             ?unit_price
             ~quantity
             amount
           |> Or_error.ok_exn))))
;;

let in_arrears (plan : Plan.t) records ~zone ~period ~(rounding : Rounding_rules.t) =
  List.filter_map plan.components ~f:(fun component ->
    match Price.meter component.price with
    | None -> None
    | Some meter ->
      let summary = Usage.Summary.of_records records ~meter ~zone period in
      if summary.records = 0
      then None
      else
        Some
          (let%bind.Or_error amount =
             Price.usage
               component.price
               ~units:summary.units
               ~volumes:summary.volumes
               ~rounding:rounding.line
           in
           let quantity =
             match component.price with
             | Percentage _ -> summary.records
             | Flat _ | Per_seat _ | Tiered _ -> summary.units
           in
           Line.create
             ~code:component.code
             ~description:component.description
             ~kind:Usage
             ~period
             ~quantity
             amount))
  |> Or_error.all
;;
