open! Core

module Value = struct
  type t =
    | Units of int
    | Volume of Money.Amount.Stable.V1.t
  [@@deriving bin_io, sexp, compare, equal]
end

module Record = struct
  module Repr = struct
    type t =
      { id : Usage_id.Stable.V1.t
      ; subscription : Subscription_id.Stable.V1.t
      ; meter : Meter.Stable.V1.t
      ; value : Value.t
      ; occurred_at : Time_ns.Stable.Alternate_sexp.V1.t
      }
    [@@deriving bin_io, sexp, compare, equal]
  end

  type t = Repr.t =
    { id : Usage_id.t
    ; subscription : Subscription_id.t
    ; meter : Meter.t
    ; value : Value.t
    ; occurred_at : Time_ns.Alternate_sexp.t
    }
  [@@deriving compare, equal, sexp_of]

  let create ~id ~subscription ~meter ~occurred_at value =
    match (value : Value.t) with
    | Units n when n < 0 ->
      Or_error.error_s
        [%message "Billing_core.Usage: negative units" (id : Usage_id.t) (n : int)]
    | Units _ | Volume _ -> Ok { id; subscription; meter; value; occurred_at }
  ;;

  let validate (r : Repr.t) =
    create
      ~id:r.id
      ~subscription:r.subscription
      ~meter:r.meter
      ~occurred_at:r.occurred_at
      r.value
  ;;

  let t_of_sexp sexp = Repr.t_of_sexp sexp |> validate |> Or_error.ok_exn
  let date t ~zone = Time_ns.to_date t.occurred_at ~zone

  module Stable = struct
    module V1 = struct
      type nonrec t = t [@@deriving compare, equal, sexp]

      include
        Binable.Of_binable_without_uuid [@alert "-legacy"]
          (Repr)
          (struct
            type nonrec t = t

            let to_binable t = t
            let of_binable r = validate r |> Or_error.ok_exn
          end)
    end
  end
end

module Outcome = struct
  type t =
    | Accepted
    | Duplicate
  [@@deriving sexp, compare, equal]
end

let check ~existing (record : Record.t) =
  match existing with
  | None -> Ok Outcome.Accepted
  | Some existing ->
    if Record.equal existing record
    then Ok Duplicate
    else Error (Refusal.Usage_conflict record.id)
;;

let chronological (a : Record.t) (b : Record.t) =
  match Time_ns.compare a.occurred_at b.occurred_at with
  | 0 -> Usage_id.compare a.id b.id
  | c -> c
;;

module Summary = struct
  type t =
    { units : int
    ; volumes : Money.Amount.t list
    ; records : int
    }
  [@@deriving sexp_of]

  let of_records records ~meter ~zone period =
    let mine =
      List.filter records ~f:(fun (r : Record.t) ->
        Meter.equal r.meter meter && Period.contains period (Record.date r ~zone))
      |> List.sort ~compare:chronological
    in
    List.fold mine ~init:{ units = 0; volumes = []; records = 0 } ~f:(fun acc r ->
      let acc = { acc with records = acc.records + 1 } in
      match r.value with
      | Units n -> { acc with units = acc.units + n }
      | Volume v -> { acc with volumes = v :: acc.volumes })
    |> fun t -> { t with volumes = List.rev t.volumes }
  ;;
end

module Log = struct
  type t = Record.t Usage_id.Map.t [@@deriving sexp_of]

  let empty = Usage_id.Map.empty

  let ingest t (record : Record.t) =
    let%map.Result outcome = check ~existing:(Map.find t record.id) record in
    match outcome with
    | Accepted -> Map.set t ~key:record.id ~data:record, outcome
    | Duplicate -> t, outcome
  ;;

  let find t id = Map.find t id
  let records t = Map.data t |> List.sort ~compare:chronological
end
