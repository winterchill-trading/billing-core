open! Core

module Component = struct
  module Repr = struct
    type t =
      { code : string
      ; description : string
      ; price : Price.Stable.V1.t
      }
    [@@deriving bin_io, sexp, compare, equal]
  end

  type t = Repr.t =
    { code : string
    ; description : string
    ; price : Price.t
    }
  [@@deriving compare, equal, sexp_of]

  let create ~code ~description price =
    match Ledger.Validated_id.check code with
    | Error err -> Error (Error.tag err ~tag:"Billing_core.Plan.Component: bad code")
    | Ok () ->
      if String.is_empty (String.strip description)
      then Or_error.error_s [%message "Billing_core.Plan.Component: empty description"]
      else Ok { code; description; price }
  ;;

  let validate ({ code; description; price } : Repr.t) = create ~code ~description price
  let t_of_sexp sexp = Repr.t_of_sexp sexp |> validate |> Or_error.ok_exn
end

module Repr = struct
  type t =
    { id : Plan_id.Stable.V1.t
    ; name : string
    ; currency : Money.Currency.Stable.V1.t
    ; interval : Interval.Stable.V1.t
    ; components : Component.Repr.t list
    ; trial_days : int
    ; features : string list
    }
  [@@deriving bin_io, sexp, compare, equal]
end

type t = Repr.t =
  { id : Plan_id.t
  ; name : string
  ; currency : Money.Currency.t
  ; interval : Interval.t
  ; components : Component.t list
  ; trial_days : int
  ; features : string list
  }
[@@deriving compare, equal, sexp_of]

let create ~id ~name ~currency ~interval ?(trial_days = 0) ?(features = []) components =
  let error message = Or_error.error_s [%message message (id : Plan_id.t)] in
  if List.is_empty components
  then error "Billing_core.Plan: a plan needs a component"
  else if String.is_empty (String.strip name)
  then error "Billing_core.Plan: empty name"
  else if trial_days < 0 || trial_days > 365
  then error "Billing_core.Plan: trial_days must be 0..365"
  else if
    List.exists components ~f:(fun (c : Component.t) ->
      not (Money.Currency.equal (Price.currency c.price) currency))
  then error "Billing_core.Plan: a component is priced in another currency"
  else if
    List.contains_dup components ~compare:(fun (a : Component.t) (b : Component.t) ->
      String.compare a.code b.code)
  then error "Billing_core.Plan: duplicate component code"
  else Ok { id; name; currency; interval; components; trial_days; features }
;;

let flat ~id ~name ~interval ?trial_days ?features amount =
  let%bind.Or_error price = Price.flat amount in
  let%bind.Or_error component = Component.create ~code:"plan" ~description:name price in
  create
    ~id
    ~name
    ~currency:(Money.Amount.currency amount)
    ~interval
    ?trial_days
    ?features
    [ component ]
;;

let validate (r : Repr.t) =
  let%bind.Or_error components = List.map r.components ~f:Component.validate |> Or_error.all in
  create
    ~id:r.id
    ~name:r.name
    ~currency:r.currency
    ~interval:r.interval
    ~trial_days:r.trial_days
    ~features:r.features
    components
;;

let t_of_sexp sexp = Repr.t_of_sexp sexp |> validate |> Or_error.ok_exn

let recurring_total t ~seats =
  List.fold t.components ~init:(Money.Amount.zero t.currency) ~f:(fun total c ->
    Money.Amount.add_exn total (Price.recurring c.price ~seats))
;;

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
