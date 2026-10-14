open! Core

module Tier = struct
  module Repr = struct
    type t =
      { up_to : int option
      ; unit_price : Unit_price.Stable.V1.t
      ; flat : Money.Amount.Stable.V1.t
      }
    [@@deriving bin_io, sexp, compare, equal]
  end

  type t = Repr.t =
    { up_to : int option
    ; unit_price : Unit_price.t
    ; flat : Money.Amount.t
    }
  [@@deriving sexp_of, compare, equal]

  let create ?up_to ?flat unit_price =
    let currency = Unit_price.currency unit_price in
    let flat = Option.value flat ~default:(Money.Amount.zero currency) in
    if Option.exists up_to ~f:(fun n -> n < 1)
    then Or_error.error_s [%message "Billing_core.Price.Tier: up_to must be at least 1"]
    else if not (Money.Currency.equal (Money.Amount.currency flat) currency)
    then
      Or_error.error_s
        [%message "Billing_core.Price.Tier: flat part in another currency" (flat : Money.Amount.t)]
    else if Money.Amount.is_negative flat
    then
      Or_error.error_s
        [%message "Billing_core.Price.Tier: negative flat part" (flat : Money.Amount.t)]
    else Ok { up_to; unit_price; flat }
  ;;

  let validate ({ up_to; unit_price; flat } : Repr.t) = create ?up_to ~flat unit_price
end

module Tier_mode = struct
  type t =
    | Graduated
    | Volume
  [@@deriving bin_io, sexp, compare, equal, enumerate]
end

module Fee_basis = struct
  type t =
    | Per_event
    | Per_period
  [@@deriving bin_io, sexp, compare, equal, enumerate]
end

module Repr = struct
  type t =
    | Flat of Money.Amount.Stable.V1.t
    | Per_seat of
        { unit_price : Money.Amount.Stable.V1.t
        ; included : int
        }
    | Tiered of
        { meter : Meter.Stable.V1.t
        ; mode : Tier_mode.t
        ; tiers : Tier.Repr.t list
        }
    | Percentage of
        { meter : Meter.Stable.V1.t
        ; fee : Money.Fee.Stable.V2.t
        ; basis : Fee_basis.t
        }
  [@@deriving bin_io, sexp, compare, equal]
end

type t = Repr.t =
  | Flat of Money.Amount.t
  | Per_seat of
      { unit_price : Money.Amount.t
      ; included : int
      }
  | Tiered of
      { meter : Meter.t
      ; mode : Tier_mode.t
      ; tiers : Tier.t list
      }
  | Percentage of
      { meter : Meter.t
      ; fee : Money.Fee.t
      ; basis : Fee_basis.t
      }
[@@deriving compare, equal, sexp_of]

let non_negative what amount =
  if Money.Amount.is_negative amount
  then
    Or_error.error_s
      [%message "Billing_core.Price: negative amount" (what : string) (amount : Money.Amount.t)]
  else Ok ()
;;

let flat amount =
  let%map.Or_error () = non_negative "flat" amount in
  Flat amount
;;

let per_seat ?(included = 0) unit_price =
  let%bind.Or_error () = non_negative "per seat" unit_price in
  if included < 0
  then Or_error.error_s [%message "Billing_core.Price: negative included seats" (included : int)]
  else Ok (Per_seat { unit_price; included })
;;

let tiered meter mode tiers =
  match tiers with
  | [] -> Or_error.error_s [%message "Billing_core.Price: a tiered price needs a tier"]
  | (first : Tier.t) :: _ ->
    let currency = Unit_price.currency first.unit_price in
    let rec check previous = function
      | [] -> Ok ()
      | (tier : Tier.t) :: rest ->
        if not (Money.Currency.equal (Unit_price.currency tier.unit_price) currency)
        then Or_error.error_s [%message "Billing_core.Price: tiers in different currencies"]
        else (
          match tier.up_to, rest with
          | None, [] -> Ok ()
          | None, _ :: _ ->
            Or_error.error_s [%message "Billing_core.Price: only the last tier is open-ended"]
          | Some _, [] ->
            Or_error.error_s [%message "Billing_core.Price: the last tier must be open-ended"]
          | Some up_to, _ :: _ ->
            if up_to <= previous
            then
              Or_error.error_s
                [%message "Billing_core.Price: tier bounds must increase" (up_to : int)]
            else check up_to rest)
    in
    let%map.Or_error () = check 0 tiers in
    Tiered { meter; mode; tiers }
;;

let percentage ?(basis = Fee_basis.Per_period) meter fee = Percentage { meter; fee; basis }

let validate : Repr.t -> t Or_error.t = function
  | Flat amount -> flat amount
  | Per_seat { unit_price; included } -> per_seat ~included unit_price
  | Tiered { meter; mode; tiers } ->
    let%bind.Or_error tiers = List.map tiers ~f:Tier.validate |> Or_error.all in
    tiered meter mode tiers
  | Percentage { meter; fee; basis } -> Ok (percentage ~basis meter fee)
;;

let t_of_sexp sexp = Repr.t_of_sexp sexp |> validate |> Or_error.ok_exn

let currency = function
  | Flat amount -> Money.Amount.currency amount
  | Per_seat { unit_price; _ } -> Money.Amount.currency unit_price
  | Tiered { tiers; _ } ->
    (* [tiered] guarantees at least one tier. *)
    Unit_price.currency (List.hd_exn tiers).unit_price
  | Percentage { fee; _ } -> Money.Fee.currency fee
;;

let meter = function
  | Flat _ | Per_seat _ -> None
  | Tiered { meter; _ } | Percentage { meter; _ } -> Some meter
;;

let in_advance = function
  | Flat _ | Per_seat _ -> true
  | Tiered _ | Percentage _ -> false
;;

let recurring t ~seats =
  match t with
  | Flat amount -> amount
  | Per_seat { unit_price; included } ->
    Money.Amount.scale unit_price (Int.max 0 (seats - included))
  | Tiered _ | Percentage _ -> Money.Amount.zero (currency t)
;;

let tiered_amount mode (tiers : Tier.t list) ~quantity ~rounding =
  if quantity < 0
  then raise_s [%message "Billing_core.Price.tiered_amount: negative quantity" (quantity : int)];
  let currency = Unit_price.currency (List.hd_exn tiers).unit_price in
  let tier_q (tier : Tier.t) ~units =
    Q.add (Unit_price.extend_exact tier.unit_price ~quantity:units) (Money.Amount.to_q tier.flat)
  in
  let exact =
    if quantity = 0
    then Q.zero
    else (
      match mode with
      | Tier_mode.Volume ->
        let tier =
          List.find_exn tiers ~f:(fun (tier : Tier.t) ->
            match tier.up_to with
            | None -> true
            | Some up_to -> quantity <= up_to)
        in
        tier_q tier ~units:quantity
      | Graduated ->
        let _, total =
          List.fold tiers ~init:(0, Q.zero) ~f:(fun (below, total) (tier : Tier.t) ->
            let top = Option.value tier.up_to ~default:Int.max_value in
            let units = Int.max 0 (Int.min quantity top - below) in
            let total = if units > 0 then Q.add total (tier_q tier ~units) else total in
            top, total)
        in
        total)
  in
  Money.Amount.of_q ~rounding currency exact
;;

let usage t ~units ~volumes ~rounding =
  let currency = currency t in
  match t with
  | Flat _ | Per_seat _ -> Ok (Money.Amount.zero currency)
  | Tiered { mode; tiers; _ } -> Ok (tiered_amount mode tiers ~quantity:units ~rounding)
  | Percentage { fee; basis; _ } ->
    (match volumes with
     | [] -> Ok (Money.Amount.zero currency)
     | _ :: _ ->
       (match basis with
        | Per_event ->
          let%bind.Or_error fees =
            List.map volumes ~f:(Money.Fee.compute fee ~side:Receivable) |> Or_error.all
          in
          Money.Amount.sum currency fees
        | Per_period ->
          let%bind.Or_error total = Money.Amount.sum currency volumes in
          Money.Fee.compute fee ~side:Receivable total))
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
