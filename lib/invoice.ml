open! Core

module Status = struct
  type t =
    | Draft
    | Open
    | Paid
    | Void
    | Uncollectible
  [@@deriving bin_io, sexp, compare, equal, enumerate]

  let to_string t = Sexp.to_string (sexp_of_t t) |> String.lowercase
end

module Item = struct
  type t =
    { line : Line.Stable.V1.t
    ; rates : Tax.Rate.Stable.V1.t list
    }
  [@@deriving bin_io, sexp, compare, equal]
end

module Tax_line = struct
  type t =
    { rate : Tax.Rate.Stable.V1.t
    ; taxable : Money.Amount.Stable.V1.t
    ; amount : Money.Amount.Stable.V1.t
    }
  [@@deriving bin_io, sexp, compare, equal]

  module Stable = struct
    module V1 = struct
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
    end
  end
end

module Payment = struct
  type t =
    { id : Payment_id.Stable.V1.t
    ; amount : Money.Amount.Stable.V1.t
    ; paid_at : Time_ns.Stable.Alternate_sexp.V1.t
    }
  [@@deriving bin_io, sexp, compare, equal]
end

module Credit = struct
  type t =
    { credit_note : Credit_note_id.Stable.V1.t
    ; amount : Money.Amount.Stable.V1.t
    }
  [@@deriving bin_io, sexp, compare, equal]
end

module Repr = struct
  type t =
    { id : Invoice_id.Stable.V1.t
    ; customer : Customer_id.Stable.V1.t
    ; subscription : Subscription_id.Stable.V1.t option
    ; period : Period.Stable.V1.t option
    ; currency : Money.Currency.Stable.V1.t
    ; rounding : Rounding_rules.Stable.V1.t
    ; items : Item.t list
    ; subtotal : Money.Amount.Stable.V1.t
    ; taxes : Tax_line.t list
    ; tax_total : Money.Amount.Stable.V1.t
    ; total : Money.Amount.Stable.V1.t
    ; status : Status.t
    ; number : string option
    ; issued_on : Date.Stable.V1.t option
    ; due_on : Date.Stable.V1.t option
    ; payments : Payment.t list
    ; credits : Credit.t list
    ; written_off_after : int option
    ; revision : int
    }
  [@@deriving bin_io, sexp, compare, equal]
end

type t = Repr.t =
  { id : Invoice_id.t
  ; customer : Customer_id.t
  ; subscription : Subscription_id.t option
  ; period : Period.t option
  ; currency : Money.Currency.t
  ; rounding : Rounding_rules.t
  ; items : Item.t list
  ; subtotal : Money.Amount.t
  ; taxes : Tax_line.t list
  ; tax_total : Money.Amount.t
  ; total : Money.Amount.t
  ; status : Status.t
  ; number : string option
  ; issued_on : Date.t option
  ; due_on : Date.t option
  ; payments : Payment.t list
  ; credits : Credit.t list
  ; written_off_after : int option
  ; revision : int
  }
[@@deriving compare, equal, sexp_of]

let sum currency amounts = Money.Amount.sum currency amounts |> Or_error.ok_exn

module Totals = struct
  type t =
    { subtotal : Money.Amount.t
    ; taxes : Tax_line.t list
    ; tax_total : Money.Amount.t
    ; total : Money.Amount.t
    }
  [@@deriving sexp_of]

  (* Totals derive from the items alone, so [draft], [add_lines] and decoding share one
     computation and cannot disagree. *)
  let compute ~currency ~(rounding : Rounding_rules.t) (items : Item.t list) =
    let subtotal = sum currency (List.map items ~f:(fun item -> item.line.amount)) in
    let rates =
      (* Each rate once, in the order of its first use, so tax lines print in a stable
         order. *)
      List.fold
        (List.concat_map items ~f:(fun item -> item.rates))
        ~init:[]
        ~f:(fun acc rate -> if List.mem acc rate ~equal:Tax.Rate.equal then acc else rate :: acc)
      |> List.rev
    in
    let taxes =
      List.map rates ~f:(fun rate ->
        let lines =
          List.filter_map items ~f:(fun item ->
            Option.some_if (List.mem item.rates rate ~equal:Tax.Rate.equal) item.line.amount)
        in
        let taxable = sum currency lines in
        let amount =
          match rounding.tax_basis with
          | Per_rate -> Money.Amount.mul_bps ~rounding:rounding.tax taxable ~bps:rate.bps
          | Per_line ->
            sum
              currency
              (List.map lines ~f:(fun line ->
                 Money.Amount.mul_bps ~rounding:rounding.tax line ~bps:rate.bps))
        in
        { Tax_line.rate; taxable; amount })
    in
    let tax_total = sum currency (List.map taxes ~f:(fun tax -> tax.amount)) in
    { subtotal; taxes; tax_total; total = Money.Amount.add_exn subtotal tax_total }
  ;;
end

let totals ~currency ~rounding items =
  let { Totals.subtotal; taxes; tax_total; total } = Totals.compute ~currency ~rounding items in
  subtotal, taxes, tax_total, total
;;

let items_of ~tax ~customer lines =
  List.map lines ~f:(fun line -> { Item.line; rates = Tax.rates tax ~customer line })
;;

let check_currency currency lines =
  match
    List.find lines ~f:(fun line ->
      not (Money.Currency.equal (Line.currency line) currency))
  with
  | None -> Ok ()
  | Some line ->
    Or_error.error_s
      [%message
        "Billing_core.Invoice: line in another currency"
          (currency : Money.Currency.t)
          (line : Line.t)]
;;

let with_items t items =
  let subtotal, taxes, tax_total, total =
    totals ~currency:t.currency ~rounding:t.rounding items
  in
  { t with items; subtotal; taxes; tax_total; total; revision = t.revision + 1 }
;;

let draft ~id ~customer ?subscription ?period ~currency ~tax ~rounding lines =
  let%map.Or_error () = check_currency currency lines in
  let items = items_of ~tax ~customer lines in
  let subtotal, taxes, tax_total, total = totals ~currency ~rounding items in
  { id
  ; customer
  ; subscription
  ; period
  ; currency
  ; rounding
  ; items
  ; subtotal
  ; taxes
  ; tax_total
  ; total
  ; status = Draft
  ; number = None
  ; issued_on = None
  ; due_on = None
  ; payments = []
  ; credits = []
  ; written_off_after = None
  ; revision = 0
  }
;;

let refuse t ~action =
  Refusal.invalid_state
    ~what:"invoice"
    ~id:(Invoice_id.to_string t.id)
    ~state:(Status.to_string t.status)
    ~action
;;

let add_lines t ~tax lines =
  match t.status with
  | Open | Paid | Void | Uncollectible -> refuse t ~action:"add_lines"
  | Draft ->
    let%map.Result () = check_currency t.currency lines |> Refusal.of_or_error in
    with_items t (t.items @ items_of ~tax ~customer:t.customer lines)
;;

let amount_paid t = sum t.currency (List.map t.payments ~f:(fun p -> p.amount))
let amount_credited t = sum t.currency (List.map t.credits ~f:(fun c -> c.amount))

let outstanding t =
  Money.Amount.sub_exn (Money.Amount.sub_exn t.total (amount_paid t)) (amount_credited t)
;;

let amount_due t =
  match t.status with
  | Draft | Void -> Money.Amount.zero t.currency
  | Open | Paid | Uncollectible ->
    let left = outstanding t in
    if Money.Amount.is_negative left then Money.Amount.zero t.currency else left
;;

let overpaid t =
  let left = outstanding t in
  if Money.Amount.is_negative left then Money.Amount.neg left else Money.Amount.zero t.currency
;;

let written_off t =
  Option.map t.written_off_after ~f:(fun n ->
    let before = sum t.currency (List.map (List.take t.payments n) ~f:(fun p -> p.amount)) in
    Money.Amount.sub_exn (Money.Amount.sub_exn t.total before) (amount_credited t))
;;

let is_recovery t payment_id =
  match t.written_off_after with
  | None -> false
  | Some n ->
    List.drop t.payments n
    |> List.exists ~f:(fun (p : Payment.t) -> Payment_id.equal p.id payment_id)
;;

let is_overdue t ~today =
  match t.status, t.due_on with
  | (Open | Uncollectible), Some due_on ->
    Date.( > ) today due_on && Money.Amount.is_positive (amount_due t)
  | _ -> false
;;

let finalize t ~number ~issued_on ~due_on =
  match t.status with
  | Open | Paid | Void | Uncollectible -> refuse t ~action:"finalize"
  | Draft ->
    if Money.Amount.is_negative t.total
    then Error (Refusal.Negative_total t.total)
    else if Date.( < ) due_on issued_on
    then
      Error
        (Refusal.Invalid
           (Error.create_s
              [%message
                "Billing_core.Invoice: due before issue" (issued_on : Date.t) (due_on : Date.t)]))
    else if String.is_empty (String.strip number)
    then Error (Refusal.Invalid (Error.of_string "Billing_core.Invoice: empty number"))
    else (
      let status : Status.t = if Money.Amount.is_zero t.total then Paid else Open in
      Ok
        { t with
          status
        ; number = Some number
        ; issued_on = Some issued_on
        ; due_on = Some due_on
        ; revision = t.revision + 1
        })
;;

let check_amount t amount =
  let got = Money.Amount.currency amount in
  if not (Money.Currency.equal got t.currency)
  then Error (Refusal.Currency_mismatch { expected = t.currency; got })
  else if not (Money.Amount.is_positive amount)
  then
    Error
      (Refusal.Invalid
         (Error.create_s
            [%message "Billing_core.Invoice: amount must be positive" (amount : Money.Amount.t)]))
  else Ok ()
;;

let settle_if_covered t =
  match t.status with
  | (Open | Uncollectible) when Money.Amount.is_zero (amount_due t) -> { t with status = Paid }
  | _ -> t
;;

let pay t (payment : Payment.t) =
  match List.find t.payments ~f:(fun p -> Payment_id.equal p.id payment.id) with
  | Some existing ->
    if Money.Amount.equal existing.amount payment.amount
    then Ok (t, `Duplicate)
    else Error (Refusal.Payment_conflict payment.id)
  | None ->
    (match t.status with
     | Draft | Paid | Void -> refuse t ~action:"pay"
     | Open | Uncollectible ->
       let%bind.Result () = check_amount t payment.amount in
       let due = amount_due t in
       if Money.Amount.( > ) payment.amount due
       then Error (Refusal.Overpayment { due; offered = payment.amount })
       else
         Ok
           ( settle_if_covered
               { t with payments = t.payments @ [ payment ]; revision = t.revision + 1 }
           , `Applied ))
;;

let credit t ~credit_note ~amount =
  match
    List.find t.credits ~f:(fun c -> Credit_note_id.equal c.credit_note credit_note)
  with
  | Some existing ->
    if Money.Amount.equal existing.amount amount
    then Ok (t, `Duplicate)
    else
      Error
        (Refusal.Invalid
           (Error.create_s
              [%message
                "Billing_core.Invoice: credit note already applied with another amount"
                  (credit_note : Credit_note_id.t)]))
  | None ->
    (match t.status with
     | Draft | Void | Uncollectible -> refuse t ~action:"credit"
     | Open | Paid ->
       let%bind.Result () = check_amount t amount in
       let creditable = Money.Amount.sub_exn t.total (amount_credited t) in
       if Money.Amount.( > ) amount creditable
       then Error (Refusal.Over_credit { creditable; requested = amount })
       else
         Ok
           ( settle_if_covered
               { t with
                 credits = t.credits @ [ { credit_note; amount } ]
               ; revision = t.revision + 1
               }
           , `Applied ))
;;

let void t =
  match t.status with
  | Draft -> Ok { t with status = Void; revision = t.revision + 1 }
  | Open when List.is_empty t.payments && List.is_empty t.credits ->
    Ok { t with status = Void; revision = t.revision + 1 }
  | Open | Paid | Void | Uncollectible -> refuse t ~action:"void"
;;

let mark_uncollectible t =
  match t.status with
  | Open ->
    Ok
      { t with
        status = Uncollectible
      ; written_off_after = Some (List.length t.payments)
      ; revision = t.revision + 1
      }
  | Draft | Paid | Void | Uncollectible -> refuse t ~action:"mark_uncollectible"
;;

let validate (t : t) =
  let%bind.Or_error () = check_currency t.currency (List.map t.items ~f:(fun i -> i.line)) in
  let subtotal, taxes, tax_total, total =
    totals ~currency:t.currency ~rounding:t.rounding t.items
  in
  if Money.Amount.equal subtotal t.subtotal
     && List.equal Tax_line.equal taxes t.taxes
     && Money.Amount.equal tax_total t.tax_total
     && Money.Amount.equal total t.total
  then Ok t
  else
    Or_error.error_s
      [%message "Billing_core.Invoice: totals disagree with the lines" (t.id : Invoice_id.t)]
;;

let t_of_sexp sexp = Repr.t_of_sexp sexp |> validate |> Or_error.ok_exn

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
