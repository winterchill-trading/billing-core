open! Core

type t = Plan.t Plan_id.Map.t [@@deriving sexp_of]

let create plans =
  match Plan_id.Map.of_alist (List.map plans ~f:(fun (p : Plan.t) -> p.id, p)) with
  | `Ok t -> Ok t
  | `Duplicate_key id ->
    Or_error.error_s [%message "Billing_core.Price_list: duplicate plan" (id : Plan_id.t)]
;;

let plans t = Map.data t
let find t id = Map.find t id

let find_or_error t id =
  match find t id with
  | Some plan -> Ok plan
  | None ->
    Or_error.error_s [%message "Billing_core.Price_list: unknown plan" (id : Plan_id.t)]
;;

let in_currency t currency =
  List.filter (plans t) ~f:(fun (p : Plan.t) -> Money.Currency.equal p.currency currency)
;;
