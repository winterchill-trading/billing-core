open! Core

module Repr = struct
  type t =
    | Months of int
    | Days of int
  [@@deriving bin_io, sexp, compare, equal, hash]
end

type t = Repr.t =
  | Months of int
  | Days of int
[@@deriving compare, equal, hash, sexp_of]

let months n =
  if n >= 1 && n <= 120
  then Ok (Months n)
  else
    Or_error.error_s [%message "Billing_core.Interval: months must be 1..120" (n : int)]
;;

let days n =
  if n >= 1 && n <= 3660
  then Ok (Days n)
  else Or_error.error_s [%message "Billing_core.Interval: days must be 1..3660" (n : int)]
;;

let validate = function
  | Repr.Months n -> months n
  | Days n -> days n
;;

let t_of_sexp sexp = Repr.t_of_sexp sexp |> validate |> Or_error.ok_exn
let monthly = Months 1
let quarterly = Months 3
let yearly = Months 12

let boundary t ~anchor ~index =
  match t with
  | Months n -> Date.add_months anchor (n * index)
  | Days n -> Date.add_days anchor (n * index)
;;

let period t ~anchor ~index =
  if index < 0
  then raise_s [%message "Billing_core.Interval.period: negative index" (index : int)];
  Period.create_exn
    ~start:(boundary t ~anchor ~index)
    ~stop:(boundary t ~anchor ~index:(index + 1))
;;

let index_of t ~anchor date =
  if Date.( < ) date anchor
  then None
  else (
    (* A first guess from the calendar, then a walk: month lengths differ, so the guess
       can be one period off in either direction. *)
    let guess =
      match t with
      | Days n -> Date.diff date anchor / n
      | Months n ->
        let months =
          ((Date.year date - Date.year anchor) * 12)
          + (Month.to_int (Date.month date) - Month.to_int (Date.month anchor))
        in
        Int.max 0 (months / n)
    in
    let rec walk index =
      let p = period t ~anchor ~index in
      if Period.contains p date
      then index
      else if Date.( < ) date p.start
      then walk (index - 1)
      else walk (index + 1)
    in
    Some (walk guess))
;;

let to_string = function
  | Months n -> sprintf "P%dM" n
  | Days n -> sprintf "P%dD" n
;;

module Stable = struct
  module V1 = struct
    type nonrec t = t [@@deriving compare, equal, hash, sexp]

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
