open! Core

module Reset = struct
  type t =
    | Never
    | Yearly
  [@@deriving bin_io, sexp, compare, equal, enumerate]
end

module Repr = struct
  type t =
    { prefix : string
    ; width : int
    ; reset : Reset.t
    }
  [@@deriving bin_io, sexp, compare, equal]
end

type t = Repr.t =
  { prefix : string
  ; width : int
  ; reset : Reset.t
  }
[@@deriving compare, equal, sexp_of]

let create ?(width = 6) ?(reset = Reset.Yearly) prefix =
  let valid_char c = Char.is_uppercase c || Char.is_digit c || Char.equal c '-' in
  if String.length prefix < 1
     || String.length prefix > 16
     || not (String.for_all prefix ~f:valid_char)
  then
    Or_error.error_s
      [%message "Billing_core.Numbering: prefix must be 1-16 of A-Z 0-9 -" (prefix : string)]
  else if width < 1 || width > 12
  then Or_error.error_s [%message "Billing_core.Numbering: width must be 1..12" (width : int)]
  else Ok { prefix; width; reset }
;;

let validate ({ prefix; width; reset } : Repr.t) = create ~width ~reset prefix
let t_of_sexp sexp = Repr.t_of_sexp sexp |> validate |> Or_error.ok_exn

let counter t ~issued_on =
  match t.reset with
  | Never -> t.prefix
  | Yearly -> sprintf "%s-%04d" t.prefix (Date.year issued_on)
;;

let format t ~issued_on ~seq =
  if seq < 1 then raise_s [%message "Billing_core.Numbering.format: seq must be >= 1" (seq : int)];
  sprintf "%s-%0*d" (counter t ~issued_on) t.width seq
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
