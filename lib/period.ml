open! Core

module Repr = struct
  type t =
    { start : Date.Stable.V1.t
    ; stop : Date.Stable.V1.t
    }
  [@@deriving bin_io, sexp, compare, equal, hash]
end

type t = Repr.t =
  { start : Date.t
  ; stop : Date.t
  }
[@@deriving compare, equal, hash, sexp_of]

let create ~start ~stop =
  if Date.( < ) start stop
  then Ok { start; stop }
  else
    Or_error.error_s
      [%message
        "Billing_core.Period: start must be before stop" (start : Date.t) (stop : Date.t)]
;;

let create_exn ~start ~stop = create ~start ~stop |> Or_error.ok_exn
let t_of_sexp sexp = Repr.t_of_sexp sexp |> fun { start; stop } -> create_exn ~start ~stop
let days t = Date.diff t.stop t.start
let contains t date = Date.( >= ) date t.start && Date.( < ) date t.stop
let last_day t = Date.add_days t.stop (-1)
let to_string t = Date.to_string t.start ^ ".." ^ Date.to_string (last_day t)

module Stable = struct
  module V1 = struct
    type nonrec t = t [@@deriving compare, equal, hash, sexp]

    include
      Binable.Of_binable_without_uuid [@alert "-legacy"]
        (Repr)
        (struct
          type nonrec t = t

          let to_binable t = t
          let of_binable ({ start; stop } : Repr.t) = create_exn ~start ~stop
        end)
  end
end
