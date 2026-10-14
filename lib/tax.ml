open! Core

module Rate = struct
  module Repr = struct
    type t =
      { name : string
      ; bps : int
      }
    [@@deriving bin_io, sexp, compare, equal, hash]
  end

  type t = Repr.t =
    { name : string
    ; bps : int
    }
  [@@deriving compare, equal, hash, sexp_of]

  let create ~name ~bps =
    match Ledger.Validated_id.check name with
    | Error err -> Error (Error.tag err ~tag:"Billing_core.Tax.Rate: bad name")
    | Ok () ->
      if bps < 0 || bps > 10_000
      then
        Or_error.error_s
          [%message "Billing_core.Tax.Rate: bps must be 0..10000" (bps : int)]
      else Ok { name; bps }
  ;;

  let validate ({ name; bps } : Repr.t) = create ~name ~bps
  let t_of_sexp sexp = Repr.t_of_sexp sexp |> validate |> Or_error.ok_exn

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
end

type t =
  { name : string
  ; rates_for : customer:Customer_id.t -> Line.t -> Rate.t list
  }

let create ~name rates_for = { name; rates_for }
let none = create ~name:"none" (fun ~customer:_ _ -> [])
let flat rate = create ~name:rate.Rate.name (fun ~customer:_ _ -> [ rate ])
let rates t ~customer line = t.rates_for ~customer line
let name t = t.name
