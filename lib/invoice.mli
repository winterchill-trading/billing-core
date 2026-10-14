(** An invoice and its life.

    {v
      Draft --finalize--> Open --payments and credits cover the total--> Paid
        |                  |--mark_uncollectible--> Uncollectible --pay--> Paid
        +--void--> Void    +--void (nothing paid or credited)--> Void
    v}

    A draft holds lines and recomputes its totals whenever lines are added. Finalising
    assigns the number, the issue and due dates and freezes the lines; an invoice whose
    total is zero is [Paid] at once. Totals always satisfy
    [subtotal = sum of line amounts], [tax_total = sum of tax lines] and
    [total = subtotal + tax_total]; the amount due is
    [total - paid - credited], never below zero.

    Payments are idempotent by payment id: the same id with the same amount is a
    duplicate that changes nothing, with another amount a {!Refusal.Payment_conflict}.
    Every transition increments [revision], which a store uses to refuse a stale write. *)

open! Core

module Status : sig
  type t =
    | Draft
    | Open
    | Paid
    | Void
    | Uncollectible
  [@@deriving sexp, compare, equal, enumerate]

  (** Lower-case name, e.g. ["open"]. *)
  val to_string : t -> string
end

(** A line with the tax rates that apply to it, fixed when the line was added. *)
module Item : sig
  type t =
    { line : Line.t
    ; rates : Tax.Rate.t list
    }
  [@@deriving sexp, compare, equal]
end

(** The tax charged at one rate. *)
module Tax_line : sig
  type t =
    { rate : Tax.Rate.t
    ; taxable : Money.Amount.t (** the sum of the line amounts the rate applies to *)
    ; amount : Money.Amount.t
    }
  [@@deriving sexp, compare, equal]
end

module Payment : sig
  type t =
    { id : Payment_id.t
    ; amount : Money.Amount.t (** positive *)
    ; paid_at : Time_ns.Alternate_sexp.t
    }
  [@@deriving sexp, compare, equal]
end

(** A credit note applied to the invoice. *)
module Credit : sig
  type t =
    { credit_note : Credit_note_id.t
    ; amount : Money.Amount.t (** the credit note's total, tax included; positive *)
    }
  [@@deriving sexp, compare, equal]
end

type t = private
  { id : Invoice_id.t
  ; customer : Customer_id.t
  ; subscription : Subscription_id.t option
  ; period : Period.t option (** the billing period the invoice was raised for *)
  ; currency : Money.Currency.t
  ; rounding : Rounding_rules.t
  ; items : Item.t list
  ; subtotal : Money.Amount.t
  ; taxes : Tax_line.t list (** in order of each rate's first use *)
  ; tax_total : Money.Amount.t
  ; total : Money.Amount.t
  ; status : Status.t
  ; number : string option (** set by {!finalize} *)
  ; issued_on : Date.t option
  ; due_on : Date.t option
  ; payments : Payment.t list (** oldest first *)
  ; credits : Credit.t list (** oldest first *)
  ; revision : int
  }
[@@deriving sexp, compare, equal]

(** A draft with these lines. Fails if a line is in another currency. *)
val draft
  :  id:Invoice_id.t
  -> customer:Customer_id.t
  -> ?subscription:Subscription_id.t
  -> ?period:Period.t
  -> currency:Money.Currency.t
  -> tax:Tax.t
  -> rounding:Rounding_rules.t
  -> Line.t list
  -> t Or_error.t

(** Adds lines to a draft and recomputes the totals. *)
val add_lines : t -> tax:Tax.t -> Line.t list -> (t, Refusal.t) Result.t

(** Issues a draft: [Open], or [Paid] when the total is zero. Refuses a negative total
    and a due date before the issue date. *)
val finalize
  :  t
  -> number:string
  -> issued_on:Date.t
  -> due_on:Date.t
  -> (t, Refusal.t) Result.t

(** The sum of the payments. *)
val amount_paid : t -> Money.Amount.t

(** The sum of the credits. *)
val amount_credited : t -> Money.Amount.t

(** [total - paid - credited], at least zero. Zero for drafts and void invoices. *)
val amount_due : t -> Money.Amount.t

(** [paid + credited - total] when positive: money owed back to the customer. *)
val overpaid : t -> Money.Amount.t

(** [true] when open or uncollectible, money is still due and [today] is after the due
    date. *)
val is_overdue : t -> today:Date.t -> bool

(** Records a payment against an [Open] or [Uncollectible] invoice. Refuses an amount
    above the amount due, a non-positive amount and another currency. [`Duplicate] when
    the payment id is already recorded with the same amount. *)
val pay : t -> Payment.t -> (t * [ `Applied | `Duplicate ], Refusal.t) Result.t

(** Applies a credit note's total to an [Open] or [Paid] invoice. The credits together
    may not exceed the total. An open invoice whose amount due reaches zero becomes
    [Paid]. [`Duplicate] when the credit note is already applied with the same amount. *)
val credit
  :  t
  -> credit_note:Credit_note_id.t
  -> amount:Money.Amount.t
  -> (t * [ `Applied | `Duplicate ], Refusal.t) Result.t

(** Cancels a draft, or an open invoice with no payment and no credit. *)
val void : t -> (t, Refusal.t) Result.t

(** Gives up collecting an open invoice. The amount due at that moment is written off;
    a later payment still settles it. *)
val mark_uncollectible : t -> (t, Refusal.t) Result.t

module Stable : sig
  module V1 : sig
    (** Decoding recomputes and checks the totals, so a damaged record cannot carry a
        total that disagrees with its lines. *)
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
