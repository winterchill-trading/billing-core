(** Where billing state lives. Libraries never persist: a service implements this
    signature over its database, and {!Store_memory} serves tests, examples and offline
    modes. {!Store_conformance} checks an implementation.

    Writes are idempotent or compare-and-set, so several runners can bill at once:

    - [insert_*] creates a record under its id once; a second insert returns what is
      stored ([`Exists]), whatever its content.
    - [save_*] replaces a record only when the stored revision is exactly one below the
      new one (or, for a new dunning process, when none is stored and the revision is
      0); otherwise it returns the stored record ([`Stale]) and the caller reloads.
    - {!S.number} hands out document numbers per counter, gapless and idempotent per
      document: asking twice for the same document returns the same number. *)

open! Core

module type S = sig
  type 'a io
  type t

  (** {2 Subscriptions} *)

  val insert_subscription
    :  t
    -> Subscription.t
    -> [ `Inserted | `Exists of Subscription.t ] io

  val find_subscription : t -> Subscription_id.t -> Subscription.t option io
  val save_subscription : t -> Subscription.t -> [ `Saved | `Stale of Subscription.t ] io

  (** Every subscription that is not cancelled. *)
  val live_subscriptions : t -> Subscription.t list io

  (** {2 Usage} *)

  val insert_usage : t -> Usage.Record.t -> [ `Inserted | `Exists of Usage.Record.t ] io

  (** A subscription's records with [from <= occurred_at < until]. *)
  val usage
    :  t
    -> Subscription_id.t
    -> from:Time_ns.t
    -> until:Time_ns.t
    -> Usage.Record.t list io

  (** {2 Invoices} *)

  val insert_invoice : t -> Invoice.t -> [ `Inserted | `Exists of Invoice.t ] io
  val find_invoice : t -> Invoice_id.t -> Invoice.t option io
  val save_invoice : t -> Invoice.t -> [ `Saved | `Stale of Invoice.t ] io

  (** A customer's invoices, in id order. *)
  val invoices_of_customer : t -> Customer_id.t -> Invoice.t list io

  (** {2 Credit notes} *)

  val insert_credit_note
    :  t
    -> Credit_note.t
    -> [ `Inserted | `Exists of Credit_note.t ] io

  (** The credit notes against an invoice, in id order. *)
  val credit_notes_of_invoice : t -> Invoice_id.t -> Credit_note.t list io

  (** {2 Dunning} *)

  val find_dunning : t -> Invoice_id.t -> Dunning.t option io
  val save_dunning : t -> Dunning.t -> [ `Saved | `Stale of Dunning.t ] io

  (** Every dunning process whose state is not final. *)
  val open_dunning : t -> Dunning.t list io

  (** {2 Numbering} *)

  (** [number t ~counter ~document] is the number [document] holds under [counter], or
      the next one (1, 2, ...) if it has none yet. Must be atomic per counter. *)
  val number : t -> counter:string -> document:string -> int io
end
