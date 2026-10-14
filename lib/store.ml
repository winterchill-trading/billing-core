open! Core

module type S = sig
  type 'a io
  type t

  val insert_subscription
    :  t
    -> Subscription.t
    -> [ `Inserted | `Exists of Subscription.t ] io

  val find_subscription : t -> Subscription_id.t -> Subscription.t option io
  val save_subscription : t -> Subscription.t -> [ `Saved | `Stale of Subscription.t ] io
  val live_subscriptions : t -> Subscription.t list io
  val insert_usage : t -> Usage.Record.t -> [ `Inserted | `Exists of Usage.Record.t ] io

  val usage
    :  t
    -> Subscription_id.t
    -> from:Time_ns.t
    -> until:Time_ns.t
    -> Usage.Record.t list io

  val insert_invoice : t -> Invoice.t -> [ `Inserted | `Exists of Invoice.t ] io
  val find_invoice : t -> Invoice_id.t -> Invoice.t option io
  val save_invoice : t -> Invoice.t -> [ `Saved | `Stale of Invoice.t ] io
  val invoices_of_customer : t -> Customer_id.t -> Invoice.t list io

  val insert_credit_note
    :  t
    -> Credit_note.t
    -> [ `Inserted | `Exists of Credit_note.t ] io

  val credit_notes_of_invoice : t -> Invoice_id.t -> Credit_note.t list io
  val find_dunning : t -> Invoice_id.t -> Dunning.t option io
  val save_dunning : t -> Dunning.t -> [ `Saved | `Stale of Dunning.t ] io
  val open_dunning : t -> Dunning.t list io
  val number : t -> counter:string -> document:string -> int io
end
