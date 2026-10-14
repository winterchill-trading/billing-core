open! Core

module Make (M : Monad.S) = struct
  type 'a io = 'a M.t

  type t =
    { subscriptions : Subscription.t Subscription_id.Table.t
    ; usage : Usage.Record.t Usage_id.Table.t
    ; invoices : Invoice.t Invoice_id.Table.t
    ; credit_notes : Credit_note.t Credit_note_id.Table.t
    ; dunning : Dunning.t Invoice_id.Table.t
    ; counters : int String.Table.t
    ; numbers : int String.Table.t (** keyed by counter and document, NUL-separated *)
    }

  let create () =
    { subscriptions = Subscription_id.Table.create ()
    ; usage = Usage_id.Table.create ()
    ; invoices = Invoice_id.Table.create ()
    ; credit_notes = Credit_note_id.Table.create ()
    ; dunning = Invoice_id.Table.create ()
    ; counters = String.Table.create ()
    ; numbers = String.Table.create ()
    }
  ;;

  let insert table key value =
    match Hashtbl.find table key with
    | Some existing -> M.return (`Exists existing)
    | None ->
      Hashtbl.set table ~key ~data:value;
      M.return `Inserted
  ;;

  (* Compare-and-set on the revision: the stored record must be the one the new revision
     was derived from. *)
  let save table key value ~revision ~stored_revision =
    match Hashtbl.find table key with
    | Some existing when stored_revision existing = revision value - 1 ->
      Hashtbl.set table ~key ~data:value;
      M.return `Saved
    | None when revision value = 0 ->
      Hashtbl.set table ~key ~data:value;
      M.return `Saved
    | Some existing -> M.return (`Stale existing)
    | None ->
      (* Nothing to be stale against: a save of a record never inserted is a caller bug. *)
      raise_s [%message "Billing_core.Store_memory: save of an unknown record"]
  ;;

  let insert_subscription t (s : Subscription.t) = insert t.subscriptions s.id s
  let find_subscription t id = M.return (Hashtbl.find t.subscriptions id)

  let save_subscription t (s : Subscription.t) =
    save
      t.subscriptions
      s.id
      s
      ~revision:(fun (s : Subscription.t) -> s.revision)
      ~stored_revision:(fun (s : Subscription.t) -> s.revision)
  ;;

  let live_subscriptions t =
    Hashtbl.data t.subscriptions
    |> List.filter ~f:(fun (s : Subscription.t) ->
      not (Subscription.Status.equal s.status Cancelled))
    |> List.sort ~compare:(fun (a : Subscription.t) (b : Subscription.t) ->
      Subscription_id.compare a.id b.id)
    |> M.return
  ;;

  let insert_usage t (r : Usage.Record.t) = insert t.usage r.id r

  let usage t subscription ~from ~until =
    Hashtbl.data t.usage
    |> List.filter ~f:(fun (r : Usage.Record.t) ->
      Subscription_id.equal r.subscription subscription
      && Time_ns.( >= ) r.occurred_at from
      && Time_ns.( < ) r.occurred_at until)
    |> List.sort ~compare:(fun (a : Usage.Record.t) (b : Usage.Record.t) ->
      match Time_ns.compare a.occurred_at b.occurred_at with
      | 0 -> Usage_id.compare a.id b.id
      | c -> c)
    |> M.return
  ;;

  let insert_invoice t (i : Invoice.t) = insert t.invoices i.id i
  let find_invoice t id = M.return (Hashtbl.find t.invoices id)

  let save_invoice t (i : Invoice.t) =
    save
      t.invoices
      i.id
      i
      ~revision:(fun (i : Invoice.t) -> i.revision)
      ~stored_revision:(fun (i : Invoice.t) -> i.revision)
  ;;

  let invoices_of_customer t customer =
    Hashtbl.data t.invoices
    |> List.filter ~f:(fun (i : Invoice.t) -> Customer_id.equal i.customer customer)
    |> List.sort ~compare:(fun (a : Invoice.t) (b : Invoice.t) ->
      Invoice_id.compare a.id b.id)
    |> M.return
  ;;

  let insert_credit_note t (c : Credit_note.t) = insert t.credit_notes c.id c

  let credit_notes_of_invoice t invoice =
    Hashtbl.data t.credit_notes
    |> List.filter ~f:(fun (c : Credit_note.t) -> Invoice_id.equal c.invoice invoice)
    |> List.sort ~compare:(fun (a : Credit_note.t) (b : Credit_note.t) ->
      Credit_note_id.compare a.id b.id)
    |> M.return
  ;;

  let find_dunning t invoice = M.return (Hashtbl.find t.dunning invoice)

  let save_dunning t (d : Dunning.t) =
    save
      t.dunning
      d.invoice
      d
      ~revision:(fun (d : Dunning.t) -> d.revision)
      ~stored_revision:(fun (d : Dunning.t) -> d.revision)
  ;;

  let open_dunning t =
    Hashtbl.data t.dunning
    |> List.filter ~f:(fun (d : Dunning.t) -> not (Dunning.State.is_final d.state))
    |> List.sort ~compare:(fun (a : Dunning.t) (b : Dunning.t) ->
      Invoice_id.compare a.invoice b.invoice)
    |> M.return
  ;;

  let number t ~counter ~document =
    let key = counter ^ "\000" ^ document in
    match Hashtbl.find t.numbers key with
    | Some n -> M.return n
    | None ->
      let n = 1 + Option.value (Hashtbl.find t.counters counter) ~default:0 in
      Hashtbl.set t.counters ~key:counter ~data:n;
      Hashtbl.set t.numbers ~key ~data:n;
      M.return n
  ;;
end

module Direct = Make (Monad.Ident)
