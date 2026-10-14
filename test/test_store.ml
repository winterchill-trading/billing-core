open! Core
open Billing_core
module Conformance = Store_conformance.Make (Monad.Ident) (Store_memory.Direct)

let%expect_test "the in-memory store passes the conformance scenarios" =
  print_s [%sexp (Conformance.check (Store_memory.Direct.create ()) : unit Or_error.t)];
  [%expect {| (Ok ()) |}]
;;
