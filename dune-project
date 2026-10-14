(lang dune 3.17)

(name billing-core)

(generate_opam_files true)

(license MIT)

(authors "WinterChill AI")

(maintainers "hello@winterchill.xyz")

(source
 (github winterchill-trading/billing-core))

(homepage "https://github.com/winterchill-trading/billing-core")

(bug_reports "https://github.com/winterchill-trading/billing-core/issues")

(documentation "https://github.com/winterchill-trading/billing-core#readme")

(package
 (name billing-core)
 (synopsis
  "Billing as pure state machines: plans, subscriptions, usage, invoices, dunning, credit notes and ledger bookings")
 (description
  "billing-core models recurring and usage-based billing for banks and payment platforms as values and pure transitions: price lists of plans with flat, per-seat, tiered usage and percentage-fee components; subscriptions with calendar periods, plan changes and proration; usage records ingested once per id; invoices with lines, a tax hook, rounding rules and gapless numbering; payments, receipts, credit notes and dunning; and the balanced double-entry ledger entries for receivables, fee income and tax. Persistence is a module type with an in-memory implementation and a conformance suite. Requires the OxCaml compiler (through its money and ledger dependencies).")
 (tags
  (billing invoicing subscriptions usage dunning ledger banking oxcaml))
 (depends
  oxcaml
  (ocaml
   (and
    (>= 5.2.0)
    (< 5.3~)))
  dune
  (core
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (ppx_jane
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (zarith
   (>= 1.12))
  money
  ledger
  (expect_test_helpers_core
   (and
    :with-test
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (odoc :with-doc)))
