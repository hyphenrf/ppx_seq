(lang dune 2.9)
(generate_opam_files true)

(authors "Hazem Elmasry")
(maintainers hyphens@pm.me)

(name ppx_seq)
(license ISC)
(homepage https://sr.ht/~hyphens/ppx_seq)
(source (uri git+https://git.sr.ht/~hyphens/ppx_seq))
(bug_reports https://lists.sr.ht/~hyphens/ppx_seq)
(documentation https://git.sr.ht/~hyphens/ppx_seq/tree/master/item/README)

(package
 (name ppx_seq)
 (depends
  (ppxlib (and (>= 0.23) :build))
  (ocaml (>= 4.04.2))
  (dune :build)
  seq)
 (tags (syntax))
 (synopsis "Seq literals ppx for OCaml")
 (description "Simple unintrusive ppx rewriter that offers Seq literals.\n\
               It offers List-like syntax in the form of [%seq a; b; c...]"))
