(lang dune 2.9.1)
(generate_opam_files true)

(authors "Hazem Elmasry")
(maintainers hyphens@pm.me)

(name ppx_seq)
(license ISC)
(source (github hyphenrf/ppx_seq))

(package
 (name ppx_seq)
 (depends
  (ppxlib (>= 0.24.0)))
 (tags (syntax))
 (synopsis "Simple unintrusive ppx rewriter that offers Seq literals")
 (description "Simple unintrusive ppx rewriter that offers Seq literals"))
