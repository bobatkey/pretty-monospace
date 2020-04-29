(lang dune 2.5)
(name pretty-monospace)

(generate_opam_files true)

(source (github bobatkey/pretty-monospace))
(license ISC)
(authors "Robert Atkey")
(maintainers "bob.atkey@gmail.com")

(package
 (name pretty-monospace)
 (synopsis "A library for pretty-printing monospaced text")
 (description "A library for pretty-printing monospaced text, based on Wadler's combinators.")
 (depends
  (dune (> 1.5))))
