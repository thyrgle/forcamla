(lang dune 3.23)

(name forcamla)

(generate_opam_files true)

(source
 (github thyrgle/forcamla))

(authors "Christopher Sumnicht <csumnicht@berkeley.edu>")

(maintainers "Christopher Sumnicht <csumnicht@berkeley.edu>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name forcamla)
 (synopsis "Organize program execution via formulae")
 (description "Formulas are either terms or compound statements (terms combined with binary operators). Terms may be updated and then compound statements will reflect the changed updated terms. With this event listeners can be constructed that wait for formulas to be satisfied, changed, etc.")
 (depends ocaml)
 (tags
  ("mathematical programming" "formula" "event listeners")))
