(lang dune 3.16)

(using menhir 3.0)

(name jackvm)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Raagam Parmar")

(maintainers "Raagam Parmar")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name jackvm)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune assembler)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
