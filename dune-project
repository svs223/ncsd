(lang dune 3.17)

(name ncsd)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ncsd)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml lwt lwt_ppx libncs)
 (tags
  ("add topics" "to describe" your project)))

(package 
  (name libncs)
  (synopsis "")
  (depends ocaml unix lwt logs lwt_ppx yojson ppx_yojson_conv))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
