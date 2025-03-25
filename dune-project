(lang dune 2.9)
(name zkocaml)
(version 0.1.0)

(generate_opam_files true)

(source (github username/zkocaml))
(license MIT)
(authors "ZK Researcher")
(maintainers "maintainer@example.com")

(package
 (name zkocaml)
 (synopsis "OCaml library for optimized zero-knowledge proofs")
 (description "A zero-knowledge proof library with novel optimizations for off-chain computation")
 (depends
  (ocaml (>= 4.12.0))
  dune
  (zarith (>= 1.12))
  (core (>= v0.15.0))
  (ppx_jane (>= v0.15.0))
  (ppx_deriving (>= 5.2.1))
  (domainslib (>= 0.4.2))
  (alcotest :with-test)
  (odoc :with-doc)))