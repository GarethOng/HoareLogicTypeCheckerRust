(lang dune 3.16)
(using menhir 2.1)

(name RustVerifier)
(generate_opam_files true)

(source
 (github GarethOng/RustVerifier))

(authors "Gareth Ong <gareth.ong@u.nus.edu>")

(license MIT)

(package
 (name RustVerifier)
 (synopsis "Program verifier")
 (description "A verifier for Rust-like language with ownership model, borrowing and lifetimes")
 (depends
  (ocaml (>= 5))
  (z3 (>= 4.12.2))
  (menhir (>= 20220210))
  (ounit2 (>= 2.2.7))
  (dune (>= 3.8.0))))
