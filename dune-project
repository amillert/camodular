(lang dune 2.8)

(using menhir 2.1)

(name camodular)

(generate_opam_files true)

(package
 (name camodular)
 (depends (ocaml (>= "4.08.0")) menhir sedlex))
