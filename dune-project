(lang dune 3.1)
(name knights_tour)
(version 0.0.7)

(generate_opam_files true)

(source (github kdvolder/knights_tour))
(license MIT)
(authors "Kris De Volder")
(maintainers "kris.de.volder@gmail.com")

(package
 (name knights_tour)
 (synopsis "Solves the 'Knights Tour' and various 'Poyomino' puzzles")
 (description "See https://en.wikipedia.org/wiki/Knight%27s_tour or https://en.wikipedia.org/wiki/Polyomino")
 (depends 
  ("ocaml" (>= "5.1.0")) 
  "graphics" "stdio" "ppx_inline_test" "ppx_expect" 
  ("odoc" :with-doc)
  ))