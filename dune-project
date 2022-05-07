(lang dune 3.1)
(name knights-tour)
(version 0.0.1)

(generate_opam_files true)

(source (github kdvolder/knights-tour))
(license MIT)
(authors "Kris De Volder")
(maintainers "kris.de.volder@gmail.com")

(package
 (name knights-tour)
 (synopsis "Solves the Knight's tour puzzle")
 (description "See https://en.wikipedia.org/wiki/Knight%27s_tour")
 (depends "graphics" "stdio" "ppx_inline_test"))