Knights Tour
------------

An ocaml puzzle solver that solves the classic [Knight's Tour](https://en.wikipedia.org/wiki/Knight%27s_tour) and [Pentomino](https://en.wikipedia.org/wiki/Pentomino) puzzles.

To run the solver:

```
dune exec ./knight_solve.exe # Solve knights_tour
```

To play the game yourself interactively:

```
dune exec ./knight_play.exe
```

## Pentominos

To run the solver:

```
dune exec ./pento_solve.exe
```

## Opam install

```
opam install knigths_tour
```

## Api docs

This puzzle solver was essentially a test-case for experimenting with
a general purpose 'searchspace solver'. This is included as a library
that might potentially be of use outside of this project.

The generated api docs are [here](https://kdvolder.github.io/knights_tour/knights_tour/index.html). 
The most interesting part of this library is the [SearchSpace](https://kdvolder.github.io/knights_tour/knights_tour/Searchspace/index.html). 
