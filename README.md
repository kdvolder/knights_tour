Knights Tour
------------

An ocaml puzzle solver that solves the classic [Knight's Tour](https://en.wikipedia.org/wiki/Knight%27s_tour).

To run the solver:

```
dune exec ./knight_solve.exe
```

To play the game yourself interactively:

```
dune exec ./knight_play.exe
```

## Opam install

TODO: Once we figured out how to publish a package on opam.

## Api docs

This puzzle solver was essentially a test-case for experimenting with
a general purpose 'searchspace solver'. This is included as a library
that might potentially be of use outside of this project.

The generated api docs are [here](https://kdvolder.github.io/knights_tour/knights_tour/index.html). 
The most interesting part of this library is the [SearchSpace](https://kdvolder.github.io/knights_tour/knights_tour/Lib/Searchspace/index.html) 
Module. The rest of the code isn't really as useful or re-usable and has almost no documentation.

However the code that creates a 'searchspace' to solve the puzzle is [here](https://github.com/kdvolder/knights_tour/blob/v0.0.1/lib/horsejump.ml#L236)
and may make for a good example on how it is intended to be used.
