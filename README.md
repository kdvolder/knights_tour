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

There are several 'executables' to run the solver.

To solve classic pentomino puzzle of 'chessboard with missing center':

```
dune exec ./pento_solve.exe
```

Solve hexomino puzzle with a similar 'donut' shape:

```
dune exec ./hexo_solve.exe
```

This generates all possible hexomino puzzle pieces, randomizes their
ordering and then tries to solve them on square board with hole
cut out in the middle.

It will keep running untul you press CTRL-C to terminate the process.
As the piece order is randomized it will find different solutions
each time you run this.

If you want something that produces more 'deterministic results', you 
can use the command:

```
dune exec ./solve_file.exe
```

This will read a puzzle specification from a file called `polymino-puzzle.txt`.
A sample is included in this repo. You can create your own by editing the file,
or you can generate a 'random' hexomino puzzle with:

```
dune exec ./random_hexo_puzzle.exe
```

This will generate puzzle like `hexo_solve.exe` but instead of solving
the puzzle it will write it into a `polymino-puzzle.txt` file which you
can subsequently solve using `solve_file.exe`. 

The `solve_file.exe` solver search exhaustively and report 'progress' by:

- printing solved boards in the terminal.
- drawing the last solved board in a Graphics window.
- occasionally drawing partially solved boards in Graphics window.

*Note 1:* graphical progress reporting is severely limited (only 
few partial boards are drawn and most are skipped). Drawing the boards 
is comparatively slow, so skipping it makes the search a 
lot faster.

*Note 2:* The total number of solutions to the hexomino puzzles is unknown 
but it is an astronomically large number and the solver will just keep
searching forever until you terminate the process (CTRL-C).

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
