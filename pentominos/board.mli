(**
  A Polyomino board is a grid of squares that need to be
  covered by Polyomino puzzle pieces. It is possible for
  some parts of the grid to be blocked off. 
  
  For example in the classical Pentomino puzzle, the board
  is an [8x8] chessboard, with a [2x2] grid at the center
  of the board blocked of. This leaves [8x8-2x2 = 64 - 4 = 60]
  squares to be filled. The 60 remaining squares need to
  be covered up by fitting all of the 12 pentomino shapes,
  which are 5 squares each.
  
  The Board.t data structure keeps track of the current state 
  of the board. This means that it provides a way to:

  - determine which squares are still vacant
  - for occupied squares, determine which puzzle piece has 
    been placed to cover that square.
  - count the number of vacant spots (to determine if 
    the puzzle is solved)
*)

open Knights_tour
open Collections.Persist

type t

(** A value of type [square] indicates the state of a
    square at a given board [{x;y}] coordinate. 
  
    A square can be in either one of the following states:

    - [Blocked]: a square that is not meant to be covered. Either the square is
                not within the bounds of the board, or it is one of the squares
                on the board that are blocked from the start. (e.g. the 4 center
                squares in the classic board) 
    - [Occupied]: a polyomino was placed on the board and it covers the square.
    - [Vacant]: no polymino has been placed that covers the square yet.
    *)
type square =
  | Occupied of Polyomino.t 
  | Vacant
  | Blocked

(** A constant to gives the size in pixels of a polyomino square. *)
val draw_size : int

(** Initialize the graphic module to draw boards in a window on the screen. 
    This function should only be called once, and should be called before
    drawing the first board on the screen. It requires the dimensions of
    the board as an input parameter (needed to open a window of the 
    appropriate size)*)
val init_graphics : Point.t -> unit

(** Each board has a size consisting of a width and height. All non-blocked board
    coordinates are contained within the range [0..width] for the [x] coordinate
    and [0..height] for the [y] coordinate.
    
    This function returns the size of the board as a point [{x=width;y=height}] *)
val size : t -> Point.t

(** Get the state of a square at a given [{x;y}] coordinate. *)
val get : t -> Point.t -> square

(** Initialize a Polyomino board starting state from a 'string image'.
    The string image represents the vacant squares by [`#`] characters
    and any other sqaures by '.'. Note that the dimensions of the board
    are implied. So for example the classic pentomino board is 
    initialized like so:

    {[
        let classic_board = Board.of_string "
          ########
          ########
          ########
          ###..###
          ###..###
          ########
          ########
          ########
        "
    ]}
*)
val of_string : string -> t

(** Loads a board from a string image. This requires a function that interprets
    the characters of the string, mapping each char to [square] state*)
val load_string : (char -> square) -> string -> t

(** Classic Pentomino board. A chess board with the 4 center squares blocked off.*)
val classic : t

(** [rectangle w h] creates a plain rectangular board of given dimensions. All squares in this board
    are vacant (so there are no blocked areas inside the rectangle)*)
val rectangle : int -> int -> t

(** Place a polyomino on the board. It is assumed that the
    given PointSet is a rotated / mirrored / translated variant
    of the given Polyomino. *)
val put : t -> PointSet.t -> Polyomino.t -> t

(** Gets all vacant points on the board (i.e. any point that is not occupied and is not blocked)*)
val vacant : t -> PointSet.t

(** Convert board into a two-dimensional 'string image'. Each square on the board is
    represented by a single character:
    
    - `.`: a vacant square
    - a capital letter: square occupied by polyomino with that name
    - #: a blocked square *)
val to_string : t -> string

(** Draw the board using the [graphics] library*)
val draw : ?black_and_white:bool -> t -> unit

val persistable : (char -> Polyomino.t) -> (module Persistable with type t = t)