(** A {{: https://en.wikipedia.org/wiki/Pentomino}Polyomino} is a puzzle piece
    composed of a number equally sized grid-squares. 
    Depending on the number of squares they can go by different names. 

    - 5 squares: 5-omino or {b pentomino}
    - 2 squares: 2-omino or {b domino}
    
    We represent polyominos as a Set of points with an [x] and [y] coordinate.

    Typically puzzles and games involving polyominos have physical puzzle pieces
    that can be flipped over and rotated. The pieces can also be placed down
    on the puzzle board at different locations. 

    This means that to decide whether two sets of points represent the same
    Polyomino puzzle piece, we need to consider them equivalent under these
    transformations:

    - rotate 90, 180 or 270 degrees
    - flipped around (mirroring along the vertical or horizontal axis)
    - translated
*)

open Knights_tour

(** An unplaced polyomino puzzle piece. It is characterized by a normalized
    PointSet, making it invariant to any combination 90 degree rotation, 
    mirroring, and translation.*)
type t

(** Compares two polymoninos to see if they represent the same shape;
    invariant to rotation, mirroring and translation*)
val compare : t -> t -> int

(** Create a polyomino from a given set of points *)
val create : PointSet.t -> t

(** Gets the canonical PointSet that characterizes this Polyomino *)
val points : t -> PointSet.t

(** Gets all variants of a given Polyomino. A variant is simlar shape 
    obtained by applying rotation and mirroring transformations (but no translation).
    I.e. it is a specific orientation of Polyomino that has not yet been placed
    on a specific location of the board. *)
val variants : t -> PointSet.t List.t

(** Takes a 'string image' of a pointset and parses it.
    See [PointSet.of_string] for details about the  format.*)
val of_string : string -> t

(** Create a 'string-image' of the polyomino*)
val to_string : t -> string

(** Obtain all unique polymino of order [n] *)
val of_order : int -> t Searchspace.t

(** Print string image of polyomino to a formatter *)
val pp_poly : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]