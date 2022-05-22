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

(** An unplaced polyomino puzzle piece. It is characterized by a normalized
    PointSet, making it invariant to any combination 90 degree rotation, 
    mirroring, and translation.*)
type t

(** Gets the 'name' of a polyomino. The name is unique for polyomino of a the same
    order (i.e. if two polyominos have the same order and the same name, it means
    they are the same polyomino)*)
val name : t -> char

(** Compares two polymoninos to see if they represent the same shape;
    invariant to rotation, mirroring and translation*)
val compare : t -> t -> int

(** The number of squares in a polyomino is called its 'order'.*)
val order : t -> int

(** Gets the canonical PointSet that characterizes this Polyomino *)
val points : t -> PointSet.t

(** Gets all variants of a given Polyomino. A variant is simlar shape 
    obtained by applying rotation and mirroring transformations (but no translation).
    I.e. it is a specific orientation of Polyomino that has not yet been placed
    on a specific location of the board. *)
val variants : t -> PointSet.t List.t

(** Create a 'string-image' of the polyomino*)
val to_string : t -> string

(** Obtain all unique polymino of order [n] *)
val of_order : int -> t list

(** Print string image of polyomino to a formatter *)
val pp_poly : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]

(** Find the first polyomino that is completely assymetric and remove all but
    one of its variants. This is a 'hack' that allows eliminating equivalent
    symmetric solutions from a puzzle.*)
val pin_symmetry : t list -> t list