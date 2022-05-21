(**
  A polyomino puzzle consists of a board and a set of polyomino pieces to be placed onto
  the board. The puzzle is considered solved when all pieces have been placed.
*)

open Knights_tour

(** Data type representing the state of a puzzle.*)
type t = {
    (** pieces remaining to be placed *)
    pieces : Polyomino.t list;
    (** board upon which to place the pieces. Maybe some pieces are
        already on the board for a partially solved puzzle. *)
    board : Board.t;
}

(** The initial state of the 'classic' Pentominos puzzle.*)
val classic : t

val solve : t -> Board.t Searchspace.t