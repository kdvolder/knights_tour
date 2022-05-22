(**
  A polyomino puzzle consists of a board and a set of polyomino pieces to be placed onto
  the board. The puzzle is considered solved when all pieces have been placed.
*)

open Knights_tour

(** Data type representing the state of a puzzle.*)
type t = {
    pieces : Polyomino.t list; (** pieces remaining to be placed *)
    board : Board.t; (** board upon which to place the pieces. For a (partially) solved
                         puzzle it tracks what piece occupies each square. *)

}

(** The initial state of the 'classic' Pentominos puzzle.*)
val classic : t

val solve : ?report_progress:(string -> t -> unit) -> t -> Board.t Searchspace.t