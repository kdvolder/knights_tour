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

(** Same as the 'classic' puzzle, but one assymetric puzzle piece deliberately has
    all but one of its variants removed (this ensures that symmetric solutions are
    eliminated, by not allowing that one piece to only be used in one orientation)*)
val classic_no_symmetric_solutions : t

val solve : ?report_progress:(string -> t -> unit) -> t -> Board.t Searchspace.t

(** Write a textual representation of a puzzle to a channel. The format is human readable;
    but it can also be used to restore a puzzle via the [load] function.*)
val save : out_channel -> t -> unit

(** Write a textual representation of a puzzle to a formatter. The format is human readable;
    but it can also be used to restore a puzzle via the [load] function.*)
val save_fmt : Format.formatter -> t -> unit

(** Load a puzzle from a textual representation as produced by [save].*)
val load : in_channel -> t

(** Load a puzzle from a textual representation as produced by [save].*)
val load_lines : Lines.t -> t