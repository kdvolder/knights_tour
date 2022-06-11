(** A move takes a knight from one Point.t on the board to another. *)
type move = { from : Point.t; dest : Point.t; }

(** Checks whether this move represents a valid 'L-shaped' knight move.*)
val moveIsValid : move -> bool

(** Converts move to user-friendly string. This string is meant to be shown to
    a user in a list of valid moves, they can choose from. *)
val move_to_string : move -> string

(** Like [move_to_string], but operates on a list of moves. *)
val moves_to_string : move list -> string

module Board :
  sig

    (** represents the 'chessboard' of n x n squares. The [Board.t] data 
    type is two-dimenional array. Each cell in the array keeps track of whether
    and when a knight has visited the square. If a knight has visited it, 
    the cell contains a number indicating when (i.e. the number indicates 
    which turn of the game the knight visited that square. *)
    type t

    (** The size of the board. If [Board.size b] is n, then the boards is a
        n x n square. *)
    val size : t -> int

    (** [Board.make n] creates a board of size n x n *)
    val make : int -> t

    (** Produces a string representation of the board that is suitable for printing 
        on the console.*)
    val to_string : t -> string

    (** [Board.get b {x;y}] reads the current contents of a square of the board at a 
        given [{x;y}] coordinate. *)
    val get : t -> Point.t -> int option

    (** [Board.set b {x;y} newContents] sets the contents of the board at a given coordinate.*)
    val set : t -> Point.t -> int option -> t

    (** Drays the board using the [graphics] library.*)
    val draw : t -> unit

    (** Validates where a given board representation corresponds to a valid sequence of moves.
        (I.e. if go through the squares on the board in the order indicated by the numbers recorded
        in them, then each transition represents a valid move. *)
    val isValid : t -> bool

    (** Checks that given coordinate is a valid place for a knight to move to. This means that 
        the coordinate is within the board; and has not yet been visited by a knight before.*)
    val isValidTarget : t -> Point.t -> bool

    (** Counts the number of already visited squares in the board.*)
    val count_targets : t -> Point.t -> int
  end
module GameState :
  sig

    (** A [GameState.t] represents the complete state of the game at a given Point.t in time. This 
        includes the state of the board as well as the current position of the knight on the board.*)
    type t

    (** Creates a 'fresh' gamestate consisting of an empty board and the knight at its starting position. *)
    val make : int -> t

    (** Returns a list of moves that is currently valid given the current state of the game. *)
    val valid_moves : t -> move list

    (** Returns the number of steps the knight has taken so far. Placing the knight at the start position
        is counted as a step. (So the number starts a 1 on a 'fresh' board)*)
    val steps : t -> int

    (** Returns current state of the board. *)
    val board : t -> Board.t

    (** Update the game state by doing a given move. *)
    val do_move : t -> move -> t

    (** Decide whether the current state represents a winning / solved state. *)
    val isWinning : t -> bool
  end

(** Searches for a single solution to the game. You can pass in a optional
    [report_backtracking] callback. This is called every time when the search process
    reaches a 'dead-end' and has to backtrack. This is useful to monitor progress
    of the search when it is taking a long time. This can be used, for example
    to periodically draw the best solution found so far on the screen. *)  
val solve : ?report_backtracking:(GameState.t -> unit) -> GameState.t -> Board.t option

(** Create a [Board.t Searchspace.t] representing all solutions to the knights tour.*)
val make_search_space : ?report_backtracking:(GameState.t -> unit) -> int -> Board.t Searchspace.t

