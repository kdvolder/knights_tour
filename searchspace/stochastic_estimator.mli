(** Interface for stochastic estimation functions *)

(** a [decision] represent a choice that has been made at a decision point *)
type decision = {
    chosen: int;
    (** A number between [0] inclusive and [choices] exclusive, indicating which choice was made*)
    choices: int
    (** A number >= [2] indicating the number of available choices at a decision point. *)
}

(** A [rng] is a function that accepts a positive integer as an argument and returns a
    random integer between 0 (inclusive) and that integer (exclusive)*)
type rng = int -> int

(** [random_walk] searchspace walks a random path in the search space. Every time
    a decision point is reached, a random choice is made among the available options.
    The decisions are recorded and returned along with the final result. *)
val random_walk : rng -> 'a Searchspace.t -> decision List.t * 'a option