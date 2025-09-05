(** Interface for stochastic estimation functions *)

(** a [decision] represent a choice that has been made at a decision point *)
type decision = {
    (** A number between [0] inclusive and [choices] exclusive, indicating which choice was made*)
    chosen: int;
    (** A number >= [2] indicating the number of available choices at a decision point. Each of the 
       available choices can be indicate by a number from [0] upto and excluding [choices] *)
    choices: int
}

(** A [rng] is a function that accepts a positive integer as an argument and returns a
    random integer between 0 (inclusive) and that integer (exclusive)*)
type rng = int -> int

(** [random_walk] searchspace walks a random path in the search space. Every time *)
val random_walk : rng -> 'a Searchspace.t -> decision List.t * 'a option