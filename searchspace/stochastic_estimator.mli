(** Interface for stochastic estimation functions *)

type decision = {
        chosen: int;
        (** A number between [0] inclusive and [choices] exclusive, indicating which choice was made*)
        choices: int
        (** A number >= [2] indicating the number of available choices at a decision point. *)
}
(** a [decision] represents a choice that has been made at a decision point *)

type rng = int -> int
(** A [rng] is a function that accepts a positive integer as an argument and returns a
    random integer between 0 (inclusive) and that integer (exclusive)*)

val random_walk : rng -> 'a Searchspace.t -> decision List.t * 'a option
(** [random_walk] searchspace walks a random path in the search space. Every time
    a decision point is reached, a random choice is made among the available options.
    The decisions are recorded and returned along with the final result. *)

type 'a child_selector
(** Selector functions for choosing among children at a fork.*)

val uniform_selector : 'a child_selector
(** Uses a uniform random choice among available children. *)

val undersampled_selector : 'a child_selector
(** Prefers to select children that have been sampled less often. *)

val weighted_selector : 'a child_selector
(** Prefers to select children that have a higher number of descendants. *)

val variance_selector : 'a child_selector
(** Prefers to select children that have a higher variance in their estimates. *)

type estimates = {
    nodes : float;
    (** The estimated number of nodes in the search space. *)
    fails : float;
    (** The estimated number of leaf nodes in the search space that represent failures. *)
    solutions : float;
    (** The estimated number of leaf nodes in the search space that represent solutions. *)
    materialized_nodes : int;
    (** The number of nodes that were actually materialized during the estimation process. *)
}

val estimate : ?selector:'a child_selector -> int -> 'a Searchspace.t -> estimates
(** [estimate n searchspace] performs [n] random walks in the given search space
    and uses the results to produce an estimate of the total number of nodes,
    fails and solutions in the search space. The estimate is returned as a record
    of type [estimates]. The number of materialized nodes is also reported.
    
    The optional [selector] argument can be used to influence how choices are made
    at decision points. By default, the [undersampled_selector] is used. Other strategies
    are available in this module. *)

type stats = {
    nodes : int;
    (** The exact number of nodes in the search space. *)
    forks : int;
    (** The exact number of fork nodes in the search space. *)
    fails : int;
    (** The exact number of leaf nodes in the search space that represent failures. *)
    solutions : int;
    (** The exact number of leaf nodes in the search space that represent solutions. *)
}

val calculate_true_values : 'a Searchspace.t -> stats
(** [calculate_true_values searchspace] fully explores the given search space
    and returns the exact counts of nodes, fails and solutions as an [estimates] record.

    This function is useful for validating the accuracy of estimates produced
    by the [estimate] function. It should only be used on small search spaces
    where a full exploration is feasible. *)

type 'a t
(** An incremental estimator for a search space. *)

val create : ?selector:'a child_selector -> 'a Searchspace.t -> 'a t
(** [create ?selector searchspace] creates a new incremental estimator for the given search space, optionally using a custom selector. *)

val sample : int -> 'a t -> unit
(** [sample n est] performs [n] additional samples, updating the estimator's statistics. *)

val estimates : 'a t -> estimates
(** [estimates est] returns the current estimates from the estimator. *)