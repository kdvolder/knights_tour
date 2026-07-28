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

val probabilistic_undersampled_selector : 'a child_selector
(** Similar to [undersampled_selector] but uses a probabilistic approach to avoid
    over-focusing on the least-sampled children. *)

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

val create : ?selector:'a child_selector -> ?on_solution:('a -> unit) -> 'a Searchspace.t -> 'a t
(** [create ?selector ?on_solution searchspace] creates a new incremental estimator for the given search space, optionally using a custom selector and an optional callback to receive solutions as they are found. The default callback does nothing. *)

val sample : int -> 'a t -> bool
(** [sample n est] performs [n] additional samples, updating the estimator's statistics.
    Returns [true] if the sampling was complete, meaning the entire search space is completely
    explored. This means there is no point in doing any more sampling. *)
val estimates : 'a t -> estimates
(** [estimates est] returns the current estimates from the estimator. *)

type leaf_type = Fail | Solution

type materialized_stats = {
    total_materialized : int;
    (** Total number of nodes in the materialized tree. *)
    max_depth : int;
    (** Maximum depth of any node in the materialized tree. *)
    leaf_depths_fail : (int * int) list;
    (** Depth histogram for failed leaves in the materialized tree. *)
    leaf_depths_solution : (int * int) list;
    (** Depth histogram for solution leaves in the materialized tree. *)
    fork_depths : (int * int) list;
    (** List of (depth, count) pairs for fork nodes in the materialized tree. *)
    avg_leaf_depth_fail : float;
    (** Average depth of failed leaves in the materialized tree. *)
    avg_leaf_depth_solution : float;
    (** Average depth of solution leaves in the materialized tree. *)
}
(** Statistics about the structure of the materialized portion of the search space. *)

val analyze_materialized : 'a t -> materialized_stats
(** [analyze_materialized est] inspects the structure of the already-materialized tree.
    This is a read-only operation that does not trigger any new materialization. *)

(** Progress monitoring and reporting *)

type progress = {
  elapsed_seconds : float;
  total_nodes_estimate : float;
  fails_estimate : float;
  solutions_estimate : float;
  materialized_nodes : int;
  progress_percent : float;
  estimated_remaining_seconds : float;
}
(** Progress record with elapsed time, completion percentage, and ETA. *)

val make_progress : float -> 'a t -> progress
(** [make_progress start_time est] creates a progress record from the elapsed time since [start_time]
    and the current state of the estimator. *)

val format_time : float -> string
(** [format_time seconds] formats a duration in seconds into human-readable form.
    For small values: "1 day, 2 h 30 min 5 s"
    For large values (>1 billion years): "3.17e22 years" *)

val default_progress_printer : progress -> unit
(** Default stdout printer for progress reports. Shows completion %, materialized nodes, and ETA. *)

val run_with_progress : ?batch_size:int -> ?on_progress:(progress -> unit) -> 'a t -> unit
(** [run_with_progress ~batch_size ?on_progress est] runs batches of sampling and invokes
    [on_progress] after each batch with a progress record. Stops when the search space is complete.
    Defaults to [default_progress_printer] for stdout output. *)
