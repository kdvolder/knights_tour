(** Progress monitoring and reporting for stochastic estimators *)

type progress = {
  batch : int;
  total_samples : int;
  elapsed_seconds : float;
  total_nodes_estimate : float;
  fails_estimate : float;
  solutions_estimate : float;
  materialized_nodes : int;
  pruned_nodes : int;
  progress_ratio : float; (* 0..1 *)
  estimated_remaining_seconds : float;
}
(** Progress record with batch number, cumulative samples, elapsed time, completion ratio (0..1), and ETA. *)

val make_progress : float -> 'a Stochastic_estimator.t -> progress
(** [make_progress start_time est] creates a progress record from the elapsed time since [start_time]
    and the current state of the estimator. *)

val default_progress_printer : progress -> bool
(** Default stdout printer for progress reports. Shows completion %, materialized nodes, and ETA.
    Always returns [true] to continue running. *)

val run_with_progress : ?batch_size:int -> ?on_progress:(progress -> bool) -> 'a Stochastic_estimator.t -> unit
(** [run_with_progress ~batch_size ?on_progress est] runs batches of sampling and invokes
    [on_progress] after each batch with a progress record. Stops when the search space is complete
    or [on_progress] returns [false]. Defaults to [default_progress_printer] for stdout output. *)
