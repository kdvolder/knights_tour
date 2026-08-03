type progress = {
  batch : int;                          (* Batch number, starts at 0 for initial state *)
  total_samples : int;                  (* Cumulative samples across all batches *)
  elapsed_seconds : float;
  total_nodes_estimate : float;
  fails_estimate : float;
  solutions_estimate : float;
  materialized_nodes : int;
  pruned_nodes : int;
  progress_ratio : float; (* 0..1 *)
  estimated_remaining_seconds : float;
}

open Stochastic_estimator

let make_progress (start_time : float) (est : 'a Stochastic_estimator.t) : progress =
  let now = Unix.gettimeofday () in
  let elapsed = now -. start_time in
  let ests = Stochastic_estimator.estimates est in
  let materialized_at_load = Stochastic_estimator.materialized_at_load est in
  (* Work accounting: total_work is what remains to be done this session *)
  let work_done = ests.materialized_nodes - materialized_at_load in
  let total_work = ests.nodes -. Float.of_int materialized_at_load in
  let work_remaining = total_work -. Float.of_int work_done in
  let progress_ratio =
    if total_work > 0. then Float.of_int work_done /. total_work
    else 0.0
  in
  let estimated_remaining =
    if progress_ratio > 0. && progress_ratio < 1. then
      elapsed *. (work_remaining /. Float.of_int work_done)
    else if progress_ratio >= 1. then
      0.0
    else
      Float.infinity
  in
  {
    batch = 0; total_samples = 0;
    elapsed_seconds = elapsed;
    total_nodes_estimate = ests.nodes;
    fails_estimate = ests.fails;
    solutions_estimate = ests.solutions;
    materialized_nodes = ests.materialized_nodes;
    pruned_nodes = ests.pruned_nodes;
    progress_ratio;
    estimated_remaining_seconds = estimated_remaining;
  }

let default_progress_printer (p : progress) : bool =
  let eta_str =
    if p.progress_ratio >= 1. then "done"
    else if Float.is_infinite p.estimated_remaining_seconds then "inf"
    else Table_logger.format_time 20 p.estimated_remaining_seconds
  in
  Printf.printf "[%5.1f%%] materialized: %d, elapsed: %-20s ETA: %s\n" (p.progress_ratio *. 100.0) p.materialized_nodes (Table_logger.format_time 20 p.elapsed_seconds) eta_str;
  flush stdout; true

let run_with_progress ?(batch_size = 100) ?(on_progress = default_progress_printer) (est : 'a t) : unit =
  let start_time = Unix.gettimeofday () in
  let batch_count = ref 0 in
  let total_samples = ref 0 in
  (* Initial progress event (batch 0, 0 samples, pre-sampling state) *)
  let p0 = { (make_progress start_time est) with batch = 0; total_samples = 0 } in
  if not (on_progress p0) then raise Exit;
  let rec loop () =
    if not (is_completed est) then (
      ignore (sample batch_size est);
      incr batch_count;
      total_samples := !total_samples + batch_size;
      let p = { (make_progress start_time est) with batch = !batch_count; total_samples = !total_samples } in
      if not (on_progress p) then raise Exit;
      loop ()
    )
  in
  (try loop () with Exit -> ());
  (* Final report only when complete — interrupted runs already printed their last batch *)
  if is_completed est then (
    let p = { (make_progress start_time est) with batch = !batch_count; total_samples = !total_samples } in
    ignore (on_progress p)
  )

(** Progress Data Structure Tests *)

let%expect_test "make_progress initial state" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution_a";
      return "solution_b";
      empty
    ]
  ) in
  let est = create simple_tree in
  let start_time = Unix.gettimeofday () in
  ignore (Unix.sleepf 0.1);
  let p = make_progress start_time est in
  Printf.printf "materialized: %d\n" p.materialized_nodes;
  Printf.printf "progress%%: %.1f\n" (p.progress_ratio *. 100.0);
  [%expect{|
    materialized: 1
    progress%: 100.0
  |}]
end

let%expect_test "make_progress after sampling" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution_a";
      return "solution_b";
      empty
    ]
  ) in
  let est = create simple_tree in
  ignore (sample 10 est);
  let start_time = Unix.gettimeofday () in
  ignore (Unix.sleepf 0.1);
  let p = make_progress start_time est in
  Printf.printf "materialized: %d\n" p.materialized_nodes;
  Printf.printf "progress%%: %.1f\n" (p.progress_ratio *. 100.0);
  [%expect{|
    materialized: 4
    progress%: 100.0
  |}]
end

(** Reporter Integration Tests *)

let%expect_test "run_with_progress invokes callback after each batch" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution_a";
      return "solution_b";
      empty
    ]
  ) in
  let est = create simple_tree in
  let reports = ref [] in
  run_with_progress ~batch_size:3 ~on_progress:(fun p ->
    reports := !reports @ [p.materialized_nodes];
    true
  ) est;
  Printf.printf "Reports: %s\n" (String.concat ", " (List.map string_of_int !reports));
  [%expect{|
    Reports: 1, 4, 4
  |}]
end

let%expect_test "run_with_progress uses default stdout printer" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution_a";
      return "solution_b";
      empty
    ]
  ) in
  let est = create simple_tree in
  run_with_progress ~batch_size:3 est;
  [%expect{|
    [100.0%] materialized: 1, elapsed:                  0 s ETA: done
    [100.0%] materialized: 4, elapsed:                  0 s ETA: done
    [100.0%] materialized: 4, elapsed:                  0 s ETA: done
    |}]
end

let%expect_test "run_with_progress stops when complete" = begin
  let simple_tree = Searchspace.(
    alt [ return "sol"; empty ]
  ) in
  let est = create simple_tree in
  let reports = ref [] in
  run_with_progress ~batch_size:5 ~on_progress:(fun p ->
    reports := !reports @ [p.materialized_nodes];
    true
  ) est;
  Printf.printf "Final materialized: %d\n" (List.hd (List.rev !reports));
  [%expect{|
    Final materialized: 3
  |}]
end
