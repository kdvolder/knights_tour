type progress = {
  elapsed_seconds : float;
  total_nodes_estimate : float;
  fails_estimate : float;
  solutions_estimate : float;
  materialized_nodes : int;
  pruned_nodes : int;
  progress_percent : float;
  estimated_remaining_seconds : float;
}

open Stochastic_estimator

let make_progress (start_time : float) (est : 'a t) : progress =
  let now = Unix.gettimeofday () in
  let elapsed = now -. start_time in
  let ests = Stochastic_estimator.estimates est in
  let progress_percent =
    if ests.nodes > 0. then
      (float_of_int ests.materialized_nodes) /. ests.nodes *. 100.0
    else 0.0
  in
  let estimated_remaining =
    if progress_percent > 0. && progress_percent < 100. then
      elapsed *. (100.0 /. progress_percent) -. elapsed
    else if progress_percent >= 100. then
      0.0
    else
      Float.infinity
  in
  {
    elapsed_seconds = elapsed;
    total_nodes_estimate = ests.nodes;
    fails_estimate = ests.fails;
    solutions_estimate = ests.solutions;
    materialized_nodes = ests.materialized_nodes;
    pruned_nodes = ests.pruned_nodes;
    progress_percent;
    estimated_remaining_seconds = estimated_remaining;
  }

let default_progress_printer (p : progress) : unit =
  let eta_str =
    if p.progress_percent >= 100. then "done"
    else if Float.is_infinite p.estimated_remaining_seconds then "inf"
    else Table_logger.format_time 20 p.estimated_remaining_seconds
  in
  Printf.printf "[%5.1f%%] materialized: %d, elapsed: %-20s ETA: %s\n" p.progress_percent p.materialized_nodes (Table_logger.format_time 20 p.elapsed_seconds) eta_str;
  flush stdout

let run_with_progress ?(batch_size = 100) ?(on_progress = default_progress_printer) (est : 'a t) : unit =
  let start_time = Unix.gettimeofday () in
  let rec loop () =
    if not (is_completed est) then (
      ignore (sample batch_size est);
      let p = make_progress start_time est in
      on_progress p;
      loop ()
    )
  in
  loop ();
  (* Final report when complete *)
  let p = make_progress start_time est in
  on_progress p

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
  Printf.printf "progress%%: %.1f\n" p.progress_percent;
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
  Printf.printf "progress%%: %.1f\n" p.progress_percent;
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
    reports := !reports @ [p.materialized_nodes]
  ) est;
  Printf.printf "Reports: %s\n" (String.concat ", " (List.map string_of_int !reports));
  [%expect{|
    Reports: 4, 4
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
    reports := !reports @ [p.materialized_nodes]
  ) est;
  Printf.printf "Final materialized: %d\n" (List.hd (List.rev !reports));
  [%expect{|
    Final materialized: 3
  |}]
end
