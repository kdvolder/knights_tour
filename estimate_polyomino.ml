open Pentominos

let format_number n =
  if n >= 1e10 || (n > 0. && n < 1.) then
    Printf.sprintf "%.5e" n
  else
    Int64.to_string (Int64.of_float n)

let batch_size = 1000

let () =
  if Array.length Sys.argv <> 2 then begin
    Printf.eprintf "Usage: %s <puzzle-file>\n" Sys.argv.(0);
    exit 1
  end;
  let puzzle_file = Sys.argv.(1) in
  let puzzle =
    In_channel.with_open_text puzzle_file Puzzle.load
  in
  let searchspace = Puzzle.solve puzzle in
  let estimator = Stochastic_estimator.create ~selector:Stochastic_estimator.greedy_completion_selector searchspace in
  
  Printf.printf "%-5s | %-7s | %-12s | %-12s | %-8s | %-12s | %-10s | %-10s | %-10s | %s\n" "Batch" "Samples" "Nodes Est" "Fails Est" "Sols Est" "Materialized" "%Complete" "Pruned" "Elapsed" "ETA";
  Printf.printf "----- | ------- | ------------ | ------------ | -------- | ------------ | ---------- | ---------- | ---------- | ------------------\n";
  
  let batch_count = ref 0 in
  let total_samples = ref 0 in
  Stochastic_estimator.run_with_progress ~batch_size ~on_progress:(fun p ->
    incr batch_count;
    total_samples := !total_samples + batch_size;
    let est = Stochastic_estimator.estimates estimator in
    Printf.printf "%-5d | %-7d | %12s | %12s | %-8s | %-12d | %10.2e%% | %-10d | %-10s | %s\n%!"
      !batch_count !total_samples
      (format_number est.nodes) (format_number est.fails) (format_number est.solutions)
      p.materialized_nodes
      p.progress_percent
      p.pruned_nodes
      (Stochastic_estimator.format_time p.elapsed_seconds)
      (Stochastic_estimator.format_time p.estimated_remaining_seconds);
    flush stdout
  ) estimator;
  
  (* Final report *)
  let est = Stochastic_estimator.estimates estimator in
  Printf.printf "\nDone! Final estimates: nodes=%.0e, fails=%.0e, solutions=%.1f\n" est.nodes est.fails est.solutions;
