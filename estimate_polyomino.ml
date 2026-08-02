open Pentominos

let batch_size = 1000

type progress_row = {
  batch : int;
  total_samples : int;
  nodes_est : float;
  fails_est : float;
  sols_est : float;
  found : int;
  materialized : int;
  pruned : int;
  net_nodes : int;
  undersampled_pct : float;
  progress_pct : float;
  elapsed : string;
  eta : string;
}

let solutions_file_path puzzle_file =
  let today = Unix.localtime (Unix.time ()) in
  let day = today.tm_mday in
  let month = today.tm_mon + 1 in
  let year = today.tm_year + 1900 in
  let hour = today.tm_hour in
  let min = today.tm_min in
  let timestamp = Printf.sprintf "%04d-%02d-%02d-%02d-%02d" year month day hour min in
  let dir = Filename.dirname puzzle_file in
  Filename.concat dir (Printf.sprintf "solutions-%s.txt" timestamp)

let nodes_in_memory est =
  let estimates = Stochastic_estimator.estimates est in
  estimates.materialized_nodes - estimates.pruned_nodes |> Float.of_int

let make_table () =
  Table_logger.add_column ~label:"Batch" ~extract_and_format:(fun w r -> Table_logger.format_int w r.batch) []
  |> Table_logger.add_column ~label:"Samples" ~extract_and_format:(fun w r -> Table_logger.format_int w r.total_samples)
  |> Table_logger.add_column ~label:"Nodes Est" ~extract_and_format:(fun w r -> Table_logger.format_float w r.nodes_est)
  |> Table_logger.add_column ~label:"Fails Est" ~extract_and_format:(fun w r -> Table_logger.format_float w r.fails_est)
  |> Table_logger.add_column ~label:"Sols Est" ~extract_and_format:(fun w r -> Table_logger.format_float w r.sols_est)
  |> Table_logger.add_column ~label:"Found" ~extract_and_format:(fun w r -> Table_logger.format_int w r.found)
  |> Table_logger.add_column ~label:"Materialized" ~extract_and_format:(fun w r -> Table_logger.format_int w r.materialized)
  |> Table_logger.add_column ~label:"Pruned" ~extract_and_format:(fun w r -> Table_logger.format_int w r.pruned)
  |> Table_logger.add_column ~label:"Net Nodes" ~extract_and_format:(fun w r -> Table_logger.format_int w r.net_nodes)
  |> Table_logger.add_column ~label:"%Under" ~extract_and_format:(fun w r -> Table_logger.format_percent w r.undersampled_pct)
  |> Table_logger.add_column ~label:"%Done" ~extract_and_format:(fun w r -> Table_logger.format_percent w r.progress_pct)
  |> Table_logger.add_column ~label:"Elapsed" ~extract_and_format:(fun w r -> Table_logger.format_string_right w r.elapsed)
  |> Table_logger.add_column ~label:"ETA" ~extract_and_format:(fun w r -> Table_logger.format_string_right w r.eta)

let () =
  if Array.length Sys.argv <> 2 then begin
    Printf.eprintf "Usage: %s <puzzle-file>\n" Sys.argv.(0);
    exit 1
  end;
  let puzzle_file = Sys.argv.(1) in
  let solutions_file = solutions_file_path puzzle_file in
  let out_ch = open_out solutions_file in
  let found_count = ref 0 in
  let on_solution solution =
    incr found_count;
    Printf.fprintf out_ch "Solution %d:\n%s\n---\n%!" !found_count (Pentominos.Board.to_string solution)
  in

  let puzzle =
    In_channel.with_open_text puzzle_file Puzzle.load
  in
  let searchspace = Puzzle.solve puzzle in
  let (selector, get_stats) = Stochastic_estimator.gradual_braking_memory_aware_selector ~threshold:100_000. ~memory_pressure:nodes_in_memory in
  (*let selector = Stochastic_estimator.hard_braking_memory_aware_selector ~threshold:0.3 ~memory_pressure:(fun _ -> Searchspace.heap_usage_mb ()) in*)
  (* let selector = Stochastic_estimator.probabilistic_undersampled_selector in *)
  let estimator = Stochastic_estimator.create ~on_solution ~selector searchspace in

  let table = make_table () in
  Table_logger.print_header table;

  let batch_count = ref 0 in
  let total_samples = ref 0 in
  Stochastic_progress.run_with_progress ~batch_size ~on_progress:(fun p ->
    incr batch_count;
    total_samples := !total_samples + batch_size;
    let est = Stochastic_estimator.estimates estimator in
    let net_nodes = p.materialized_nodes - p.pruned_nodes in
    let stats = get_stats () in
    let batch_total = stats.undersampled_count + stats.greedy_count in
    let undersampled_pct =
      if batch_total > 0 then Float.of_int stats.undersampled_count /. Float.of_int batch_total *. 100.0
      else 0.0 in
    let row = {
      batch = !batch_count;
      total_samples = !total_samples;
      nodes_est = est.nodes;
      fails_est = est.fails;
      sols_est = est.solutions;
      found = !found_count;
      materialized = p.materialized_nodes;
      pruned = p.pruned_nodes;
      net_nodes;
      undersampled_pct;
      progress_pct = p.progress_percent;
      elapsed = Stochastic_progress.format_time p.elapsed_seconds;
      eta = Stochastic_progress.format_time p.estimated_remaining_seconds;
    } in
    Table_logger.print_row table row
  ) estimator;

  (* Final report *)
  let est = Stochastic_estimator.estimates estimator in
  Printf.printf "\nDone! Final estimates: nodes=%.0e, fails=%.0e, solutions=%.1f\n" est.nodes est.fails est.solutions;

  close_out out_ch;
