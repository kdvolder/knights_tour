open Pentominos

let format_number n =
  if n >= 1e10 || (n > 0. && n < 1.) then
    Printf.sprintf "%.5e" n
  else
    Int64.to_string (Int64.of_float n)

let batch_size = 1000

(* Runtime_events-based memory measurement - initialized once at startup *)
let re_available = ref false

let init_runtime_events () =
  re_available := Searchspace.init_runtime_events ();
  if !re_available then begin
    Searchspace.poll_runtime_events ();
    Printf.eprintf "[RuntimeEvents] Initial heap: %.1f MB\n" (Searchspace.heap_usage_mb ());
    flush stderr
  end

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
  (* Initialize Runtime_events memory measurement *)
  init_runtime_events ();
  
  let puzzle =
    In_channel.with_open_text puzzle_file Puzzle.load
  in
  let searchspace = Puzzle.solve puzzle in
  let (selector, get_stats) = Stochastic_estimator.gradual_braking_memory_aware_selector ~threshold:8000. ~memory_pressure:(fun _ -> Searchspace.heap_usage_mb ()) in
  (*let selector = Stochastic_estimator.hard_braking_memory_aware_selector ~threshold:0.3 ~memory_pressure:(fun _ -> Searchspace.heap_usage_mb ()) in*)
  (* let selector = Stochastic_estimator.probabilistic_undersampled_selector in *)
  let estimator = Stochastic_estimator.create ~on_solution ~selector searchspace in
  
  Printf.printf "%-5s | %-7s | %-12s | %-12s | %-8s | %-6s | %-12s | %-10s | %-10s | %-6s | %-8s | %-12s | %s\n%!" "Batch" "Samples" "Nodes Est" "Fails Est" "Sols Est" "Found" "Materialized" "Pruned" "Net Nodes" "%Under" "Mem.Mb" "Elapsed" "ETA";
  Printf.printf "----- | ------- | ------------ | ------------ | -------- | ------ | ------------ | ---------- | ---------- | ------ | -------- | ------------ | ------------------\n%!";
  
  let batch_count = ref 0 in
  let total_samples = ref 0 in
  Stochastic_estimator.run_with_progress ~batch_size ~on_progress:(fun p ->
    incr batch_count;
    total_samples := !total_samples + batch_size;
    let est = Stochastic_estimator.estimates estimator in
    Searchspace.poll_runtime_events ();
    let free_pct = Searchspace.heap_usage_mb () in
    let net_nodes = p.materialized_nodes - p.pruned_nodes in
    let stats = get_stats () in
    let batch_total = stats.undersampled_count + stats.greedy_count in
    let undersampled_pct =
      if batch_total > 0 then Float.of_int stats.undersampled_count /. Float.of_int batch_total *. 100.0
      else 0.0 in
    Printf.printf "%-5d | %-7d | %12s | %12s | %-8s | %-6d | %12d | %10d | %10d | %5.1f%% | %-8.1f | %-12s | %-10s\n%!"
      !batch_count !total_samples
      (format_number est.nodes) (format_number est.fails) (format_number est.solutions)
      !found_count
      p.materialized_nodes
      p.pruned_nodes
      net_nodes
      undersampled_pct
      free_pct
      (Stochastic_estimator.format_time p.elapsed_seconds)
      (Stochastic_estimator.format_time p.estimated_remaining_seconds)
  ) estimator;
  
  (* Final report *)
  let est = Stochastic_estimator.estimates estimator in
  Printf.printf "\nDone! Final estimates: nodes=%.0e, fails=%.0e, solutions=%.1f\n" est.nodes est.fails est.solutions;
  

  close_out out_ch;
