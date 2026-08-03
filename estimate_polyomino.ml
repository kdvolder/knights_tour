open Pentominos

let () = Random.self_init ()

(* ============================================================================ *)
(* CLI Argument Parsing                                                         *)
(* ============================================================================ *)

let parse_args () =
  (* Check for --help before parsing *)
  if Array.exists (fun a -> a = "--help") Sys.argv then begin
    Printf.printf "Usage: estimate_polyomino <puzzle-file> [--no-resume] [--save-interval SECS] [--batch-size N]\n";
    Printf.printf "\nOptions:\n";
    Printf.printf "  --no-resume           Always start fresh, ignore saved state\n";
    Printf.printf "  --save-interval SECS  Minimum auto-save interval in seconds (default: 300)\n";
    Printf.printf "  --batch-size N        Samples per batch (default: 1000)\n";
    Printf.printf "  --help                Show this help and exit\n";
    exit 0
  end;
  
  let no_resume = ref false in
  let min_save_interval = ref 300.0 in   (* default: 5 minutes *)
  let batch_size = ref 1000 in
  
  let usage_msg = 
    "Usage: estimate_polyomino <puzzle-file> [--no-resume] [--save-interval SECS] [--batch-size N]"
  in
  
  let speclist = Arg.[
    "--no-resume", Unit (fun () -> no_resume := true), " Always start fresh, ignore saved state";
    "--save-interval", Float (fun f -> min_save_interval := f), " Minimum auto-save interval in seconds (default: 300)";
    "--batch-size", Int (fun i -> batch_size := i), " Samples per batch (default: 1000)";
  ] in
  
  let puzzle_file = ref "" in
  Arg.parse speclist (fun s -> puzzle_file := s) usage_msg;
  
  if !puzzle_file = "" then begin
    Printf.eprintf "Error: no puzzle file specified\n%!";
    Arg.usage speclist (Printf.sprintf "Usage: %s <puzzle-file> [options]" Sys.argv.(0));
    exit 1
  end;
  
  (!puzzle_file, !no_resume, !min_save_interval, !batch_size)

(* ============================================================================ *)
(* File Path Utilities (relative to puzzle file directory)                     *)
(* ============================================================================ *)

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

let autosave_path puzzle_file =
  let dir = Filename.dirname puzzle_file in
  Filename.concat dir "estimate-state.backup"

let autosave_tmp_path puzzle_file =
  let dir = Filename.dirname puzzle_file in
  Filename.concat dir "estimate-state.tmp"

(* ============================================================================ *)
(* Auto-Save Functions (atomic swap strategy)                                  *)
(* ============================================================================ *)

let cleanup_orphans puzzle_file =
  (* Remove any leftover .tmp files from crashed writes *)
  let tmp = autosave_tmp_path puzzle_file in
  if Sys.file_exists tmp then Sys.remove tmp

let save_autosave est puzzle_file =
  let start_time = Unix.gettimeofday () in
  let tmp = autosave_tmp_path puzzle_file in
  Stochastic_estimator.save_state tmp est;
  let save_time = Unix.gettimeofday () -. start_time in
  (* Atomic rename: backup may be overwritten or preserved *)
  let backup = autosave_path puzzle_file in
  if Sys.file_exists backup then Sys.remove backup;
  Sys.rename tmp backup;
  save_time   (* return actual save time for adaptive interval *)

(* Adaptive interval state *)
type auto_save_state = {
  min_interval : float;       (* Configured minimum, e.g., 300s *)
  mutable next_save_time : float;  (* Absolute time of next allowed save *)
}

let make_auto_save_state min_interval = {
  min_interval;
  next_save_time = Unix.gettimeofday () +. min_interval;
}

let try_autosave auto_save_state est puzzle_file elapsed =
  let now = Unix.gettimeofday () in
  if now >= auto_save_state.next_save_time then (
    let save_time = save_autosave est puzzle_file in
    (* Compute next interval: max(min_interval, save_time * 1000) *)
    let dynamic_interval = save_time *. 1000. in
    let next_interval = max auto_save_state.min_interval dynamic_interval in
    auto_save_state.next_save_time <- now +. next_interval;
    Printf.printf "[Auto-save at %s (next in %s, save took %.1fs)]\n%!"
      (Table_logger.format_time 20 elapsed) (Table_logger.format_time 20 next_interval) save_time;
    true   (* saved *)
  ) else false   (* too soon, skip *)

(* ============================================================================ *)
(* Progress Row Type (unchanged from original)                                 *)
(* ============================================================================ *)

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
  undersampled_ratio : float; (* 0..1 *)
  completion_ratio : float;   (* 0..1 *)
  elapsed_seconds : float;
  eta_seconds : float;
}

let nodes_in_memory est =
  let estimates = Stochastic_estimator.estimates est in
  estimates.materialized_nodes - estimates.pruned_nodes |> Float.of_int

let make_table () =
  Table_logger   .add_column ~width: 7 ~label:"Batch" ~extract_and_format:(fun w r -> Table_logger.format_int w r.batch) []
  |> Table_logger.add_column ~width: 8 ~label:"Samples" ~extract_and_format:(fun w r -> Table_logger.format_int w r.total_samples)
  |> Table_logger.add_column ~width:12 ~label:"Nodes Est" ~extract_and_format:(fun w r -> Table_logger.format_float w r.nodes_est)
  |> Table_logger.add_column ~width:12 ~label:"Fails Est" ~extract_and_format:(fun w r -> Table_logger.format_float w r.fails_est)
  |> Table_logger.add_column ~width:12 ~label:"Sols Est" ~extract_and_format:(fun w r -> Table_logger.format_float w r.sols_est)
  |> Table_logger.add_column ~width: 8 ~label:"Found" ~extract_and_format:(fun w r -> Table_logger.format_int w r.found)
  |> Table_logger.add_column ~width: 8 ~label:"Materialized" ~extract_and_format:(fun w r -> Table_logger.format_int w r.materialized)
  |> Table_logger.add_column ~width: 8 ~label:"Pruned" ~extract_and_format:(fun w r -> Table_logger.format_int w r.pruned)
  |> Table_logger.add_column ~width: 8 ~label:"Net Nodes" ~extract_and_format:(fun w r -> Table_logger.format_int w r.net_nodes)
  |> Table_logger.add_column ~width: 5 ~label:"%Under" ~extract_and_format:(fun w r -> Table_logger.format_percent w r.undersampled_ratio)
  |> Table_logger.add_column ~width: 8 ~label:"%Done" ~extract_and_format:(fun w r -> Table_logger.format_percent w r.completion_ratio)
  |> Table_logger.add_column ~width:20 ~label:"Elapsed" ~extract_and_format:(fun w r -> Table_logger.format_time w r.elapsed_seconds)
  |> Table_logger.add_column ~width:20 ~label:"ETA" ~extract_and_format:(fun w r -> Table_logger.format_time w r.eta_seconds)

(* ============================================================================ *)
(* Main Function with Resume, Auto-Save, and CTRL-C Handling                   *)
(* ============================================================================ *)

let () =
  (* Parse CLI arguments *)
  let puzzle_file, no_resume, min_save_interval, batch_size = parse_args () in
  
  (* Load puzzle *)
  let puzzle = In_channel.with_open_text puzzle_file Puzzle.load in
  let searchspace = Puzzle.solve puzzle in
  
  (* Setup auto-save state *)
  let auto_save_state = make_auto_save_state min_save_interval in
  
  (* Setup CTRL-C handler *)
  let shutting_down = ref false in
  ignore (Sys.signal Sys.sigint (Sys.Signal_handle (fun _ -> 
    Printf.printf "\nInterrupted — saving state and shutting down...\n%!";
    shutting_down := true
  )));
  
  (* Check for saved state and cleanup orphans *)
  cleanup_orphans puzzle_file;
  
  let was_resumed =
    if no_resume then false
    else Sys.file_exists (autosave_path puzzle_file)
  in
  
  if was_resumed then (
    Printf.printf "Resuming from saved state\n%!";
  ) else (
    Printf.printf "Starting fresh estimation\n%!";
  );
  
  (* Setup solution callback and file *)
  let solutions_file = solutions_file_path puzzle_file in
  let found_count = ref 0 in
  let out_ch = 
    if Sys.file_exists solutions_file then open_out_gen [Open_append; Open_text] 0o644 solutions_file
    else open_out solutions_file
  in
  let on_solution solution =
    incr found_count;
    Printf.fprintf out_ch "Solution %d:\n%s\n---\n%!" !found_count (Pentominos.Board.to_string solution)
  in
  
  (* Create estimator with selector and callback *)
  let (selector, get_stats) = Stochastic_estimator.gradual_braking_memory_aware_selector ~threshold:100_000. ~memory_pressure:nodes_in_memory in
  
  let estimator =
    if was_resumed then (
      Stochastic_estimator.load_state ~selector ~on_solution searchspace (autosave_path puzzle_file)
    ) else (
      Stochastic_estimator.create ~on_solution ~selector searchspace
    )
  in
  
  (* Elapsed time tracking *)
  let start_time = Unix.gettimeofday () in
  
  (* Setup progress table *)
  let table = make_table () in
  Table_logger.print_header table;
  
  (* Run estimation with auto-save and CTRL-C handling *)
  Stochastic_progress.run_with_progress ~batch_size ~on_progress:(fun p ->
    (* Auto-save check (only between batches) *)
    ignore (try_autosave auto_save_state estimator puzzle_file p.elapsed_seconds);
    
    (* Progress table update *)
    let est = Stochastic_estimator.estimates estimator in
    let net_nodes = p.materialized_nodes - p.pruned_nodes in
    let stats = get_stats () in
    let batch_total = stats.undersampled_count + stats.greedy_count in
    let undersampled_ratio =
      if batch_total > 0 then Float.of_int stats.undersampled_count /. Float.of_int batch_total
      else 0.0 in
    let row = {
      batch = p.batch;
      total_samples = p.total_samples;
      nodes_est = est.nodes;
      fails_est = est.fails;
      sols_est = est.solutions;
      found = !found_count;
      materialized = p.materialized_nodes;
      pruned = p.pruned_nodes;
      net_nodes;
      undersampled_ratio;
      completion_ratio = p.progress_ratio;
      elapsed_seconds = p.elapsed_seconds;
      eta_seconds = p.estimated_remaining_seconds;
    } in
    Table_logger.print_row table row;
    if p.batch > 0 && p.batch mod 30 = 0 then begin
      Printf.printf "\n%!";
      Table_logger.print_header table
    end;
    not !shutting_down
  ) estimator;
  
  (* Final save on completion or shutdown *)
  let final_elapsed = Unix.gettimeofday () -. start_time in
  ignore (try_autosave auto_save_state estimator puzzle_file final_elapsed);
  
  if !shutting_down then (
    Printf.printf "[Auto-save complete] State saved. Exiting.\n%!";
    close_out out_ch;
    exit 0
  );
  
  (* Final report *)
  let est = Stochastic_estimator.estimates estimator in
  Printf.printf "\nDone! Final estimates: nodes=%.0e, fails=%.0e, solutions=%.1f\n" 
    est.nodes est.fails est.solutions;
  
  close_out out_ch
