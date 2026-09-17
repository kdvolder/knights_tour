# Task 9: CLI Integration

## Goal

Create a command-line executable for running stochastic estimation on pentomino puzzles, based on `estimate_polyomino.ml`. The CLI integrates all estimator features: solution callbacks, progress reporting via table logger, adaptive auto-save with crash resistance, graceful CTRL-C shutdown, and resume capability.

## Background

`estimate_polyomino.ml` already demonstrates the core workflow:
1. Reads a puzzle file (passed as argument)
2. Creates a search space from the puzzle
3. Runs stochastic estimation with `gradual_braking_memory_aware_selector`
4. Reports progress via `Table_logger` with batched updates
5. Writes solutions to a timestamped file

We need to enhance this into a production-ready CLI with:
- **Adaptive auto-save**: Periodic state saves that survive crashes via atomic swap, with interval dynamically adjusted based on save time
- **Graceful CTRL-C**: Clean shutdown — wait for batch, save backup, exit
- **Resume**: Detect saved state on startup and continue where left off
- **File organization**: All activity files (solutions, autosave) live relative to the puzzle file

## Acceptance Criteria

### 9.1 Puzzle File Loading

1. **Puzzle file as argument**:
   ```
   estimate_polyomino <puzzle-file> [options]
   ```
   - Puzzle file path is the first positional argument (required)
   - Error message and exit if no argument or file not found

2. **Puzzle directory as working root**:
   - All output files are saved relative to the puzzle file's directory (not CWD)
   - This allows running from any directory while keeping all artifacts with the puzzle

### 9.2 Adaptive Auto-Save with Crash Resistance

3. **Autosave file location**:
   - Autosave file is named `estimate-state.backup` in the puzzle file's directory
   - Written using atomic swap strategy (write to `.tmp`, then `Sys.rename`)
   - On crash mid-write: old backup remains intact (atomic rename guarantees)

4. **Adaptive save interval**:
   - Save only happens between batches (never during a sampling batch)
   - Interval is dynamically computed: `next_interval = max(min_interval, save_time * 1000)`
   - `min_interval` is configurable via `--save-interval SECS` (default: 300 seconds / 5 minutes)
   - This guarantees we never spend more than ~1/1000 of total solver time on saves
   - For a tree with 500K nodes that takes 10 seconds to save: next interval = max(300, 10*1000) = 10000s (~2.8 hours)
   - For a small tree that saves in 0.1s: next interval = max(300, 0.1*1000) = 300s (5 minutes)
   - The first save after resume uses the configured min_interval

5. **Resume on startup**:
   - On startup, check if `estimate-state.backup` exists in puzzle directory
   - If found: load state and resume estimation (print "Resuming from saved state")
   - If not found: start fresh (normal behavior)
   - `--no-resume` flag overrides auto-detection: always start fresh

6. **Orphan cleanup**:
   - On startup, clean any leftover `.tmp` files from crashed writes

### 9.3 Graceful CTRL-C Handling

7. **SIGINT handler**:
   - Install a `Sys.trap Sys.sigint` handler at startup
   - When CTRL-C is received:
     a. Print message: "Interrupted — saving state and shutting down..."
     b. Set a `shutting_down` flag (ref bool) to true
     c. Wait for current batch to complete (do NOT interrupt mid-batch)
     d. After batch completes, check `shutting_down` flag
     e. If set: save backup, print "State saved. Exiting.", exit cleanly

8. **No partial saves on CTRL-C**:
   - The handler only sets a flag; actual save happens in the main loop after batch completion
   - This ensures we never save mid-batch (which would be inconsistent)

### 9.4 Solutions File

9. **Solutions file location**:
   - Same as current `estimate_polyomino.ml`: timestamped file in puzzle directory
   - Format: `solutions-YYYY-MM-DD-HH-mm.txt`
   - When resuming, append to existing solutions file (don't overwrite)

### 9.5 Console Output

10. **Progress table**:
    - Same `Table_logger` format as current `estimate_polyomino.ml`
    - Header printed every 30 batches, rows after each batch
    - Final report on completion

11. **Startup/resume messages**:
    - "Starting fresh estimation" or "Resuming from saved state (mat=N, samples=S)"
    - Clear indication of what mode is active

12. **Auto-save progress messages**:
    - Print "[Auto-save at Xs (interval: Ys)]" after each save, showing elapsed time and next interval

13. **CTRL-C acknowledgment**:
    - "Interrupted — saving state and shutting down..." (immediate)
    - "[Auto-save at Xs] State saved. Exiting." (after save completes)

### 9.6 Command-Line Interface

14. **CLI flags**:
    ```
    estimate_polyomino <puzzle-file> [options]
    
    Options:
      --no-resume           Always start fresh, ignore saved state
      --save-interval SECS  Minimum auto-save interval in seconds (default: 300)
      --batch-size N        Samples per batch (default: 1000)
      --help                Show help and exit
    ```

## Implementation Process

### Phase 1: File Path Utilities

Add file path helpers relative to puzzle file directory:

```ocaml
let puzzle_dir = Filename.dirname puzzle_file

let solutions_path puzzle_file =
  (* Current implementation: timestamped file in puzzle dir *)
  let today = Unix.localtime (Unix.time ()) in
  let timestamp = Printf.sprintf "%04d-%02d-%02d-%02d-%02d"
    (today.tm_year + 1900) (today.tm_mon + 1) today.tm_mday
    today.tm_hour today.tm_min in
  Filename.concat puzzle_dir (Printf.sprintf "solutions-%s.txt" timestamp)

let autosave_path puzzle_file =
  Filename.concat puzzle_dir "estimate-state.backup"

let autosave_tmp_path puzzle_file =
  Filename.concat puzzle_dir "estimate-state.tmp"
```

### Phase 2: Auto-Save Functions with Adaptive Interval

Implement atomic swap save, adaptive interval tracking, and CTRL-C handling:

```ocaml
let cleanup_orphans puzzle_file =
  (* Remove any leftover .tmp files *)
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

let load_autosave_if_exists puzzle_file search_space : 'a Stochastic_estimator.t * bool =
  let backup = autosave_path puzzle_file in
  if Sys.file_exists backup then (
    let est = Stochastic_estimator.load_state search_space backup in
    (est, true)   (* returned: estimator, was_resumed *)
  ) else (
    (create search_space, false)
  )

(* Adaptive interval state *)
type auto_save_state = {
  min_interval : float;       (* Configured minimum, e.g., 300s *)
  mutable next_save_time : float;  (* Absolute time of next allowed save *)
}

let make_auto_save_state min_interval = {
  min_interval;
  next_save_time = Unix.gettimeofday () +. min_interval;
}

let try_autosave auto_save_state est puzzle_file =
  let now = Unix.gettimeofday () in
  if now >= auto_save_state.next_save_time then (
    let save_time = save_autosave est puzzle_file in
    (* Compute next interval: max(min_interval, save_time * 1000) *)
    let dynamic_interval = save_time *. 1000. in
    let next_interval = max auto_save_state.min_interval dynamic_interval in
    auto_save_state.next_save_time <- now +. next_interval;
    Printf.printf "[Auto-save at %.0fs (next in %.0fs, save took %.1fs)]\n%!"
      now next_interval save_time;
    true   (* saved *)
  ) else false   (* too soon, skip *)
```

### Phase 3: Main CLI with Resume Logic and CTRL-C Handler

Restructure `estimate_polyomino.ml` main function:

```ocaml
let () =
  (* Parse args *)
  let puzzle_file = 
    match Sys.argv with
    | [|_; pf|] -> pf
    | _ -> eprintf "Usage: %s <puzzle-file> [--no-resume] [--save-interval SECS] [--batch-size N]\n"; exit 1
  in
  
  let no_resume = Array.exists (fun a -> a = "--no-resume") Sys.argv in
  let min_save_interval = 
    match List.filter (fun a -> String.starts_with ~prefix:"--save-interval" a) 
      (Array.to_list Sys.argv) with
    | [] -> 300.0   (* default: 5 minutes *)
    | [s] -> float_of_string (String.drop_prefix s 16)
    | _ -> 300.0
  in
  let batch_size = 
    match List.filter (fun a -> String.starts_with ~prefix:"--batch-size" a)
      (Array.to_list Sys.argv) with
    | [] -> 1000
    | [s] -> int_of_string (String.drop_prefix s 12)
    | _ -> 1000
  in
  
  (* Load puzzle *)
  let puzzle = In_channel.with_open_text puzzle_file Puzzle.load in
  let searchspace = Puzzle.solve puzzle in
  
  (* Setup auto-save state *)
  let auto_save_state = make_auto_save_state min_save_interval in
  
  (* Setup CTRL-C handler *)
  let shutting_down = ref false in
  Sys.trap Sys.sigint (fun _ -> 
    Printf.printf "\nInterrupted — saving state and shutting down...\n%!";
    shutting_down := true
  );
  
  (* Check for saved state *)
  cleanup_orphans puzzle_file;
  
  let est, was_resumed = load_autosave_if_exists puzzle_file searchspace in
  
  (* Print startup message *)
  if was_resumed then (
    let est_info = Stochastic_estimator.estimates est in
    Printf.printf "Resuming from saved state (mat=%d, nodes=%.0f)\n%!"
      est_info.materialized_nodes est_info.nodes;
  ) else (
    Printf.printf "Starting fresh estimation\n%!";
  );
  
  (* Setup solution callback *)
  let solutions_file = solutions_path puzzle_file in
  let found_count = ref 0 in
  let out_ch = 
    if was_resumed then open_out_gen [Open_append; Open_text] 0o644 solutions_file
    else open_out solutions_file
  in
  let on_solution solution =
    incr found_count;
    Printf.fprintf out_ch "Solution %d:\n%s\n---\n%!" 
      !found_count (Pentominos.Board.to_string solution)
  in
  
  (* Create estimator with on_solution *)
  let est = Stochastic_estimator.create ~on_solution ~selector:gradual_braking_selector searchspace in
  
  (* Run estimation with periodic auto-save *)
  Stochastic_progress.run_with_progress ~batch_size ~on_progress:(fun p ->
    (* Auto-save check (only between batches) *)
    ignore (try_autosave auto_save_state est puzzle_file);
    
    (* Check for CTRL-C shutdown *)
    if !shutting_down then (
      ignore (try_autosave auto_save_state est puzzle_file);
      Printf.printf "[Auto-save complete] State saved. Exiting.\n%!";
      close_out out_ch;
      exit 0
    );
    
    (* Progress table update (existing logic) *)
    ...
  ) est;
  
  (* Final save on completion *)
  ignore (try_autosave auto_save_state est puzzle_file);
  
  close_out out_ch;
  
  (* Final report *)
  let est = Stochastic_estimator.estimates est in
  Printf.printf "\nDone! Final estimates: nodes=%.0e, fails=%.0e, solutions=%.1f\n" 
    est.nodes est.fails est.solutions
```

### Phase 4: Integration Testing

Test the full workflow:
1. Run with a small puzzle, let it run for a few batches
2. Kill the process (simulate crash) — verify autosave exists
3. Re-run with same puzzle — should resume from autosave
4. Verify solutions continue without duplicates
5. Test CTRL-C: run, send SIGINT, verify clean shutdown with backup saved

## Design Decisions

### Adaptive interval: `max(min_interval, save_time * 1000)`
- **Why multiply by 1000?**: Guarantees saves never consume more than ~0.1% of total solver time
- **Why max with min_interval?**: Prevents saves from happening too frequently on small trees (e.g., 0.01s save → 10s interval would be annoying)
- **Default min_interval: 300 seconds (5 minutes)**: For long-running estimations (hours to days), saving every 5+ minutes is fine. If a year-long run crashes, you lose at most a day of work — acceptable
- **First save after resume**: Uses configured min_interval (no prior save time to base calculation on)

### Why `estimate-state.backup` (not `.json`)?
- Task 3 serialization uses line-based text format, not JSON
- `.backup` suffix signals "this is a checkpoint that can be safely overwritten"
- Atomic swap ensures it's always valid (never half-written)

### Why auto-detect resume (not `--resume` flag)?
- Simpler UX: just run the command, it resumes if state exists
- `--no-resume` for explicit fresh start (e.g., when puzzle file changed)
- Matches common tool patterns (e.g., `make` resumes interrupted builds)

### Why append to solutions file on resume?
- Solutions found before crash are already in the file
- Appending preserves them and adds new ones
- No risk of duplicates (estimator state tracks which nodes were explored)

### Why save only between batches?
- Sampling batch is atomic — interrupting mid-batch would give inconsistent state
- `run_with_progress` invokes callback only after batch completes — perfect hook for save
- No need for threading or async saves

### CTRL-C handling: flag-based, not immediate save
- Handler only sets `shutting_down` flag — actual save happens in main loop
- This ensures we never save mid-batch (which would be inconsistent)
- The batch completion is the natural synchronization point

### Atomic swap strategy (from Task 5)
- Write to `.tmp`, then `Sys.rename` to `.backup`
- Crash during write: `.tmp` orphaned, `.backup` intact
- Crash during rename: atomicity guarantees either old or new `.backup`
- Cleanup orphaned `.tmp` on startup

## Files to Modify

- `estimate_polyomino.ml` — add auto-save, resume logic, CTRL-C handler, CLI parsing
  - Keep existing `Table_logger` progress reporting unchanged
  - Keep existing solution callback and file writing unchanged
  - Add auto-save between batches (in `on_progress` callback)
  - Add CTRL-C handler via `Sys.trap`
  - Add resume detection at startup

## Dependencies

This task depends on:
- **Task 3** (State Serialization) — `save_state` / `load_state` functions
- **Task 4** (State Deserialization) — lazy reconstruction, error handling
- **Task 1** (Solution Callback) — `on_solution` parameter
- **Stochastic_progress** — `run_with_progress` for batched execution
