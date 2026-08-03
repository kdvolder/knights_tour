# Task 9: CLI Integration

## Goal

Create a command-line executable for running stochastic estimation on pentomino puzzles, based on `estimate_polyomino.ml`. The CLI integrates all estimator features: solution callbacks, progress reporting via table logger, auto-save with crash resistance, and resume capability.

## Background

`estimate_polyomino.ml` already demonstrates the core workflow:
1. Reads a puzzle file (passed as argument)
2. Creates a search space from the puzzle
3. Runs stochastic estimation with `gradual_braking_memory_aware_selector`
4. Reports progress via `Table_logger` with batched updates
5. Writes solutions to a timestamped file

We need to enhance this into a production-ready CLI with:
- **Auto-save**: Periodic state saves that survive crashes via atomic swap
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

### 9.2 Auto-Save with Crash Resistance

3. **Autosave file location**:
   - Autosave file is named `estimate-state.backup` in the puzzle file's directory
   - Written using atomic swap strategy (write to `.tmp`, then `Sys.rename`)
   - On crash mid-write: old backup remains intact (atomic rename guarantees)

4. **Auto-save interval**:
   - Default: every 60 seconds
   - Configurable via `--save-interval SECS` flag

5. **Resume on startup**:
   - On startup, check if `estimate-state.backup` exists in puzzle directory
   - If found: load state and resume estimation (print "Resuming from saved state")
   - If not found: start fresh (normal behavior)
   - `--no-resume` flag overrides auto-detection: always start fresh

6. **Orphan cleanup**:
   - On startup, clean any leftover `.tmp` files from crashed writes

### 9.3 Solutions File

7. **Solutions file location**:
   - Same as current `estimate_polyomino.ml`: timestamped file in puzzle directory
   - Format: `solutions-YYYY-MM-DD-HH-mm.txt`
   - When resuming, append to existing solutions file (don't overwrite)

### 9.4 Console Output

8. **Progress table**:
   - Same `Table_logger` format as current `estimate_polyomino.ml`
   - Header printed every 30 batches, rows after each batch
   - Final report on completion

9. **Startup/resume messages**:
   - "Starting fresh estimation" or "Resuming from saved state (mat=N, samples=S)"
   - Clear indication of what mode is active

### 9.5 Command-Line Interface

10. **CLI flags**:
    ```
    estimate_polyomino <puzzle-file> [options]
    
    Options:
      --no-resume           Always start fresh, ignore saved state
      --save-interval SECS  Auto-save interval in seconds (default: 60)
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

### Phase 2: Auto-Save Functions

Implement atomic swap save and load with resume detection:

```ocaml
let cleanup_orphans puzzle_file =
  (* Remove any leftover .tmp files *)
  let tmp = autosave_tmp_path puzzle_file in
  if Sys.file_exists tmp then Sys.remove tmp

let save_autosave est puzzle_file =
  let tmp = autosave_tmp_path puzzle_file in
  Stochastic_estimator.save_state tmp est;
  (* Atomic rename: backup may be overwritten or preserved *)
  let backup = autosave_path puzzle_file in
  if Sys.file_exists backup then Sys.remove backup;
  Sys.rename tmp backup

let load_autosave_if_exists puzzle_file search_space : 'a Stochastic_estimator.t * bool =
  let backup = autosave_path puzzle_file in
  if Sys.file_exists backup then (
    let est = Stochastic_estimator.load_state search_space backup in
    (est, true)   (* returned: estimator, was_resumed *)
  ) else (
    (None, false)
  )
```

### Phase 3: Main CLI with Resume Logic

Restructure `estimate_polyomino.ml` main function:

```ocaml
let () =
  (* Parse args *)
  let puzzle_file = 
    match Sys.argv with
    | [|_; pf|] -> pf
    | _ -> eprintf "Usage: %s <puzzle-file>\n"; exit 1
  in
  
  let no_resume = Array.exists (fun a -> a = "--no-resume") Sys.argv in
  let save_interval = 
    match List.filter (fun a -> String.starts_with ~prefix:"--save-interval" a) 
      (Array.to_list Sys.argv) with
    | [] -> 60.0
    | [s] -> float_of_string (String.drop_prefix s 16)
    | _ -> 60.0
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
  
  (* Check for saved state *)
  cleanup_orphans puzzle_file;
  
  let solutions_file = solutions_path puzzle_file in
  let on_solution solution found_count_ref =
    incr found_count_ref;
    Printf.fprintf !out_ch "Solution %d:\n%s\n---\n%!" 
      !found_count_ref (Pentominos.Board.to_string solution)
  in
  
  let est, was_resumed =
    if no_resume then (
      Printf.printf "Starting fresh estimation\n%!";
      let est = create_estimator searchspace in
      (est, false)
    ) else if Sys.file_exists (autosave_path puzzle_file) then (
      Printf.printf "Resuming from saved state\n%!";
      let est = Stochastic_estimator.load_state searchspace (autosave_path puzzle_file) in
      (est, true)
    ) else (
      Printf.printf "Starting fresh estimation\n%!";
      let est = create_estimator searchspace in
      (est, false)
    )
  in
  
  (* Open solutions file — append if resuming *)
  let out_ch = 
    if was_resumed then open_out_gen [Open_append; Open_text] 0o644 solutions_file
    else open_out solutions_file
  in
  
  (* Run estimation with periodic auto-save *)
  let save_timer = ref 0.0 in
  let start_time = Unix.gettimeofday () in
  
  Stochastic_progress.run_with_progress ~batch_size ~on_progress:(fun p ->
    (* Auto-save check *)
    let elapsed = Unix.gettimeofday () -. start_time in
    if elapsed -. !save_timer >= save_interval then (
      save_autosave est puzzle_file;
      save_timer := elapsed;
      Printf.printf "[Auto-save at %.0fs]\n%!" elapsed
    );
    
    (* Progress table update (existing logic) *)
    ...
  ) est;
  
  (* Final save on completion *)
  save_autosave est puzzle_file;
  
  close_out out_ch
```

### Phase 4: Integration Testing

Test the full workflow:
1. Run with a small puzzle, let it run for a few batches
2. Kill the process (simulate crash)
3. Re-run with same puzzle — should resume from autosave
4. Verify solutions continue without duplicates

## Design Decisions

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

### Auto-save interval: 60 seconds default
- Long-running estimations (hours to days) need periodic saves
- 60 seconds balances crash recovery window vs. I/O overhead
- Configurable via `--save-interval` for different use cases

### Atomic swap strategy (from Task 5)
- Write to `.tmp`, then `Sys.rename` to `.backup`
- Crash during write: `.tmp` orphaned, `.backup` intact
- Crash during rename: atomicity guarantees either old or new `.backup`
- Cleanup orphaned `.tmp` on startup

## Files to Modify

- `estimate_polyomino.ml` — add auto-save, resume logic, CLI parsing
  - Keep existing `Table_logger` progress reporting unchanged
  - Keep existing solution callback and file writing unchanged
  - Add auto-save before/after progress loop
  - Add resume detection at startup

## Dependencies

This task depends on:
- **Task 3** (State Serialization) — `save_state` / `load_state` functions
- **Task 4** (State Deserialization) — lazy reconstruction, error handling
- **Task 1** (Solution Callback) — `on_solution` parameter
- **Stochastic_progress** — `run_with_progress` for batched execution
