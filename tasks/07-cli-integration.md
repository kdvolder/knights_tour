# Task 7: CLI Integration

## Goal

Create a new executable (`knight_solve` or similar) that provides a command-line interface for running stochastic estimation, similar to `solve_file.ml`. It reads the problem definition from the current directory and saves logs, solutions, and state in that same directory.

## Background

The `solve_file.ml` executable demonstrates the pattern:
1. Reads a puzzle from `polymino-puzzle.txt` in current directory
2. Runs the solver with progress reporting
3. Outputs solutions and stats

We need a similar executable for stochastic estimation that:
- Reads the same problem format
- Uses the stochastic estimator instead of breadth-first search
- Integrates all features: solution callbacks, progress reporting, auto-save
- Saves state for resume capability

## Acceptance Criteria

### 7.1 Problem Loading

1. **Reads problem from current directory**:
   - Same format as `solve_file.ml` (reads `polymino-puzzle.txt`)
   - Error message if file not found or invalid format

2. **Works from any directory**:
   - Problem is loaded relative to current working directory
   - Output files are also written to current working directory

### 7.2 Command-Line Interface

3. **CLI flags for all features**:
   ```
   knight_solve [options]
   
   Options:
     --resume                Resume from saved state if available
     --save-interval SECS    Auto-save interval in seconds (default: 60)
     --progress-interval SECS Progress report interval in seconds (default: 10)
     --selector TYPE         Selector type: undersampled|uniform|probabilistic (default: undersampled)
     --prune                 Enable automatic pruning of completed branches
     --help                  Show help message
   ```

4. **Default behavior is sensible**:
   - Without `--resume`: start fresh estimation
   - With `--resume`: load state from `state.json` if it exists, otherwise start fresh
   - Default save interval: 60 seconds
   - Default progress interval: 10 seconds

### 7.3 Output Files

5. **Solutions file**:
   - `solutions.txt` in current directory
   - One solution per section, numbered sequentially
   - Format similar to `solve_file.ml`'s output

6. **Progress log**:
   - `progress.csv` in current directory
   - CSV with columns: timestamp, elapsed, nodes_estimate, fails_estimate, solutions_estimate, materialized_nodes, progress_percent, eta
   - Appended to on each progress report

7. **State file**:
   - `state.json` (or similar) in current directory
   - Saved periodically per `--save-interval`
   - Used for resume via `--resume`

### 7.4 Console Output

8. **Progress displayed to stdout**:
   - Human-readable progress updates at `--progress-interval`
   - Format: `[12.5%] 1234/9876 nodes | ETA: 2h 15m | Solutions: 3`
   - Updates in-place (carriage return) or line-by-line

9. **Solutions printed as found**:
   - When `--print-solutions` (or always), print each solution as it's found
   - Solution number and board representation

### 7.5 Resume Workflow

10. **Resume from saved state**:
    - `--resume` loads state from file
    - Continues sampling where it left off
    - Progress report shows resumed progress (not reset to 0%)
    - Solutions found after resume are appended to solutions file

11. **Graceful handling of missing state**:
    - `--resume` with no saved state: start fresh, print warning

## Implementation Process (TDD)

### Phase 1: CLI Argument Parsing Tests

```ocaml
let%test_module "cli_parsing" = (module struct
  (* Test: parse --resume flag *)
  let test_parse_resume () = 
    assert_equal (parse_args ["--resume"]) { resume=true; ... }
  
  (* Test: parse --save-interval *)
  let test_parse_save_interval () = 
    assert_equal (parse_args ["--save-interval"; "30"]) { save_interval=30.0; ... }
  
  (* Test: parse --selector *)
  let test_parse_selector () = 
    assert_equal (parse_args ["--selector"; "uniform"]) { selector=Uniform; ... }
  
  (* Test: default values when no flags *)
  let test_default_values () = 
    assert_equal (parse_args []) { resume=false; save_interval=60.0; ... }
  
  (* Test: invalid flag produces error *)
  let test_invalid_flag () = 
    assert_raises (Arg.Bad _) (fun () -> parse_args ["--invalid"])
end)
```

### Phase 2: Problem Loading Tests

```ocaml
let%test_module "problem_loading" = (module struct
  (* Test: load valid puzzle file *)
  let test_load_valid_puzzle () = 
    (* Create temp puzzle file, load it *)
    (* Verify puzzle is correctly parsed *)
    ...
  
  (* Test: missing file produces error *)
  let test_missing_file () = 
    (* Try to load non-existent file *)
    (* Should print error and exit gracefully *)
    ...
  
  (* Test: invalid puzzle format produces error *)
  let test_invalid_format () = 
    (* Create file with bad content *)
    (* Should print error and exit gracefully *)
    ...
end)
```

### Phase 3: Output File Tests

```ocaml
let%test_module "output_files" = (module struct
  (* Test: solutions file is created and written *)
  let test_solutions_file () = 
    (* Run estimator with callback that writes solutions *)
    (* Verify solutions.txt exists and has correct content *)
    ...
  
  (* Test: progress CSV is created with correct columns *)
  let test_progress_csv () = 
    (* Run estimator with progress reporter *)
    (* Verify progress.csv has header and data rows *)
    ...
  
  (* Test: state file is created on auto-save *)
  let test_state_file () = 
    (* Run estimator with auto-save enabled *)
    (* Wait for save interval *)
    (* Verify state file exists and is valid JSON *)
    ...
end)
```

### Phase 4: Integration Tests

```ocaml
let%test_module "full_integration" = (module struct
  (* Test: complete workflow - run, save, resume *)
  let test_full_workflow () = 
    (* Create puzzle file *)
    (* Run: knight_solve --save-interval 1 *)
    (* Kill after some time (simulate crash) *)
    (* Run: knight_solve --resume *)
    (* Verify solutions continue, no duplicates *)
    ...
  
  (* Test: all features together *)
  let test_all_features () = 
    (* Run with --resume --save-interval 5 --progress-interval 2 --selector uniform *)
    (* Verify all features work together *)
    ...
end)
```

## Design Notes

### Executable Structure

```ocaml
(* knight_solve.ml *)
open Searchspace

let () =
  let args = parse_args Sys.argv in
  
  (* Load problem *)
  let puzzle = load_puzzle "polymino-puzzle.txt" in
  
  (* Create search space from puzzle *)
  let search_space = create_search_space puzzle in
  
  (* Load saved state if resuming *)
  let est = 
    match args.resume with
    | true -> load_state_if_exists "state.json" search_space
    | false -> create search_space
  in
  
  (* Set up solution callback *)
  let sol_file = open_out "solutions.txt" in
  let on_solution board = 
    output_string sol_file (Board.to_string board);
    flush sol_file
  in
  
  (* Set up progress reporter *)
  let progress_file = open_out "progress.csv" in
  let on_progress p = 
    output_string progress_file (format_csv_line p);
    flush progress_file;
    print_progress_to_console p
  in
  
  (* Create estimator with all features *)
  let est = { 
    est with 
    on_solution;
    progress_reporter = Some { interval=args.progress_interval; on_progress };
    auto_save = Some { interval=args.save_interval; file="state.json" }
  } in
  
  (* Run estimation *)
  let rec loop () = 
    if not (sample 100 est) then (
      Unix.sleepf args.progress_interval;
      loop ()
    )
  in
  loop ();
  
  (* Cleanup *)
  close_out sol_file;
  close_out progress_file
```

### File Format Decisions

**State file**: JSON format (human-readable, easy to debug)
```json
{
  "version": 1,
  "materialized_nodes": [...],
  "root_samples": 5000,
  "is_completed": false
}
```

**Progress CSV**: Standard CSV with header row
```csv
timestamp,elapsed,nodes_estimate,fails_estimate,solutions_estimate,materialized_nodes,progress_percent,eta_seconds
1690000000.5,45.2,15000.3,8000.1,1200.5,7500,50.0,45.2
```

**Solutions file**: Human-readable board representation
```
Solution #1:
+---+---+---+
|   |   |   |
+---+---+---+
...

Solution #2:
...
```

### Console Output Format

Use carriage return for in-place updates:
```ocaml
Printf.printf "\r[%5.1f%%] %8d/%8.0f nodes | ETA: %-12s | Solutions: %d" 
  progress_pct materialized_nodes total_estimate eta_str solution_count;
Printf.printf "%!"  (* Flush without newline *)
```

Or line-by-line for easier log parsing:
```ocaml
Printf.printf "[%s] [%5.1f%%] %8d/%8.0f nodes ETA:%-12s Sol:%d\n"
  (format_time elapsed) progress_pct materialized_nodes total_estimate eta_str solution_count
```

Recommendation: Line-by-line for log file compatibility, with optional `--no-newline` flag for terminal use.

## Files to Create

- `knight_solve.ml` - new executable
- Update root `dune` to include `knight_solve` in executables list

## Files to Modify

- `searchspace/stochastic_estimator.ml` - add save/load file functions
- `searchspace/stochastic_estimator.mli` - update interface

## Dependencies

This task depends on Tasks 1-5 being complete (callback, progress reporting, serialization, deserialization, auto-save).
