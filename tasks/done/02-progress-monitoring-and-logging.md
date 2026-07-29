# Task 2: Progress Monitoring & Logging

## Goal

Add a progress reporter module that wraps around an estimator to provide periodic updates during long-running estimations. The reporter handles batching, timing, and progress computation — the estimator itself remains unchanged.

## Background

The current estimator has no visibility into progress during long-running estimations. Users need to know:
- How far along the estimation is (percentage complete)
- Whether the process is making progress or stuck
- How much longer to wait

The `sample` function performs batches of samples but provides no intermediate feedback. A progress reporter wraps the estimator, calling `sample` in a loop and reporting after each batch.

## Acceptance Criteria

### 2.1 Progress Data Structure

1. **Progress record contains all required fields**:
   - `elapsed_seconds : float` — seconds since estimation started (computed by reporter)
   - `total_nodes_estimate : float` — from `estimates est`.nodes
   - `fails_estimate : float` — from `estimates est`.fails
   - `solutions_estimate : float` — from `estimates est`.solutions  
   - `materialized_nodes : int` — from `estimates est`.materialized_nodes
   - `progress_percent : float` — 0.0 to 100.0, computed as `materialized / total * 100`
   - `estimated_remaining_seconds : float` — ETR, infinity if progress is 0%

2. **Progress percentage calculation**:
   - `progress_percent = (materialized_nodes / total_nodes_estimate) * 100.0`
   - When fully explored (isCompleted), progress = 100.0%
   - When no samples taken, progress = 0.0%

3. **ETA calculation**:
   - `estimated_remaining_seconds = elapsed_seconds * (100.0 / progress_percent) - elapsed_seconds`
   - When `progress_percent = 0`, ETR = infinity (or a sentinel value)
   - When `progress_percent >= 100`, ETR = 0

### 2.2 Human-Readable Time Formatting

4. **Format function produces parseable output**:
   - Seconds: `"5 s"` or `"42 s"`
   - Minutes: `"3 min 22 s"` (when >= 60 seconds)
   - Hours: `"2 h 15 min 30 s"` (when >= 3600 seconds)
   - Days: `"1 day, 4 h 22 min 5 s"` (when >= 86400 seconds)
   - Larger units as needed (weeks, months, years)

5. **Format is consistent and machine-parseable**:
   - Single space between number and unit
   - Comma separator for day-level boundaries
   - Abbreviations: `s`, `min`, `h`, `day`/`days`, `week`/`weeks`, etc.

### 2.3 Reporter Integration

6. **Reporter is optional** — existing code works without progress reporting
7. **Reporter wraps the estimator** — no changes to `'a t`, `sample()`, or any existing code
8. **Reporter can be called externally** — user controls when to query/report

## Implementation Process (TDD)

Each phase follows the same rhythm: write tests that **fail** (red), then implement just enough to make them **pass** (green). Move to the next phase only when all tests in the current one pass.

### Phase 1: Progress Data Structure Tests

**Goal**: Define the `progress` record type and implement `make_progress` that computes it from an estimator and elapsed time.

**Acceptance**: All tests pass — `make_progress` correctly computes progress fields from existing estimator data.

```ocaml
let%expect_test "make_progress computes correct progress" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution_a";
      return "solution_b";
      empty
    ]
  ) in
  let est = create simple_tree in
  
  (* Before any sampling *)
  let p = make_progress (Unix.gettimeofday ()) est in
  Printf.printf "Initial progress:\n";
  Printf.printf "  materialized: %d\n" p.materialized_nodes;
  Printf.printf "  progress%%: %.1f\n" p.progress_percent;
  [%expect{|\n    Initial progress:\n      materialized: 0\n      progress%: 0.0\n  |}]
  
  (* After sampling *)
  ignore (sample 10 est);
  let p = make_progress (Unix.gettimeofday ()) est in
  Printf.printf "After sampling:\n";
  Printf.printf "  materialized: %d\n" p.materialized_nodes;
  Printf.printf "  progress%%: %.1f\n" p.progress_percent;
end

let%expect_test "progress_percent calculation for known values" = begin
  (* materialized=50, estimate=100 => 50.0% *)
  (* materialized=0, estimate=100 => 0.0% *)
  (* materialized=100, estimate=100 => 100.0% *)
  ...
end

let%expect_test "ETR calculation" = begin
  (* elapsed=10s, progress=50% => ETR=10s *)
  (* elapsed=60s, progress=25% => ETR=180s *)
  (* elapsed=any, progress=0% => infinity *)
  ...
end
```

### Phase 2: Time Formatting Tests

**Goal**: Implement `format_time : float -> string` for human-readable durations.

**Acceptance**: All tests pass — `format_time` produces correct output for various time spans.

```ocaml
let%expect_test "format_time: seconds" = begin
  Printf.printf "%s\n" (format_time 0.0);
  Printf.printf "%s\n" (format_time 5.0);
  Printf.printf "%s\n" (format_time 42.0);
  [%expect{|\n    0 s\n    5 s\n    42 s\n  |}]
end

let%expect_test "format_time: minutes and seconds" = begin
  Printf.printf "%s\n" (format_time 59.0);
  Printf.printf "%s\n" (format_time 60.0);
  Printf.printf "%s\n" (format_time 142.0);
  [%expect{|\n    59 s\n    1 min\n    2 min 22 s\n  |}]
end

let%expect_test "format_time: hours" = begin
  Printf.printf "%s\n" (format_time 3600.0);
  Printf.printf "%s\n" (format_time 7530.0);
  [%expect{|\n    1 h\n    2 h 5 min 30 s\n  |}]
end

let%expect_test "format_time: days" = begin
  Printf.printf "%s\n" (format_time 86400.0);
  Printf.printf "%s\n" (format_time 150125.0);
  [%expect{|\n    1 day\n    1 day, 17 h 42 min 5 s\n  |}]
end

let%expect_test "format_time: larger units" = begin
  Printf.printf "%s\n" (format_time 31536000.0); (* ~1 year *)
  [%expect{|\n    1 year\n  |}]
end
```

### Phase 3: Reporter Integration Tests

**Goal**: Implement `run_with_progress` — a wrapper that runs batches and invokes the callback (or default stdout printer) after each batch.

**Acceptance**: All tests pass — `run_with_progress` correctly runs batches, reports progress after each one (via callback or default printer), and stops when complete.

```ocaml
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
  run_with_progress ~batch_size:3 (fun p ->
    reports := !reports @ [p.materialized_nodes]
  ) est;
  Printf.printf "Reports: %s\n" (String.concat ", " (List.map string_of_int !reports));
  [%expect{|\n    Reports: 1, 3, ...\n  |}]
end

let%expect_test "run_with_progress uses default stdout printer when no callback" = begin
  let simple_tree = Searchspace.(
    alt [
      return "solution_a";
      return "solution_b";
      empty
    ]
  ) in
  let est = create simple_tree in
  run_with_progress ~batch_size:3 est;
  [%expect{|\n    [0.0%] materialized: 1, ETA: inf\n    [50.0%] materialized: 3, ETA: 1 s\n    ...\n  |}]
end

let%expect_test "run_with_progress stops when complete" = begin
  let simple_tree = Searchspace.(
    alt [ return "sol"; empty ]
  ) in
  let est = create simple_tree in
  run_with_progress ~batch_size:5 (fun p ->
    reports := !reports @ [p.materialized_nodes]
  ) est;
  Printf.printf "Final materialized: %d\n" (List.hd (List.rev !reports));
  [%expect{|\n    Final materialized: X\n  |}]
end
```

## Design Notes

### API Design

```ocaml
type progress = {
  elapsed_seconds : float;
  total_nodes_estimate : float;
  fails_estimate : float;
  solutions_estimate : float;
  materialized_nodes : int;
  progress_percent : float;
  estimated_remaining_seconds : float;
}

val make_progress : float -> 'a t -> progress
(** [make_progress start_time est] computes a progress record from the estimator's current state. *)

val format_time : float -> string
(** [format_time seconds] produces a human-readable duration string. *)

val run_with_progress :
  ?batch_size:int ->
  ~(on_progress:(progress -> unit)) ->
  'a t -> unit
(** [run_with_progress ~batch_size ~on_progress est] runs batches of samples, invoking the callback
    after each batch with a progress record. Stops when the estimator is complete.
    The default callback prints to stdout. For custom behavior, pass your own function —
    write to a file, send over network, log as CSV — whatever the caller needs. *)
```

### Why a Wrapper, Not an Estimator Change?

- **No changes to `'a t`** — the estimator stays pure and unchanged
- **Progress is caller responsibility** — they already have `estimates est` for data, and can track time externally
- **Flexible** — callers can use `make_progress` directly for custom reporting (e.g., writing to a file, sending over network), or use `run_with_progress` for the common stdout case
- **No callback overhead** — progress is only computed when explicitly requested, not on every sample

### Progress Percentage Rationale

Using `materialized_nodes / total_nodes_estimate` as the progress metric makes sense because:
- It represents the fraction of the search space we've actually visited
- When this reaches 100%, we've fully explored the space (isCompleted = true)
- It's a natural measure of "how much work is done"

### ETA Rationale

Linear extrapolation from current rate:
- Simple and intuitive
- Works well when sampling rate is stable
- May be inaccurate early on (before estimates stabilize) — this is acceptable

### Multi-Batch Runs

The reporter tracks elapsed time from the start of `run_with_progress`, not per-batch. So if a caller runs:
```ocaml
run_with_progress ~batch_size:1000 est;  (* batch 1 *)
run_with_progress ~batch_size:1000 est;  (* batch 2 *)
```

Each call is independent — the second starts its own timer. For multi-batch runs, the caller should:
1. Record `start_time` before all calls
2. Use `make_progress start_time est` directly for cumulative reporting

## Files to Modify

- `searchspace/stochastic_estimator.ml` - implementation (new functions at end of file)
- `searchspace/stochastic_estimator.mli` - interface update

## Files to Create

- Tests are inline in `stochastic_estimator.ml` using `%expect_test`
