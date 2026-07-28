# Task 2: Progress Monitoring & Logging

## Goal

Add a progress monitoring system that provides periodic updates during estimation, including:
- Absolute numbers for solutions, fails, and materialized nodes
- Progress percentage based on materialized nodes vs estimated total nodes
- Estimated time remaining (ETR) with human-readable formatting
- **Auto-save trigger** - the progress reporter can optionally trigger periodic state saves

## Background

The current estimator has no visibility into progress during long-running estimations. Users need to know:
- How far along the estimation is (percentage complete)
- Whether the process is making progress or stuck
- How much longer to wait

The `sample` function performs batches of samples but provides no intermediate feedback. We need a way to query and report progress at regular intervals.

## Acceptance Criteria

### 2.1 Progress Data Structure

1. **Progress record contains all required fields**:
   - `elapsed_seconds : float` - seconds since estimation started
   - `total_nodes_estimate : float` - estimated total nodes in search space
   - `fails_estimate : float` - estimated number of failures
   - `solutions_estimate : float` - estimated number of solutions  
   - `materialized_nodes : int` - actual nodes materialized so far
   - `progress_percent : float` - 0.0 to 100.0, based on materialized/estimated ratio
   - `estimated_remaining_seconds : float` - ETR, infinity if progress is 0%
   - `samples_per_second : float` - throughput metric

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

### 2.3 Progress Reporter Integration

6. **Reporter is optional** - existing code works without progress reporting
7. **Reporter can be called externally** - user controls when to query/report
8. **Reporter is thread-safe** (if applicable) - no race conditions on shared state

## Implementation Process (TDD)

### Phase 1: Progress Data Structure Tests

```ocaml
let%test_module "progress_data" = (module struct
  (* Test: progress record has all required fields *)
  let test_progress_record_fields () = ...
  
  (* Test: progress_percent calculation for known values *)
  let test_progress_percent_calculation () = 
    (* materialized=50, estimate=100 => 50.0% *)
    (* materialized=0, estimate=100 => 0.0% *)
    (* materialized=100, estimate=100 => 100.0% *)
    ...
  
  (* Test: ETR calculation *)
  let test_eta_calculation () =
    (* elapsed=10s, progress=50% => ETR=10s *)
    (* elapsed=60s, progress=25% => ETR=180s *)
    (* elapsed=any, progress=0% => infinity *)
    ...
end)
```

### Phase 2: Time Formatting Tests

```ocaml
let%test_module "time_formatting" = (module struct
  (* Test: format_seconds *)
  let test_format_small_seconds () = 
    assert_equal (format_time 5.0) "5 s"
  
  (* Test: format_minutes_and_seconds *)
  let test_format_minutes () = 
    assert_equal (format_time 142.0) "2 min 22 s"
  
  (* Test: format_hours *)
  let test_format_hours () = 
    assert_equal (format_time 7530.0) "2 h 5 min 30 s"
  
  (* Test: format_days *)
  let test_format_days () = 
    assert_equal (format_time 150125.0) "1 day, 17 h 42 min 5 s"
  
  (* Test: edge cases *)
  let test_format_zero () = assert_equal (format_time 0.0) "0 s"
  let test_format_just_under_minute () = assert_equal (format_time 59.0) "59 s"
end)
```

### Phase 3: Reporter Integration Tests

```ocaml
let%test_module "progress_reporter" = (module struct
  (* Test: get_progress returns valid data immediately *)
  let test_initial_progress () = ...
  
  (* Test: progress increases with sampling *)
  let test_progress_increases () = ...
  
  (* Test: progress reaches 100% when complete *)
  let test_progress_at_completion () = ...
  
  (* Test: reporter with callback receives updates *)
  let test_reporter_with_callback () = ...
  
  (* Test: periodic reporting interval works *)
  let test_periodic_reporting () = ...
end)
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
  samples_per_second : float;
}

type progress_reporter = {
  interval_seconds : float;        (* How often to report *)
  on_progress : progress -> unit;  (* Callback for each report *)
}

val create_with_progress :
  ?selector:'a child_selector ->
  ?on_solution:('a -> unit) ->
  ?reporter:progress_reporter ->
  'a Searchspace.t -> 'a t

val get_progress : 'a t -> progress
val format_time : float -> string  (* Human-readable duration *)
```

### Progress Percentage Rationale

Using `materialized_nodes / total_nodes_estimate` as the progress metric makes sense because:
- It represents the fraction of the search space we've actually visited
- When this reaches 100%, we've fully explored the space (isCompleted = true)
- It's a natural measure of "how much work is done"

### ETA Rationale

Linear extrapolation from current rate:
- Simple and intuitive
- Works well when sampling rate is stable
- May be inaccurate early on (before estimates stabilize) - this is acceptable

## Files to Modify

- `searchspace/stochastic_estimator.ml` - implementation
- `searchspace/stochastic_estimator.mli` - interface update
