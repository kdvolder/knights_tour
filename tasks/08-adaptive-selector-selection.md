# Task 8: Adaptive Selector Selection Based on Memory Pressure

## Goal

Implement adaptive selector selection that monitors memory pressure and switches between broad exploration (undersampled) and narrow exploration (depth-first) to ensure branches complete and can be pruned. This prevents unbounded memory growth by ensuring the estimator doesn't spread samples too thin across an ever-growing frontier.

## Background

The current stochastic estimator uses a fixed selector (default: `undersampled_selector`). This spreads samples across all unexplored branches, which is great for broad coverage but has a critical flaw:

- **Undersampled selector spreads samples thin** → individual subtrees take forever to complete
- **Nothing completes → nothing gets pruned** → memory grows unboundedly until OOM

The regular solver (`solve_file.ml`) already solves this problem with `breadth_search ~limit:memory_limit`:
- When memory is plentiful, it does **broad exploration** (BFS-like) — spreads across branches
- When memory is tight, it does **narrow exploration** (DFS-like) — follows one path to completion
- The `limit` function comes from `Searchspace.limit_on_low_memory ~max_memory_ratio:0.95`

The stochastic estimator needs the same adaptive behavior, but applied to selector selection rather than search order.

### How It Works in the Regular Solver

```ocaml
(* solve_file.ml:119 *)
let memory_limit = Searchspace.limit_on_low_memory ~max_memory_ratio:0.95

(* solve_file.ml:130 *)
Puzzle.solve puzzle 
  |> Searchspace.to_seq ~search:(Searchspace.breadth_search ~limit:memory_limit ~stack_mon)
```

The `limit_on_low_memory` function reads `/proc/meminfo` (via `memfree.ml`) and returns a function that goes from 0 to infinity as memory fills up. This limit controls `breadth_search`:
- **Low limit** → narrow exploration (DFS, follows one path to completion)
- **High limit** → broad exploration (BFS, spreads across branches)

### Selector Equivalents

| Selector | Behavior | When to Use |
|----------|----------|-------------|
| `undersampled_selector` | Broad — spreads across unexplored branches | Memory plentiful, initial exploration |
| `depth_first_selector` (or similar) | Narrow — follows one path to completion | Memory tight, need to complete branches for pruning |

## Acceptance Criteria

### 8.1 Memory Monitoring

1. **Memory pressure is monitored**:
   - Uses existing `Searchspace.memfree` / `/proc/meminfo` infrastructure
   - Checks memory ratio periodically (cached, not per-sample)

2. **Memory pressure threshold is configurable**:
   - Default: trigger narrow exploration at 80% memory usage (similar to `max_memory_ratio:0.95`)
   - Configurable via parameter

### 8.2 Adaptive Selector Selection

3. **Selector switches based on memory pressure**:
   - When memory is plentiful: use `undersampled_selector` (broad exploration)
   - When memory is tight: switch to narrow selector (DFS-like, completes branches quickly)

4. **Switching is smooth**:
   - No abrupt state changes — selector can be changed between batches via `run_with_progress`
   - Once switched to narrow, stays narrow until memory is relieved (optional hysteresis)

5. **Selector change is transparent**:
   - Existing code using `run_with_progress` works without changes (defaults to undersampled)
   - New API allows specifying adaptive mode

### 8.3 Integration with Pruning (Task 6)

6. **Narrow exploration enables pruning**:
   - When in narrow mode, branches complete faster → more nodes get pruned
   - Pruning frees memory → selector can switch back to undersampled (if hysteresis enabled)

7. **Memory stays bounded**:
   - Under sustained memory pressure, the estimator should not grow beyond configured limit
   - Completed branches are pruned and freed

### 8.4 Testing

8. **Tests verify adaptive behavior**:
   - Undersampled selector used when memory is plentiful
   - Narrow selector selected when memory pressure detected
   - Estimates remain accurate regardless of selector mode

## Implementation Process (TDD)

### Phase 1: Selector Interface Extension

```ocaml
(* stochastic_estimator.mli *)
type selector_mode = 
  | Undersampled   (** Broad exploration, spreads across branches *)
  | Narrow         (** DFS-like, follows one path to completion *)
  | Adaptive of { memory_threshold : float } (** Switch based on memory pressure *)

val create : ?selector_mode:selector_mode -> ...
```

### Phase 2: Memory-Aware Selector Selection

```ocaml
let%test_module "adaptive_selector" = (module struct
  
  (* Test: undersampled selector used when memory is plentiful *)
  let test_undersampled_when_memory_plentiful () = 
    (* Create estimator with Adaptive mode *)
    (* Simulate low memory pressure (mock memfree) *)
    (* Verify undersampled selector is used *)
    ...
  
  (* Test: narrow selector selected when memory pressure detected *)
  let test_narrow_when_memory_tight () = 
    (* Create estimator with Adaptive mode *)
    (* Simulate high memory pressure (mock memfree) *)
    (* Verify narrow selector is used *)
    ...
  
  (* Test: switching between modes preserves estimates *)
  let test_switching_preserves_estimates () = 
    (* Create estimator, sample with undersampled *)
    (* Switch to narrow, continue sampling *)
    (* Verify estimates are consistent across mode change *)
    ...
end)
```

### Phase 3: Integration with Pruning

```ocaml
let%test_module "adaptive_pruning" = (module struct
  
  (* Test: narrow mode enables pruning *)
  let test_narrow_enables_pruning () = 
    (* Create large tree, enable adaptive mode *)
    (* Simulate memory pressure → switches to narrow *)
    (* Verify branches complete and prune faster than undersampled *)
    ...
  
  (* Test: memory stays bounded under pressure *)
  let test_memory_stays_bounded () = 
    (* Create estimator with adaptive mode and memory threshold *)
    (* Run until memory pressure triggers narrow mode *)
    (* Verify memory does not exceed threshold (due to pruning) *)
    ...
end)
```

### Phase 4: Memory Monitoring Integration

```ocaml
let%test_module "memory_monitoring" = (module struct
  
  (* Test: memory monitoring uses existing memfree infrastructure *)
  let test_uses_memfree () = 
    (* Verify that adaptive mode reads from Searchspace.memfree *)
    (* Or uses /proc/meminfo directly *)
    ...
  
  (* Test: memory readings are cached (not per-sample) *)
  let test_memory_readings_cached () = 
    (* Create estimator with adaptive mode, run many samples *)
    (* Verify memory is not read from /proc/meminfo on every sample *)
    ...
end)
```

## Implementation Details

### Selector Selection Logic

```ocaml
let rec select_child (selector_mode : selector_mode) (node : 'a node) : int =
  match selector_mode with
  | Undersampled -> undersampled_selector node
  | Narrow -> depth_first_selector node
  | Adaptive { memory_threshold } ->
      let free_ratio = Searchspace.memfree () in
      if 1.0 -. free_ratio > memory_threshold then (
        (* Memory tight → narrow exploration *)
        depth_first_selector node
      ) else (
        (* Memory plentiful → broad exploration *)
        undersampled_selector node
      )
```

### Integration with `run_with_progress`

```ocaml
val run_with_progress : 
  ?batch_size:int -> 
  ?selector_mode:selector_mode ->
  ?on_progress:(progress -> unit) -> 
  'a t -> unit
```

### Reusing Existing Infrastructure

The existing `Searchspace.limit_on_low_memory` function already provides the memory-to-limit mapping. We can adapt it:

```ocaml
let selector_from_limit limit =
  if limit < 1.0 then Narrow      (* Very tight → narrow *)
  else Undersampled               (* Plenty of room → broad *)

let adaptive_selector ~max_memory_ratio =
  let limit_fn = Searchspace.limit_on_low_memory ~max_memory_ratio () in
  fun node -> selector_from_limit (limit_fn ()) node
```

### Narrow Selector Implementation

The undersampled selector currently picks from unexplored branches. A narrow (DFS-like) selector should:
- Prefer the most recently explored branch (LIFO behavior)
- Follow a single path to completion before backtracking

This can be implemented by tracking exploration order and preferring the last explored child.

## Files to Modify

- `searchspace/stochastic_estimator.ml` - implementation
- `searchspace/stochastic_estimator.mli` - interface update (add `selector_mode`, memory monitoring)
- `searchspace/searchspace.ml` - possibly expose `memfree` for use by estimator

## Files to Create

- Tests inline in `stochastic_estimator.ml` using `%test_module` and `[%expect_test]`

## Dependencies

- **Task 6 (Pruning)**: Adaptive selection only makes sense if completed branches can be pruned. Without pruning, switching to narrow mode just delays the inevitable OOM.
- **Task 3/4 (Serialization)**: If we save state, the selector mode should be part of the saved state so it can resume correctly.

## Design Notes

### Why Not Just Use `limit_on_low_memory` Directly?

The regular solver uses `limit_on_low_memory` with `breadth_search`, which controls the search order within a single traversal. The stochastic estimator needs something different:

- **Regular solver**: one search, controls breadth vs depth of traversal
- **Stochastic estimator**: many independent samples, needs to choose which branch each sample explores

The adaptive selector is the stochastic estimator's equivalent of `limit_on_low_memory` — it controls whether samples spread across branches or focus on completing individual paths.

### Hysteresis (Optional)

To avoid thrashing between modes, consider hysteresis:
- Switch to narrow at 80% memory usage
- Switch back to undersampled only when below 50% (after pruning freed memory)

This prevents rapid mode switching that could waste samples.

### Performance Considerations

- Memory readings should be cached (like `memfree` does) — not read from `/proc/meminfo` on every sample
- Selector switching should happen between batches (in `run_with_progress`), not during sampling
- The narrow selector should be as efficient as the undersampled one (same O(1) child selection)
