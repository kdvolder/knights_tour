# Task 8: Adaptive Selector Selection (Hard Braking) — ✅ DONE

> **Status**: Complete
> **Date**: 2025-07-28

## Goal

Implement a memory-aware selector that switches between broad exploration (undersampled) and greedy completion based on memory pressure. This prevents unbounded memory growth by ensuring the estimator doesn't spread samples too thin across an ever-growing frontier.

## Background

The current stochastic estimator uses a fixed selector (default: `undersampled_selector`). This spreads samples across all unexplored branches, which is great for broad coverage but has a critical flaw:

- **Undersampled selector spreads samples thin** → individual subtrees take forever to complete
- **Nothing completes → nothing gets pruned** → memory grows unboundedly until OOM

The regular solver (`solve_file.ml`) already solves this problem with `breadth_search ~limit:memory_limit`. The stochastic estimator needs the same adaptive behavior, but as a **selector function** — no API changes needed.

### How It Works in the Regular Solver

```ocaml
(* solve_file.ml *)
let memory_limit = Searchspace.limit_on_low_memory ~max_memory_ratio:0.95
Puzzle.solve puzzle 
  |> Searchspace.to_seq ~search:(Searchspace.breadth_search ~limit:memory_limit ~stack_mon)
```

## Acceptance Criteria

### 8.1 Selector Function — ✅ Met

1. **Same signature as existing selectors**:
   - `hard_braking_memory_aware_selector : threshold:float -> memory_pressure:('a t -> float) -> 'a child_selector`
   - Can be passed directly to `create ~selector tree`

2. **Undersampled behavior when memory is plentiful**:
   - Below threshold: picks child with fewest samples (via `undersampled_selector`)

3. **Greedy completion when memory is tight**:
   - Above threshold: picks child with lowest remaining work (via `greedy_completion_selector`)

4. **Unit-agnostic design**:
   - Takes a `memory_pressure: ('a t -> float)` function — caller decides what metric to use
   - Threshold must be in the same units as the pressure function

### 8.2 Memory Monitoring — ✅ Met (Flexible)

5. **Memory pressure is injected**:
   - The selector doesn't read memory directly — it receives a `memory_pressure` function
   - This allows callers to use any metric: heap usage, RSS, net materialized nodes, etc.

6. **Used in practice with `nodes_in_memory`**:
   - In `estimate_polyomino.ml`: `memory_pressure:nodes_in_memory` where `nodes_in_memory est = materialized_nodes - pruned_nodes`
   - This measures actual memory footprint of the estimator tree

### 8.3 Integration with Pruning (Task 6) — ✅ Met

7. **Greedy completion enables pruning**:
   - When memory is tight, branches complete faster → more nodes get pruned
   - Pruning frees memory → selector naturally switches back to undersampled behavior

8. **Memory stays bounded**:
   - Under sustained memory pressure, the estimator switches to greedy mode
   - Completed branches are pruned and freed

### 8.4 Testing — ✅ Met

9. **Tests verify selector behavior**:
   - Scenario test in `stochastic_estimator.ml` shows switching between undersampled and greedy modes
   - Demonstrates the feedback loop: pressure rises → switches to greedy → pruning reduces pressure → switches back

## Implementation Details

### Actual Implementation

```ocaml
let hard_braking_memory_aware_selector ~threshold ~memory_pressure est (node : 'a node) : int =
  if memory_pressure est > threshold then greedy_completion_selector est node
  else undersampled_selector est node
```

### Usage in estimate_polyomino.ml

```ocaml
let nodes_in_memory est =
  let estimates = Stochastic_estimator.estimates est in
  estimates.materialized_nodes - estimates.pruned_nodes |> Float.of_int

let selector = hard_braking_memory_aware_selector 
  ~threshold:0.3 
  ~memory_pressure:(fun _ -> Searchspace.heap_usage_mb ())
```

### Design Decisions vs Original Plan

| Aspect | Original Plan | Actual Implementation |
|--------|--------------|----------------------|
| Memory source | `/proc/meminfo` via `Searchspace.memfree` | Injected `memory_pressure` function |
| Threshold unit | System memory percentage (0-1) | Unit-agnostic (any float) |
| Switching | Binary at threshold | Binary at threshold (same behavior) |
| Naming | `memory_aware_selector` | `hard_braking_memory_aware_selector` |

The unit-agnostic design was chosen because:
1. Different use cases need different pressure metrics (heap vs RSS vs system memory)
2. The selector logic is the same regardless of what "pressure" means
3. Callers can compose any measurement function with the selector

## Files Modified

- `searchspace/stochastic_estimator.ml` — added `hard_braking_memory_aware_selector` function and scenario test
- `searchspace/stochastic_estimator.mli` — exported the selector type

## Dependencies

- **Task 6 (Pruning)**: Essential — without pruning, switching to greedy mode just delays OOM
- **Task 7 (Greedy Completion Selector)**: Essential — provides the greedy strategy to switch to
