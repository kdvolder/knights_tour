# Task 10: Gradual Braking Memory-Aware Selector — ✅ DONE

> **Status**: Complete
> **Date**: 2025-07-28

## Goal

Replace the binary `hard_braking_memory_aware_selector` with a gradual-blend selector that eases off undersampled behavior as heap usage approaches a threshold, preventing the "freight train" overshoot problem.

## Background: The Freight Train Problem

The `hard_braking_memory_aware_selector` (task 8) uses a binary switch:
- Below threshold → undersampled (full speed)
- Above threshold → greedy (brakes applied)

**What happens in practice:**
1. Undersampled mode runs, opening "cans of worms" (partially-explored subtrees)
2. At threshold, the hard switch triggers greedy mode
3. But hundreds of thousands of subtrees are already open — they don't close instantly
4. Memory keeps growing until it hits a much higher level (massive overshoot)
5. Eventually reaches steady state where pruning ≈ growth, but at a much higher memory level

**Root cause:** Binary switching is too abrupt. By the time brakes are applied, there's already enormous momentum building from many open branches.

## The Solution: Gradual Braking via Randomized Threshold

### The Formula (Actual Implementation)

```ocaml
let ratio = u /. threshold in
if Random.float 1.0 >= ratio then undersampled_selector est node
else greedy_completion_selector est node
```

Where:
- **U** = current pressure value (from `memory_pressure` function)
- **T** = threshold (same units as U, default 100_000.0 for net nodes)

The condition `Random.float 1.0 >= ratio` is true with probability:

**`(T - U) / T = 1 - (U/T)`**

| U relative to T | Undersampled probability | Behavior |
|-----------------|--------------------------|----------|
| U = 0           | **100%**                 | Pure undersampled |
| U = T/4         | **75%**                  | Mostly undersampled |
| U = T/2         | **50%**                  | Half-and-half |
| U = 3T/4        | **25%**                  | Mostly greedy |
| U = T           | **0%**                   | Pure greedy (safety valve) |
| U > T           | **0%**                   | Pure greedy (safety valve) |

### Key Properties

1. **Linear decay**: undersampled probability decreases linearly from 100% to 0% as U goes from 0 to T
2. **Braking starts immediately**: no period of pure undersampled mode, preventing momentum buildup
3. **Safety valve**: if pressure spikes above T (external allocation), immediately go full greedy

### Why This Works Better Than Binary Switching

- **No momentum buildup**: braking starts from the first byte of pressure, preventing freight trains
- **Linear decay**: smooth transition across the entire range [0, T], not just near the limit
- **No permanent stuck states**: randomization provides mixing even at U = T

## Acceptance Criteria

### 10.1 Selector Function — ✅ Met

1. **Returns selector + stats**:
   - `gradual_braking_memory_aware_selector : threshold:float -> memory_pressure:('a t -> float) -> ('a child_selector * (unit -> gradual_braking_stats))`
   - Returns a tuple of selector function and stats accessor

2. **Gradual blending behavior**:
   - At low pressure (U << T): 100% undersampled
   - At U = T/2: ~50% undersampled, 50% greedy
   - At U = T: 0% undersampled (pure greedy)
   - At U > T: 0% undersampled (safety valve)

3. **Unit-agnostic design**:
   - Takes a `memory_pressure: ('a t -> float)` function — caller decides what metric to use
   - Threshold must be in the same units as the pressure function

4. **Stats tracking**:
   - `gradual_braking_stats` tracks total calls, undersampled count, greedy count per batch
   - Stats are reset after each read (cumulative since last call to `get_stats`)

### 10.2 Gradual Braking Verification — ✅ Met

5. **Tests verify linear decay**:
   - Scenario test in `stochastic_estimator.ml` shows undersampled probability decreasing from 100% to 0% as U goes from 0 to T
   - Results: ~100%, ~75.6%, ~47.8%, ~25.5%, 0% — closely matches expected linear decay

6. **Tests verify independent stats**:
   - Multiple selectors maintain independent statistics
   - Stats are correctly reset after reading

### 10.3 Integration — ✅ Met

7. **Used in estimate_polyomino.ml**:
   ```ocaml
   let (selector, get_stats) = Stochastic_estimator.gradual_braking_memory_aware_selector 
     ~threshold:100_000. 
     ~memory_pressure:nodes_in_memory
   ```
   - Threshold: 100,000 net nodes (materialized - pruned)
   - Pressure: `nodes_in_memory` = materialized_nodes - pruned_nodes

8. **Stats displayed in progress table**:
   - `undersampled_ratio` column shows the % of samples using undersampled vs greedy mode
   - Helps monitor whether gradual braking is working as expected

## Implementation Details

### Actual Implementation

```ocaml
let gradual_braking_memory_aware_selector ~threshold ~memory_pressure
  : ('a child_selector * (unit -> gradual_braking_stats)) =
  let total_calls = ref 0 in
  let undersampled_count = ref 0 in
  let greedy_count = ref 0 in
  let selector est (node : 'a node) : int =
    incr total_calls;
    let u = memory_pressure est in
    let ratio = u /. threshold in
    if Random.float 1.0 >= ratio then (
      incr undersampled_count;
      undersampled_selector est node
    ) else (
      incr greedy_count;
      greedy_completion_selector est node
    )
  in
  let get_stats () : gradual_braking_stats = ... in
  (selector, get_stats)
```

### Design Decisions vs Original Plan

| Aspect | Original Plan | Actual Implementation |
|--------|--------------|----------------------|
| Formula | `U + (C mod T) < T` with call counter C | `Random.float 1.0 >= U/T` — cleaner, no counter needed |
| Counter C | Incrementing call counter for mixing | Not needed — `Random.float` provides the randomness directly |
| Stats tracking | Not planned | Built-in via returned `get_stats` function |
| Heap measurement | Runtime_events ring buffer | Injected `memory_pressure` — caller chooses metric |
| Naming | `gradual_braking_selector` | `gradual_braking_memory_aware_selector` (consistent with hard braking) |

The cleaner formula was chosen because:
1. `Random.float 1.0 >= ratio` achieves the same linear decay without needing a counter
2. No int<->float conversion mess, no modulo arithmetic
3. Simpler to understand and maintain
4. Stats tracking was added because it's useful for monitoring

### Why `nodes_in_memory` as Pressure Metric?

In `estimate_polyomino.ml`, the pressure is measured as:
```ocaml
let nodes_in_memory est =
  let estimates = Stochastic_estimator.estimates est in
  estimates.materialized_nodes - estimates.pruned_nodes |> Float.of_int
```

This measures the actual memory footprint of the estimator tree (net materialized nodes). It's:
- **Accurate**: directly measures what the estimator is using
- **Zero overhead**: already computed during sampling
- **Meaningful threshold**: 100,000 net nodes is a reasonable limit

## Files Modified

- `searchspace/stochastic_estimator.ml` — added `gradual_braking_memory_aware_selector`, stats type, and tests
- `searchspace/stochastic_estimator.mli` — exported the selector and stats types
- `estimate_polyomino.ml` — uses gradual braking with `nodes_in_memory` pressure metric

## Dependencies

- **Task 8 (Hard Braking Selector)**: This task replaces/enhances the existing hard braking selector
- **Task 6 (Pruning)**: Gradual braking only works if completed branches can be pruned
- **Task 7 (Greedy Completion Selector)**: Provides the greedy strategy to blend with undersampled
