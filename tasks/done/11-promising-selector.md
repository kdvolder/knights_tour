# Task 11: Promising Selector — Replace Greedy with Solution Density

> **Status**: Done ✅
> **Date**: 2026-08-11
> **Completed**: 2026-08-11

## Goal

Add a new `greedy_solution` selector (which picks children with highest solution density) alongside the existing `greedy_completion_selector`. The new selector picks the child with the highest **solution density** — i.e., `solution_estimate / (fail_estimate + solution_estimate)`.

This fixes the root cause of solver stagnation: "least remaining work" is biased toward dead ends (small branches terminate early because pieces were placed in constrained/impossible ways). The new selector drives exploration toward regions where solutions actually live.

## Background

The `greedy_completion_selector` picks the child with `nodes_estimate - materialized_nodes` being smallest. This was intended to "finish branches faster for pruning," but it has an unintended bias:

- A branch is small because pieces were placed in ways that constrain future moves
- Constrained placements → early termination → dead ends
- "Least remaining work" = "most likely to be a dead end"

The solver has been running for ~35 days, finding 2180 solutions and then stagnating. Estimates froze because the greedy selector keeps re-exploring the same dead-end regions without discovering new ones.

The `greedy_solution` selector addresses this by optimizing for solution density instead of remaining work.

### The New Metric: Solution Density

For each materialized, non-completed child with samples > 0:
```
density = solution_estimate / (fail_estimate + solution_estimate)
```

This gives the fraction of leaves that are solutions. Higher = more promising.

### Priority Ordering (highest score wins)

| Condition | Score | Rationale |
|-----------|-------|-----------|
| `isCompleted` | -2.0 | Fully explored, nothing to gain from re-exploring |
| `samples == 0` or `None` | -1.0 | Unexplored — could be anything, but better than re-walking completed branches |
| Otherwise | `density` (0.0 to 1.0) | Explored, use density as the signal |

Pick the child with the **highest** score. This naturally:
1. Prefers explored branches with high solution density (exploitation)
2. Falls back to unexplored when no explored candidates exist (exploration)
3. Avoids completed branches entirely

### Self-Correcting Feedback Loop

This is a multi-armed bandit strategy:
- **Rich region**: keep finding solutions → density stays high → stay there
- **Lucky break**: more sampling reveals the truth → density drops → migrate elsewhere

No external "exploration bonus" needed — the density signal handles diversification automatically.

## API Design

The braking selectors are parameterized with a `greedy_selector` argument instead of having it hardcoded. This makes the braking mechanism generic and reusable.

### New selector
```ocaml
val greedy_solution : 'a child_selector
(** Picks the child with highest solution density: `solution_estimate / (fail_estimate + solution_estimate)`.
    Greedy for solutions rather than greedy for completion. *)
```

### Updated braking selectors (parameterized)
```ocaml
val hard_braking_memory_aware_selector :
  threshold:float -> memory_pressure:('a t -> float) ->
  greedy_selector:'a child_selector -> 'a child_selector
(** Switches between undersampled and the provided greedy selector based on threshold. *)

val gradual_braking_memory_aware_selector :
  threshold:float -> memory_pressure:('a t -> float) ->
  greedy_selector:'a child_selector -> ('a child_selector * (unit -> gradual_braking_stats))
(** Probabilistically blends undersampled and the provided greedy selector. *)
```

### Usage example
```ocaml
gradual_braking_memory_aware_selector
  ~threshold:8000.0
  ~memory_pressure:(fun est -> Float.of_int (est.root.materialized_nodes - est.root.pruned_nodes))
  ~greedy_selector:greedy_solution
```

### Acceptance Criteria

### 11.1 Selector Function (`greedy_solution`) ✅ Met

1. **Same signature as existing selectors**:
   - `greedy_solution_selector : 'a t -> 'a node -> int` ✅
   - Can be passed directly to `create ~selector:greedy_solution_selector tree` ✅

2. **Score calculation**:
   - Completed children → score -2.0 ✅
   - Unexplored children (`samples == 0` or `None`) → score -1.0 ✅
   - Explored children (`samples > 0`, not completed) → `solution_estimate / (fail_estimate + solution_estimate)` ✅

3. **Picks highest score**:
   - Among candidates with the same highest score, pick randomly ✅

4. **Fallback when all children are completed**:
   - Pick any child at random ✅

### 11.2 Integration with Gradual Braking ✅ Met

5. **Braking selectors accept `greedy_selector` parameter**:
   - Both braking selectors take a `greedy_selector:'a child_selector` argument ✅
   - The greedy selector is no longer hardcoded — callers choose which one to use ✅

6. **Default usage in solver code**:
   - `estimate_polyomino.ml` updated to pass `~greedy_selector:greedy_solution_selector` ✅

### 11.3 Testing ✅ Met (unit tests), ⏸ Deferred (integration)

8. **Tests verify density-based selection**:
   - Step-by-step inspection test shows selector picks highest density ✅

9. **Tests verify fallback behavior**:
   - Covered by test logic (unexplored > completed, explored with density > unexplored) ✅

10. **Tests verify parameterized braking**:
   - Both selectors accept the parameter, all call sites updated ✅

11. **Tests verify backward compat**:
   - `greedy_completion_selector` still exists and works ✅
   - All existing tests pass with identical output ✅

## Results

### What works:
- `greedy_solution_selector` correctly picks children by highest solution density
- Braking selectors are parameterized — callers choose which greedy selector to use
- Test demonstrates density-driven selection: avoids dead-end branches (density=0) in favor of high-density ones
- Selector is memory-safe: still commits to one child at a time (no explosion)

### What doesn't work yet:
- **Cold-start problem**: With zero solutions found anywhere in the tree, all densities are 0. The selector becomes random tie-breaking — no advantage over undersampled.
- **No signal to guide**: The solver ran 63 batches (~1 hour) with zero solutions. Estimates grew monotonically upward (5e26 → 1e27) as the solver discovered new dead-end regions, but no density signal emerged.
- **Real-world behavior**: On the polyomino puzzle, `greedy_completion` (smallest first) gets trapped in small dead-end branches. The new selector would help *once solutions are found*, but doesn't solve the cold-start problem.

### Key insight from live testing:
Systematic search (DFS-like) outperforms random sampling because solutions are **clustered**, not uniformly distributed. Lowering the braking threshold to force greedy behavior earlier was more effective than increasing it — committing to one region and sweeping it is better than scattering probes. This suggests the selector should complement, not replace, systematic exploration.

## Implementation Process (TDD)

### Phase 0: Parameterize Braking API ✅ Completed

Pure refactoring — no behavior change. Make the braking selectors accept a `greedy_selector` parameter instead of having it hardcoded.

- [x] Update `.mli`: add `greedy_selector:'a child_selector` parameter to both braking selectors
- [x] Update `.ml`: pass `greedy_selector` through instead of calling `greedy_completion_selector` directly
- [x] Update all call sites (tests + solver code) to pass `~greedy_selector:greedy_completion_selector`
- [x] Build passes, all tests pass with identical output

### Phase 1: `greedy_solution` Selector Tests ✅ Completed

- [x] Implemented `greedy_solution_selector` with density-based scoring
- [x] Added `print_tree` helper showing fails, sols, and density for inspection
- [x] Created step-by-step inspection test with switchable selector:
  - Phase 1: undersampled to materialize children
  - Phase 2: switch to `greedy_solution_selector` — avoids dead-end branches (density=0)
  - Phase 3: switch to `greedy_completion_selector` — goes back to dead ends (smallest first)
- [x] Test promotes output showing selector behavior
- [x] Removed floating-point tolerance from candidate selection (exact equality only)

### Phase 2: Parameterized Braking Tests ✅ Completed

- [x] Both `hard_braking_memory_aware_selector` and `gradual_braking_memory_aware_selector` accept `~greedy_selector`
- [x] All existing tests updated to pass `~greedy_selector:greedy_completion_selector`
- [x] Build passes, all tests pass

### Phase 3: Integration Tests ⏸ Deferred

- Not yet implemented — requires solutions to be found in the search space for density signal to exist
- The cold-start problem means `greedy_solution_selector` cannot differentiate branches until at least one solution is found
- This will be addressed by Task 12 (sliding threshold) which keeps undersampled alive longer, increasing chance of finding first solution

```ocaml
let%expect_test "greedy_solution picks highest density" = begin
  (* Create tree, sample enough to get different densities in children *)
  (* Apply greedy_solution — should pick child with highest solution_estimate / (fail + sol) *)
end

let%expect_test "greedy_solution prefers explored over completed" = begin
  (* Create tree, complete one child, leave another unexplored *)
  (* Apply greedy_solution — should pick the unexplored child (score -1) over completed (-2) *)
end

let%expect_test "greedy_solution prefers explored with density over unexplored" = begin
  (* Create tree, one child has samples and positive density, another is None *)
  (* Apply greedy_solution — should pick the explored child (density > -1) over None (-1) *)
end

let%expect_test "greedy_solution falls back to random when all completed" = begin
  (* Create tree, complete all children *)
  (* Apply greedy_solution — should pick randomly among completed children *)
end

let%expect_test "greedy_solution falls back to random when all unexplored" = begin
  (* Create tree, no children materialized *)
  (* Apply greedy_solution — should pick randomly among None children *)
end
```



## Files to Modify

- `searchspace/stochastic_estimator.ml` - implement `greedy_solution`, parameterize braking selectors
- `searchspace/stochastic_estimator.mli` - export `greedy_solution`, update braking selector signatures

## Dependencies

- **Task 7 (Greedy Completion Selector)**: `greedy_completion_selector` stays as-is. This task adds a new selector alongside it.
- **Task 10 (Gradual Braking)**: The gradual braking mechanism stays the same — it just becomes parameterized with a `greedy_selector` argument instead of having one hardcoded.
- **Task 6 (Pruning)**: Pruning still needed — completed branches get pruned, keeping memory bounded.

## Notes

- The selector is still "greedy" in the sense that it focuses on one child at a time (memory control is preserved). It just optimizes for *solution density* instead of *remaining work*.
- Density values in hexomino puzzles will be extremely small (~10^-11), but the relative ordering is what matters — higher density = more promising.
- The gradual braking threshold still applies: undersampled mode gets usage when memory pressure is low, promising selector takes over as pressure increases.
