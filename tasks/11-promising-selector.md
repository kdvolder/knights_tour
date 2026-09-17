# Task 11: Promising Selector — Replace Greedy with Solution Density

> **Status**: Not Started
> **Date**: 2026-08-11

## Goal

Replace the `greedy_completion_selector` (which picks children with least remaining work) with a new selector that picks the child with the highest **solution density** — i.e., `solution_estimate / (fail_estimate + solution_estimate)`.

This fixes the root cause of solver stagnation: "least remaining work" is biased toward dead ends (small branches terminate early because pieces were placed in constrained/impossible ways). The new selector drives exploration toward regions where solutions actually live.

## Background

The current greedy completion selector picks the child with `nodes_estimate - materialized_nodes` being smallest. This was intended to "finish branches faster for pruning," but it has an unintended bias:

- A branch is small because pieces were placed in ways that constrain future moves
- Constrained placements → early termination → dead ends
- "Least remaining work" = "most likely to be a dead end"

The solver has been running for ~35 days, finding 2180 solutions and then stagnating. Estimates froze because the greedy selector keeps re-exploring the same dead-end regions without discovering new ones.

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

## Acceptance Criteria

### 11.1 Selector Function

1. **Same signature as existing selectors**:
   - `promising_selector : 'a node -> int`
   - Can be passed directly to `create ~selector:promising_selector tree`

2. **Score calculation**:
   - Completed children → score -2.0 (never picked unless all are completed)
   - Unexplored children (`samples == 0` or `None`) → score -1.0
   - Explored children (`samples > 0`, not completed) → `solution_estimate / (fail_estimate + solution_estimate)`

3. **Picks highest score**:
   - Among candidates with the same highest score, pick randomly

4. **Fallback when all children are completed**:
   - Pick any child at random (walk will immediately return since completed nodes have empty children arrays; parent should become completed too)

### 11.2 Integration with Gradual Braking

5. **Replace greedy in gradual braking**:
   - `gradual_braking_memory_aware_selector` should use the new promising selector instead of `greedy_completion_selector`
   - Keep the gradual braking mechanism (sliding threshold) as-is — it still provides undersampled fallback

### 11.3 Testing

6. **Tests verify density-based selection**:
   - Given children with different densities, picks the highest
   - Unexplored children (-1.0) are preferred over completed children (-2.0)
   - Explored children with any positive density beat unexplored children (-1.0)

7. **Tests verify fallback behavior**:
   - When all children are completed, picks randomly
   - When all children are unexplored, picks randomly

## Implementation Process (TDD)

### Phase 1: Promising Selector Tests

```ocaml
let%expect_test "promising_selector picks highest density" = begin
  (* Create tree, sample enough to get different densities in children *)
  (* Apply promising selector — should pick child with highest solution_estimate / (fail + sol) *)
end

let%expect_test "promising_selector prefers explored over completed" = begin
  (* Create tree, complete one child, leave another unexplored *)
  (* Apply promising selector — should pick the unexplored child (score -1) over completed (-2) *)
end

let%expect_test "promising_selector prefers explored with density over unexplored" = begin
  (* Create tree, one child has samples and positive density, another is None *)
  (* Apply promising selector — should pick the explored child (density > -1) over None (-1) *)
end

let%expect_test "promising_selector falls back to random when all completed" = begin
  (* Create tree, complete all children *)
  (* Apply promising selector — should pick randomly among completed children *)
end

let%expect_test "promising_selector falls back to random when all unexplored" = begin
  (* Create tree, no children materialized *)
  (* Apply promising selector — should pick randomly among None children *)
end
```

### Phase 2: Integration Tests

```ocaml
let%expect_test "promising selector finds more solutions than greedy" = begin
  (* Compare promising_selector vs greedy_completion_selector on same tree *)
  (* Verify promising selector finds more solutions in same number of samples *)
end

let%expect_test "promising selector works with gradual braking" = begin
  (* Use promising_selector in gradual_braking_memory_aware_selector *)
  (* Verify selector switches between undersampled and promising correctly *)
end
```

## Files to Modify

- `searchspace/stochastic_estimator.ml` - implement `promising_selector`, update gradual braking
- `searchspace/stochastic_estimator.mli` - export `promising_selector`

## Dependencies

- **Task 7 (Greedy Completion Selector)**: This task replaces the greedy selector's logic. The gradual braking mechanism from Task 10 remains unchanged.
- **Task 6 (Pruning)**: Pruning still needed — completed branches get pruned, keeping memory bounded.

## Notes

- The selector is still "greedy" in the sense that it focuses on one child at a time (memory control is preserved). It just optimizes for *solution density* instead of *remaining work*.
- Density values in hexomino puzzles will be extremely small (~10^-11), but the relative ordering is what matters — higher density = more promising.
- The gradual braking threshold still applies: undersampled mode gets usage when memory pressure is low, promising selector takes over as pressure increases.
