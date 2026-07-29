# Task 7: Greedy Completion Selector — ✅ DONE

> **Status**: Complete (committed `3ad844b`)
> **Date**: 2025-07-28

## Goal

Implement a drop-in selector function (`greedy_completion_selector`) that picks the child with the least remaining unmaterialized work. This drives branches to completion faster, enabling pruning and memory reclamation.

## Background

The current default selector (`undersampled_selector`) spreads samples across all branches — great for broad coverage but terrible at completing subtrees. Without completion, nothing gets pruned, and memory grows unboundedly (as we just learned on oracle2).

The greedy completion selector does the opposite: it always picks the child closest to finishing. Once a subtree completes, pruning frees memory. This is the core logic that will later be wrapped in `memory_aware_selector` (task 8) to switch between undersampled and greedy based on memory pressure.

### Remaining Work Metric

For each child, calculate:
```
remaining_work = nodes_estimate - materialized_nodes
```

This is the **absolute remaining unmaterialized work**, not the total estimate. A large subtree that's mostly explored has less remaining work than a small one that's completely unexplored.

For `None` (unmaterialized) children, remaining work is treated as `+infinity` — we don't know how much work they have yet.

### Fallback Behavior

If all children are `None` (first visit, nothing materialized yet), remaining work is infinite for all. In this case, fall back to random selection — there's no distinguishing information.

## Acceptance Criteria

### 7.1 Selector Function

1. **Same signature as existing selectors**:
   - `greedy_completion_selector : 'a node -> int`
   - Can be passed directly to `create ~selector:greedy_completion_selector tree`

2. **Picks child with least remaining work**:
   - For each `Some child`, calculate `child.nodes_estimate -. float_of_int child.materialized_nodes`
   - Pick the child with the smallest value

3. **Handles unmaterialized children**:
   - `None` children are treated as infinite remaining work (never picked unless all are None)

4. **Fallback for first visit**:
   - If all children are `None`, pick randomly

5. **Skips completed nodes**:
   - If a child is already `isCompleted`, skip it (it will be pruned anyway)

### 7.2 Testing

6. **Tests verify greedy behavior**:
   - Given children with different remaining work, picks the smallest
   - Unmaterialized children are not picked when materialized ones exist
   - Random fallback works when all children are unmaterialized

## Implementation Process (TDD)

### Phase 1: Greedy Completion Selector Tests

```ocaml
let%expect_test "greedy_completion_selector picks least remaining work" = begin
  (* Create a tree where child 0 has less remaining work than child 1 *)
  (* Sample enough to materialize both children partially *)
  (* Apply greedy selector — should pick child with smallest remaining work *)
end

let%expect_test "greedy_completion_selector skips unmaterialized children" = begin
  (* Create tree, materialize one child partially *)
  (* Apply greedy selector — should pick the materialized child (finite work) over None (infinite) *)
end

let%expect_test "greedy_completion_selector falls back to random when all None" = begin
  (* Create tree, no children materialized *)
  (* Apply greedy selector multiple times — should pick randomly *)
end

let%expect_test "greedy_completion_selector skips completed children" = begin
  (* Create tree, complete one child *)
  (* Apply greedy selector — should pick from remaining incomplete children *)
end
```

### Phase 2: Integration Tests

```ocaml
let%expect_test "greedy selector drives branches to completion" = begin
  (* Create tree, sample with greedy_completion_selector *)
  (* Verify branches complete faster than undersampled *)
  (* Verify pruning happens as expected *)
end

let%expect_test "greedy selector works with run_with_progress" = begin
  (* Use greedy_completion_selector with run_with_progress *)
  (* Verify progress reporting works correctly *)
end
```

## Files to Modify

- `searchspace/stochastic_estimator.ml` - implementation + tests
- `searchspace/stochastic_estimator.mli` - export `greedy_completion_selector`

## Dependencies

- **Task 6 (Pruning)**: Greedy selector only matters if completed branches get pruned. Without pruning, completing a subtree just leaves it sitting in memory.
