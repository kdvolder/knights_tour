# Task 6: Memory Management - Tree Pruning

## Goal

Implement automatic pruning of fully-explored branches from the estimator's materialized tree to reduce memory consumption. When a node becomes `isCompleted = true`, its children array is immediately replaced with an empty array, freeing the child node structures to GC.

## Background

The current estimator's tree (`'a node`) grows monotonically — nodes are only added, never removed. For very large search spaces that take a long time to estimate (e.g., hexominos on oracle2), this leads to significant memory usage.

The key insight: **once a node is marked `isCompleted = true`, we will never traverse through it again during sampling**. The undersampled selector only considers nodes with unmaterialized children or incomplete subtrees. A completed node's statistics (samples, estimates) are still needed for parent calculations, but the actual tree structure below it is not.

### What Gets Pruned vs Preserved

**Preserved (needed for continued estimation):**
- `node_view` — to know how many children exist (for selector)
- `samples`, `nodes_estimate`, `fail_estimate`, `solution_estimate` — for parent statistics
- `materialized_nodes` count — total nodes ever created (for progress tracking)
- `pruned_nodes` count — how many descendants have been pruned (for memory accounting)

**Can be pruned (not needed after completion):**
- `children` array — all children are completed, won't be traversed again
- The actual child node structures (freed by GC when no longer referenced)

### Compact Representation

After pruning, replace the children array with an empty array:

```ocaml
type 'a node = {
  node_view : 'a Searchspace.node_view;
  mutable isCompleted : bool;
  mutable children : 'a node option array;   (* [||] after pruning *)
  mutable samples : int;
  mutable nodes_estimate : float;
  mutable fail_estimate : float;
  mutable solution_estimate : float;
  mutable materialized_nodes : int;   (* total nodes ever created *)
  mutable pruned_nodes : int;          (* descendants freed by pruning *)
}
```

**Why two counters?**
- `materialized_nodes` stays constant after pruning — it represents total work done, useful for progress tracking
- `pruned_nodes` increases as we reclaim memory — the difference (`materialized_nodes - pruned_nodes`) tells you how much is still materialized
- Pruning is an optimization, not a reversal — conceptually the nodes were explored and completed; we just stop keeping their structure in memory

When `children = [||]` (empty array), it means:
- All children have been fully explored and pruned
- **No summary is needed** — the parent node already absorbed all statistics (samples, estimates, materialized_nodes) during `walk` recursion
- The selector only needs `node_view` (already on the parent) and skips completed nodes via `isCompleted`
- Pruning simply frees the child node structures; nothing is lost because parents already have everything they need

## Acceptance Criteria

### 6.1 Automatic Pruning at Completion

1. **Pruning happens automatically when a node completes**:
   - When `walk` sets the last child of a node to completed, it immediately prunes that node
   - No separate `prune_completed` call needed

2. **Only completed nodes are pruned**:
   - A node is eligible for pruning only when `isCompleted = true`
   - Partial exploration never triggers pruning

3. **Pruning preserves all statistics**:
   - After pruning, `samples`, estimates, and counts are unchanged
   - Parent nodes can still compute correct aggregate statistics

### 6.2 `pruned_nodes` Propagation

4. **Completed nodes track their own pruned count**:
   - When a node is pruned, `pruned_nodes <- materialized_nodes - 1` (all descendants)

5. **Incomplete nodes sum children's pruned counts**:
   - When a node is not yet completed, `pruned_nodes` = sum of all children's `pruned_nodes`
   - This ensures parents always reflect the total pruned descendants, even if only some children are pruned

### 6.3 Selector Safety

6. **Selector never selects through completed nodes**:
   - The undersampled selector (and all selectors) already skip completed nodes via `isCompleted`
   - Since pruned nodes are always completed, they will never be selected

7. **`walk` guards against traversing completed nodes**:
   - `walk` checks `if node.isCompleted then ()` at the top of every fork branch
   - No exception is raised — traversal simply returns immediately

### 6.4 Memory Savings

8. **Pruning reduces memory usage**:
   - Benchmarked on large search spaces
   - Memory reduction should be significant for deep, fully-explored trees

9. **Pruning does not affect estimate accuracy**:
   - Estimates before and after pruning are identical
   - Continued sampling produces same results whether or not pruning occurred

10. **Oversampling is handled gracefully**:
    - `sample` checks `est.root.isCompleted` and stops the outer loop
    - No wasted work on an already-explored tree

## Implementation Details

### Walk Modification (Pruning Hotspot)

In `walk`, after processing a child, check if the node is now completed. If so, prune inline:

```ocaml
let rec walk (selector : 'a child_selector) (on_solution : 'a -> unit) (node : 'a node) : unit =
  match node.node_view with
  | Result _ -> ()
  | Fail -> ()
  | Fork choices ->
      if node.isCompleted then ()   (* Guard: skip already-completed nodes *)
      else (
        let chosen = select_child node in
        let child_node = match node.children.(chosen) with
          | Some child -> child
          | None ->
              let c = create_node (List.nth choices chosen) in
              node.children.(chosen) <- Some c;
              c
        in
        walk selector on_solution child_node;

        (* Propagate pruned_nodes from child if it was pruned *)
        if Array.length child_node.children = 0 then (
          node.pruned_nodes <- node.pruned_nodes + child_node.pruned_nodes
        );

        (* Update all statistics from children *)
        node.samples <- Array.fold_left (fun acc child_opt -> 
          match child_opt with Some c -> acc + c.samples | None -> acc
        ) 0 node.children;
        node.nodes_estimate <- 1. +. children_estimate node.children (fun c -> c.nodes_estimate);
        node.fail_estimate <- children_estimate node.children (fun c -> c.fail_estimate);
        node.solution_estimate <- children_estimate node.children (fun c -> c.solution_estimate);
        node.materialized_nodes <- 1 + Array.fold_left (fun acc child_opt -> 
          match child_opt with Some c -> acc + c.materialized_nodes | None -> acc
        ) 0 node.children;

        (* Update isCompleted: true if all children are materialized and completed *)
        node.isCompleted <-
          Array.length node.children > 0 &&
          Array.for_all (function | Some c -> c.isCompleted | None -> false) node.children;

        (* Pruning hotspot: if all children completed, prune this node *)
        if node.isCompleted then (
          node.pruned_nodes <- node.materialized_nodes - 1;   (* all descendants *)
          node.children <- [||]                                 (* free child structures *)
        ) else (
          node.pruned_nodes <- Array.fold_left (fun acc child_opt -> 
            match child_opt with Some c -> acc + c.pruned_nodes | None -> acc
          ) 0 node.children   (* sum children's pruned counts *)
        )
      )
```

### Key Design Decisions vs Original Spec

| Spec | Actual Implementation | Reason |
|------|----------------------|--------|
| `children : 'a node option array \| Pruned` (variant) | `children : 'a node option array`, `[||]` for pruned | OCaml mutual recursion constraints prevented `'a node` reference in a variant type |
| Exception on pruned traversal | `if node.isCompleted then ()` guard in walk | Empty array is silently handled; no traversal needed, no exception required |
| For-loop processing all children | Recursive walk, one child per sample | Kept existing recursive structure; selector picks one child at a time |
| No mention of `pruned_nodes` propagation | Else-branch sums children's `pruned_nodes` | Bug fix: parents must track pruned descendants even when not yet completed |
| "Recursive pruning" mentioned | Not applicable — setting children to `[||]` is permanent, nothing to recurse into | User confirmed recursive pruning was nonsense; pruned nodes are just gone |

### Important Considerations

1. **Selector already avoids completed nodes**: The `undersampled_selector` checks `isCompleted` before selecting children. Since pruning only happens after `isCompleted = true`, the selector will never select through a pruned node.

2. **`walk` guards at every level**: Both the root-level check in `sample` (`if est.root.isCompleted`) and the per-node guard in `walk` (`if node.isCompleted then ()`) prevent wasted traversal.

3. **No unpruning**: Pruned nodes are permanent. They represent fully-explored subtrees that should never be sampled again.

4. **`pruned_nodes` invariant**: At all times, `node.pruned_nodes` = sum of pruned descendants. When the node itself is pruned, it becomes `materialized_nodes - 1` (all descendants).

## Files Modified

- `searchspace/stochastic_estimator.ml` — implementation (`pruned_nodes` field, pruning logic in `walk`, else-branch propagation)
- Tests inline using `[%expect_test]`
