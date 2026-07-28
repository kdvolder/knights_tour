# Task 6: Memory Management - Tree Pruning

## Goal

Implement automatic pruning of fully-explored branches from the estimator's materialized tree to reduce memory consumption. When a node becomes `isCompleted = true`, its children array is immediately replaced with a compact summary, freeing the child node structures.

## Background

The current estimator's tree (`'a node`) grows monotonically - nodes are only added, never removed. For very large search spaces that take a long time to estimate (e.g., hexominos on oracle2), this leads to significant memory usage.

The key insight: **once a node is marked `isCompleted = true`, we will never traverse through it again during sampling**. The undersampled selector only considers nodes with unmaterialized children or incomplete subtrees. A completed node's statistics (samples, estimates) are still needed for parent calculations, but the actual tree structure below it is not.

### What Gets Pruned vs Preserved

**Preserved (needed for continued estimation):**
- `node_view` - to know how many children exist (for selector)
- `samples`, `nodes_estimate`, `fail_estimate`, `solution_estimate` - for parent statistics
- `materialized_nodes` count - for progress tracking

**Can be pruned (not needed after completion):**
- `children` array - all children are completed, won't be traversed again
- The actual child node structures (freed by GC)

### Compact Representation

After pruning, replace the children array with a compact summary:

```ocaml
type 'a node = {
  node_view : 'a Searchspace.node_view;
  mutable isCompleted : bool;
  mutable children : 'a node option array | Pruned of estimates_summary;
  mutable samples : int;
  mutable nodes_estimate : float;
  mutable fail_estimate : float;
  mutable solution_estimate : float;
  mutable materialized_nodes : int;
}

and estimates_summary = {
  total_samples : int;
  total_nodes_estimate : float;
  total_fail_estimate : float;
  total_solution_estimate : float;
  total_materialized_nodes : int;
}
```

When `children` is `Pruned(summary)`, it means:
- All children have been fully explored and pruned
- The summary contains aggregate statistics that parents need

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

### 6.2 Selector Safety

4. **Selector never selects through pruned nodes**:
   - The undersampled selector (and all selectors) already skip completed nodes
   - Since pruned nodes are always completed, they will never be selected

5. **Traversing a pruned node raises an exception**:
   - If somehow `walk` reaches a pruned node (bug), it raises an exception
   - Never silently re-materialize or unprune

### 6.3 Memory Savings

6. **Pruning reduces memory usage**:
   - Benchmarked on large search spaces
   - Memory reduction should be significant for deep, fully-explored trees

7. **Pruning does not affect estimate accuracy**:
   - Estimates before and after pruning are identical
   - Continued sampling produces same results whether or not pruning occurred

## Implementation Process (TDD)

### Phase 1: Pruning Tests

```ocaml
let%test_module "pruning" = (module struct
  
  (* Test: completed node is pruned automatically *)
  let test_auto_prune_on_completion () = 
    (* Create estimator, sample until a subtree is completed *)
    (* Verify that node's children array was replaced with Pruned summary *)
    (* Verify statistics preserved: samples, estimates unchanged *)
    ...
  
  (* Test: incomplete node is not pruned *)
  let test_no_prune_incomplete () = 
    (* Create estimator, sample partially (not all children explored) *)
    (* Verify node's children array is still Children variant, not Pruned *)
    ...
  
  (* Test: pruning preserves parent statistics *)
  let test_parent_stats_preserved () = 
    (* Create tree, sample until child subtree completes and prunes *)
    (* Verify parent's aggregate estimates unchanged after prune *)
    ...
  
  (* Test: deep tree pruning cascades *)
  let test_deep_tree_pruning () = 
    (* Create deep tree, sample until all branches complete *)
    (* Verify all completed nodes are pruned recursively *)
    ...
end)
```

### Phase 2: Selector Safety Tests

```ocaml
let%test_module "selector_safety" = (module struct
  
  (* Test: selector never selects through pruned/completed nodes *)
  let test_selector_avoids_completed () = 
    (* Create estimator, sample until subtree is completed and pruned *)
    (* Run many samples - verify selector never routes through pruned node *)
    ...
  
  (* Test: traversing pruned node raises exception *)
  let test_traverse_pruned_raises () = 
    (* Create estimator, complete a subtree and prune it *)
    (* Force walk to traverse the pruned node (simulate bug) *)
    (* Verify exception is raised, not silent re-materialization *)
    ...
  
  (* Test: estimates preserved after prune, no double-counting *)
  let test_no_double_counting () = 
    (* Sample until completion, verify pruning happened *)
    (* Verify estimates match expected values exactly *)
    ...
end)
```

### Phase 3: Integration Tests

```ocaml
let%test_module "integration" = (module struct
  
  (* Test: continued sampling works after pruning *)
  let test_sampling_after_prune () = 
    (* Create tree, sample until partial completion and pruning *)
    (* Continue sampling - verify new samples are added correctly *)
    (* Verify estimates accumulate properly across prune boundary *)
    ...
  
  (* Test: memory decreases after pruning *)
  let test_memory_decrease () = 
    (* Create large tree, measure memory *)
    (* Sample until completion and pruning *)
    (* Measure memory again - should be lower *)
    ...
  
  (* Test: pruning does not affect estimate accuracy *)
  let test_estimate_accuracy_preserved () = 
    (* Create small tree with known true values *)
    (* Sample until completion (with pruning) *)
    (* Verify estimates match true values within tolerance *)
    ...
end)
```

## Implementation Details

### Where Pruning Happens

Pruning is triggered in `walk` when a node transitions to completed. The key location is after the last child of a fork node completes:

```ocaml
(* In walk, after processing all children *)
if is_last_child_completed then (
  node.isCompleted <- true;
  prune_node node  (* Replace children array with Pruned summary *)
)
```

### Prune Function

```ocaml
let rec prune_node (node : 'a node) : unit =
  if not node.isCompleted then ()
  else (
    (* Compute aggregate summary from current statistics *)
    let summary = {
      total_samples = node.samples;
      total_nodes_estimate = node.nodes_estimate;
      total_fail_estimate = node.fail_estimate;
      total_solution_estimate = node.solution_estimate;
      total_materialized_nodes = node.materialized_nodes;
    } in
    (* Replace children array with Pruned variant *)
    node.children <- Pruned summary
  )
```

### Walk Modification

In `walk`, after the loop that processes children, check if all are completed:

```ocaml
let rec walk (selector : 'a child_selector) (on_solution : 'a -> unit) (node : 'a node) : unit =
  match node.node_view with
  | Result _ -> ()  (* Already a leaf *)
  | Fail -> ()       (* Already failed *)
  | Fork choices ->
      if node.isCompleted then ()  (* Already pruned, should not reach here *)
      else (
        let children = node.children in
        let num_children = Array.length children in
        let completed_count = ref 0 in
        
        for i = 0 to num_children - 1 do
          match children.(i) with
          | Some child ->
              walk selector on_solution child;
              if child.isCompleted then incr completed_count
          | None -> ()  (* Unmaterialized, skip *)
        done;
        
        (* If all children completed, prune this node *)
        if !completed_count = num_children then (
          node.isCompleted <- true;
          prune_node node
        )
      )
```

### Critical Invariant: Pruned Node Traversal

If `walk` ever reaches a node with `children = Pruned _`, it's a bug (the selector should have skipped this completed node). Raise an exception:

```ocaml
| Pruned _ -> 
    invalid_arg "Stochastic_estimator: attempted to traverse pruned node - this is a bug"
```

### Important Considerations

1. **Selector already avoids completed nodes**: The `undersampled_selector` checks `isCompleted` before selecting children. Since pruning only happens after `isCompleted = true`, the selector will never select through a pruned node.

2. **Pruning during save**: Consider pruning before saving (Task 5) to reduce state file size. This is a natural pairing - prune, then save the smaller state.

3. **Thread safety**: If we add parallel sampling later, pruning must be synchronized. For now (single-threaded), this is not a concern.

4. **No unpruning**: Pruned nodes are permanent. They represent fully-explored subtrees that should never be sampled again. If a bug causes traversal of a pruned node, raise an exception rather than silently reconstructing.

## Files to Modify

- `searchspace/stochastic_estimator.ml` - implementation
- `searchspace/stochastic_estimator.mli` - interface update (add `estimates_summary` type)

## Files to Create

- Tests inline in `stochastic_estimator.ml` using `%test_module` and `[%expect_test]`
