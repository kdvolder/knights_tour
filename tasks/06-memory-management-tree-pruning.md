# Task 6: Memory Management - Tree Pruning

## Goal

Implement pruning of fully-explored branches from the estimator's materialized tree to reduce memory consumption. Once a subtree is fully explored (all nodes completed), its structure can be replaced with a compact representation that preserves the statistics but frees memory.

## Background

The current estimator's tree (`'a node`) grows monotonically - nodes are only added, never removed. For very large search spaces that take a long time to estimate, this can lead to significant memory usage.

The key insight: **once a node is marked `isCompleted = true`, we will never traverse through it again during sampling**. The undersampled selector only considers nodes with unmaterialized children or incomplete subtrees. A completed node's statistics (samples, estimates) are still needed for parent calculations, but the actual tree structure below it is not.

### What Gets Pruned vs Preserved

**Preserved (needed for continued estimation):**
- `node_view` - to know how many children exist (for selector)
- `samples`, `nodes_estimate`, `fail_estimate`, `solution_estimate` - for parent statistics
- `materialized_nodes` count - for progress tracking

**Can be pruned (not needed after completion):**
- `children` array - all children are completed, won't be traversed again
- The actual child node structures

**Critical invariant: Once pruned, a completed subtree must NEVER be traversed again.**
If the selector routes a sample through a pruned node, that is a **bug** (either in the
selector logic or state corruption). In this case we should raise an exception rather than
silently re-materializing nodes. This is by design - a completed subtree has been fully
explored, all its solutions have been found and counted. Re-traversing it would be wasteful
and indicates something is wrong.

### Compact Representation

After pruning, replace the children array with a compact summary:

```ocaml
type 'a node = {
  node_view : 'a Searchspace.node_view;
  mutable isCompleted : bool;
  mutable children : 'a node option array | Pruned of int * estimates_summary;
  mutable samples : int;
  mutable nodes_estimate : float;
  mutable fail_estimate : float;
  mutable solution_estimate : float;
  mutable materialized_nodes : int;
}

and estimates_summary = {
  child_count : int;
  total_samples : int;
  total_nodes_estimate : float;
  total_fail_estimate : float;
  total_solution_estimate : float;
  total_materialized_nodes : int;
}
```

When `children` is `Pruned(summary)`, it means:
- This node has `child_count` children (from `node_view`)
- All are fully explored and pruned
- The summary contains aggregate statistics that parents need

## Acceptance Criteria

### 6.1 Pruning Logic

1. **Only completed subtrees are pruned**:
   - A node is eligible for pruning only when `isCompleted = true`
   - Partial exploration never triggers pruning

2. **Pruning preserves all statistics**:
   - After pruning, `samples`, estimates, and counts are unchanged
   - Parent nodes can still compute correct aggregate statistics

3. **Pruning is optional and configurable**:
   - Can be enabled/disabled via flag
   - Pruning threshold: minimum depth or age before pruning

### 6.2 Continued Sampling After Prune

4. **Sampling works correctly after pruning**:
   - New samples are added to the correct nodes
   - Pruned nodes are "unpruned" (reconstructed) when needed for new samples
   - Statistics accumulate correctly across prune/unprune cycles

5. **Unpruning is lazy**:
   - When a new sample reaches a pruned node, it's reconstructed on-demand
   - Reconstruction follows the same path through the search space

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
  (* Test: completed node can be pruned *)
  let test_prune_completed_node () = 
    (* Create estimator, sample until node is completed *)
    (* Prune the node *)
    (* Verify children array replaced with Pruned summary *)
    (* Verify statistics preserved *)
    ...
  
  (* Test: incomplete node cannot be pruned *)
  let test_no_prune_incomplete () = 
    (* Create estimator, sample partially *)
    (* Attempt to prune - should fail or be no-op *)
    ...
  
  (* Test: pruning preserves parent statistics *)
  let test_parent_stats_preserved () = 
    (* Create tree, prune a child subtree *)
    (* Verify parent's aggregate estimates unchanged *)
    ...
end)
```

### Phase 2: Selector Safety Tests

```ocaml
let%test_module "selector_safety" = (module struct
  (* Test: selector never selects through pruned/completed nodes *)
  let test_selector_avoids_completed () = 
    (* Create estimator, sample until subtree is completed *)
    (* Prune that subtree *)
    (* Run many samples - verify selector never routes through pruned node *)
    ...
  
  (* Test: traversing pruned node raises exception *)
  let test_traverse_pruned_raises () = 
    (* Prune a node *)
    (* Force walk to traverse it (simulate bug) *)
    (* Verify exception is raised, not silent re-materialization *)
    ...
  
  (* Test: estimates preserved after prune, no double-counting *)
  let test_no_double_counting () = 
    (* Sample until completion, prune *)
    (* Verify estimates match pre-prune values exactly *)
    (* No samples should be able to go through pruned node, so no double-counting *)
    ...
end)
```

### Phase 3: Selector Integration Tests

```ocaml
let%test_module "selector_integration" = (module struct
  (* Test: undersampled_selector skips completed subtrees *)
  let test_undersampled_skips_completed () = 
    (* Create tree, complete one branch *)
    (* Verify selector always picks from uncompleted branches *)
    ...
  
  (* Test: probabilistic selector also avoids completed subtrees *)
  let test_probabilistic_skips_completed () = 
    ...
end)
```

### Phase 4: Memory Tests

```ocaml
let%test_module "memory" = (module struct
  (* Test: memory usage decreases after pruning *)
  let test_memory_decrease () = 
    (* Create large tree, measure memory *)
    (* Prune completed branches *)
    (* Measure memory again - should be lower *)
    ...
  
  (* Test: pruning deep trees saves more memory *)
  let test_deep_tree_pruning () = 
    (* Compare pruning on shallow vs deep trees *)
    (* Deep tree should show more memory savings *)
    ...
end)
```

## Design Notes

### When to Prune

Options:
1. **Manual pruning** - user calls `prune_completed est` explicitly
2. **Automatic pruning** - enabled by config, prunes during idle periods or periodic saves
3. **Threshold-based** - prune when memory exceeds threshold

Recommendation: Start with manual pruning, add automatic later.

### Pruning Implementation Sketch

```ocaml
let rec prune_node (node : 'a node) : unit =
  if not node.isCompleted then ()
  else (
    (* First, recursively prune children *)
    Array.iteri (fun i child_opt ->
      match child_opt with
      | Some child -> 
          prune_node child;
          (* After pruning, replace with summary *)
          let summary = compute_summary child in
          node.children.(i) <- None;  (* Will be handled below *)
      | None -> ()
    ) node.children;
    
    (* Replace all pruned children with single Pruned variant *)
    let summary = compute_aggregate_summary node in
    node.children <- Pruned (Array.length node.children, summary)
  )

and compute_aggregate_summary (node : 'a node) : estimates_summary =
  {
    child_count = Array.length (match node.children with
      | Children arr -> arr
      | Pruned _ -> assert false  (* Should not happen *)
    );
    total_samples = node.samples;
    total_nodes_estimate = node.nodes_estimate;
    total_fail_estimate = node.fail_estimate;
    total_solution_estimate = node.solution_estimate;
    total_materialized_nodes = node.materialized_nodes;
  }
```

### Important Considerations

1. **Selector must never select pruned nodes**: The `undersampled_selector` (and all selectors) must check for the `Pruned` variant and treat it as having zero unsampled leaves. If a selector somehow routes through a pruned node, the `walk` function should raise an exception. This is a safety invariant, not a feature.

2. **Pruning during save**: Consider pruning before saving to reduce state file size too. This is a natural pairing - prune, then save the smaller state.

3. **Thread safety**: If we add parallel sampling later, pruning must be synchronized. For now (single-threaded), this is not a concern.

## Files to Modify

- `searchspace/stochastic_estimator.ml` - implementation
- `searchspace/stochastic_estimator.mli` - interface update

## Files to Create

- Tests inline in `stochastic_estimator.ml` using `%test_module` and `[%expect_test]`
