# Task 8: Memory-Aware Selector

## Goal

Implement a new selector function (`memory_aware_selector`) that monitors memory pressure and dynamically chooses between broad exploration (undersampled) and greedy completion. This prevents unbounded memory growth by ensuring the estimator doesn't spread samples too thin across an ever-growing frontier.

## Background

The current stochastic estimator uses a fixed selector (default: `undersampled_selector`). This spreads samples across all unexplored branches, which is great for broad coverage but has a critical flaw:

- **Undersampled selector spreads samples thin** → individual subtrees take forever to complete
- **Nothing completes → nothing gets pruned** → memory grows unboundedly until OOM

The regular solver (`solve_file.ml`) already solves this problem with `breadth_search ~limit:memory_limit`:
- When memory is plentiful, it does **broad exploration** (BFS-like) — spreads across branches
- When memory is tight, it does **narrow exploration** (DFS-like) — follows one path to completion
- The `limit` function comes from `Searchspace.limit_on_low_memory ~max_memory_ratio:0.95`

The stochastic estimator needs the same adaptive behavior, but as a **selector function** — no API changes needed.

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

### Selector Behavior

The `memory_aware_selector` is a single function with the same signature as all other selectors:

```ocaml
val memory_aware_selector : ?threshold:float -> 'a node -> int
```

When called on a fork node, it:
1. Reads current memory pressure via `Searchspace.memfree` (cached)
2. If memory is plentiful (below threshold): behaves like `undersampled_selector` — picks child with fewest samples
3. If memory is tight (above threshold): behaves like `greedy_completion_selector` — picks child with lowest absolute `nodes_estimate`

The greedy completion behavior uses **remaining unmaterialized work** (not total estimate). For each child, it calculates `nodes_estimate - materialized_nodes` and picks the one with the smallest value — the branch closest to completion. Once completed, pruning frees memory.

## Acceptance Criteria

### 8.1 Selector Function

1. **Same signature as existing selectors**:
   - `memory_aware_selector : ?threshold:float -> 'a node -> int`
   - Can be passed directly to `create ~selector:memory_aware_selector tree`

2. **Undersampled behavior when memory is plentiful**:
   - Below threshold (default 80% usage): picks child with fewest samples
   - Same behavior as `undersampled_selector`

3. **Greedy completion when memory is tight**:
   - Above threshold: picks child with lowest absolute `nodes_estimate`
   - Skips completed/sampled nodes
   - Treats unmaterialized children (`None`) as infinite work

4. **Configurable threshold**:
   - Default: 80% memory usage (similar to `max_memory_ratio:0.95`)
   - Configurable via optional parameter

### 8.2 Memory Monitoring

5. **Uses existing `Searchspace.memfree` infrastructure**:
   - Reads `/proc/meminfo` via cached function (not per-sample)

6. **Memory readings are cached**:
   - Like `memfree`, reads from `/proc/meminfo` periodically, not on every selector call

### 8.3 Integration with Pruning (Task 6)

7. **Greedy completion enables pruning**:
   - When memory is tight, branches complete faster (picks least remaining work) → more nodes get pruned
   - Pruning frees memory → selector naturally switches back to undersampled behavior

8. **Memory stays bounded**:
   - Under sustained memory pressure, the estimator should not grow beyond configured limit
   - Completed branches are pruned and freed

### 8.4 Testing

9. **Tests verify selector behavior**:
   - Undersampled behavior when memory is plentiful
   - Greedy completion behavior when memory pressure detected
   - Estimates remain accurate regardless of selector mode

## Implementation Process (TDD)

### Phase 1: Greedy Completion Selector Tests

```ocaml
let%test_module "greedy_completion_selector" = (module struct
  
  (* Test: greedy selector picks child with lowest remaining work *)
  let test_greedy_picks_lowest_remaining_work () = 
    (* Create tree with children having different remaining work (nodes_estimate - materialized) *)
    (* Apply greedy selector — should pick child with smallest remaining work, not necessarily smallest total *)
    ...
  
  (* Test: greedy selector avoids fully sampled/completed nodes *)
  let test_greedy_skips_completed () = 
    (* Create tree, complete one child *)
    (* Apply greedy selector — should pick from remaining uncompleted children *)
    ...
  
  (* Test: greedy selector drives branches to completion *)
  let test_greedy_completes_branches () = 
    (* Create tree, sample with greedy selector *)
    (* Verify branches complete faster than undersampled *)
    ...
end)
```

### Phase 2: Memory-Aware Selector Tests

```ocaml
let%test_module "memory_aware_selector" = (module struct
  
  (* Test: undersampled behavior when memory is plentiful *)
  let test_undersampled_when_memory_plentiful () = 
    (* Create tree with children having different sample counts *)
    (* Mock memory as plentiful (high free ratio) *)
    (* Verify selector picks child with fewest samples (undersampled behavior) *)
    ...
  

    ...
  
  (* Test: threshold parameter works *)
  let test_threshold_parameter () = 
    (* Create tree, set custom threshold *)
    (* Verify selector switches at the specified memory level *)
    ...
  
  (* Test: switching between modes preserves estimates *)
  let test_switching_preserves_estimates () = 
    (* Create estimator with memory_aware_selector *)
    (* Run samples through both modes (simulated by changing mock memory) *)
    (* Verify estimates are consistent across mode change *)
    ...
end)
```

### Phase 3: Integration with Pruning

```ocaml
let%test_module "adaptive_pruning" = (module struct
  
  (* Test: greedy mode enables pruning *)
  let test_greedy_enables_pruning () = 
    (* Create large tree, use memory_aware_selector *)
    (* Simulate memory pressure → switches to greedy *)
    (* Verify branches complete and prune faster than undersampled *)
    ...
  
  (* Test: memory stays bounded under pressure *)
  let test_memory_stays_bounded () = 
    (* Create estimator with memory_aware_selector and low threshold *)
    (* Run until memory pressure triggers greedy mode *)
    (* Verify memory does not exceed threshold (due to pruning) *)
    ...
end)
```

### Phase 4: Memory Monitoring Integration

```ocaml
let%test_module "memory_monitoring" = (module struct
  
  (* Test: memory monitoring uses existing memfree infrastructure *)
  let test_uses_memfree () = 
    (* Verify that memory_aware_selector reads from Searchspace.memfree *)
    ...
  
  (* Test: memory readings are cached (not per-selector-call) *)
  let test_memory_readings_cached () = 
    (* Create estimator with memory_aware_selector, run many samples *)
    (* Verify memory is not read from /proc/meminfo on every selector call *)
    ...
end)
```

## Implementation Details

### Memory-Aware Selector Implementation

The selector reads memory pressure internally and delegates to the appropriate strategy:

```ocaml
let memory_aware_selector ?(threshold = 0.8) (node : 'a node) : int =
  let free_ratio = Searchspace.memfree () in
  if 1.0 -. free_ratio > threshold then (
    (* Memory tight → greedy completion: pick child with lowest remaining work *)
    let children = match node.children with
      | Children arr -> arr
      | Pruned _ -> invalid_arg "Stochastic_estimator: memory_aware_selector on pruned node"
    in
    let best_idx = ref (-1) in
    let best_estimate = ref Float.infinity in
    for i = 0 to Array.length children - 1 do
      match children.(i) with
      | Some child ->
          if not child.isCompleted then (
            (* Remaining work = total estimate minus what's already materialized *)
            let remaining_work = child.nodes_estimate -. Float.of_int child.materialized_nodes in
            if remaining_work < !best_estimate then (
              best_idx := i;
              best_estimate := remaining_work
            )
          )
      | None -> ()  (* Unmaterialized — treat as infinite work, skip *)
    done;
    if !best_idx = -1 then (
      (* All children are unmaterialized — fall back to undersampled behavior *)
      undersampled_selector node
    ) else !best_idx
  ) else (
    (* Memory plentiful → undersampled: pick child with fewest samples *)
    undersampled_selector node
  )
```

### Greedy Completion Behavior

The greedy completion behavior uses **remaining unmaterialized work** (not total estimate):
- For each child, calculate: `unmaterialized_work = nodes_estimate - materialized_nodes`
- Pick the child with the **smallest** remaining work — the branch closest to completion
- Skip fully sampled/completed nodes (they have zero unmaterialized children)
- Treat `None` (unmaterialized) as infinite work — don't pick unexplored branches
- **Caveat**: if all children are `None` (first visit to this node), fall back to undersampled behavior

This is the exact opposite of undersampled:
- **Undersampled**: pick from children with fewest samples (spread thin)
- **Greedy completion**: pick child with least remaining unmaterialized work (concentrate to finish)

The goal is simple: **finish a subtree → prune it → regain memory**. Always do the least remaining work first.

Note: `nodes_estimate` alone is misleading — a large subtree that's mostly explored has less remaining work than a small one that's completely unexplored. The actual "work" is `nodes_estimate - materialized_nodes`.

### Integration with Existing API

No changes needed to `create` or any other function:

```ocaml
(* Existing API — no changes *)
let est = Stochastic_estimator.create ~selector:Stochastic_estimator.memory_aware_selector tree

(* With custom threshold *)
let est = Stochastic_estimator.create ~selector:(Stochastic_estimator.memory_aware_selector ~threshold:0.7) tree

(* Works with run_with_progress — no changes needed *)
Stochastic_estimator.run_with_progress ~batch_size:5000 est
```

### Reusing Existing Infrastructure

The existing `Searchspace.limit_on_low_memory` function already provides the memory-to-limit mapping. We can adapt it for reference:

```ocaml
(* Reference from solve_file.ml — shows the pattern *)
let memory_limit = Searchspace.limit_on_low_memory ~max_memory_ratio:0.95
```

The `memory_aware_selector` follows the same pattern but applies it at the selector level.

## Files to Modify

- `searchspace/stochastic_estimator.ml` - implementation (add `memory_aware_selector` function)
- `searchspace/stochastic_estimator.mli` - interface update (export `memory_aware_selector`)

## Files to Create

- Tests inline in `stochastic_estimator.ml` using `%test_module` and `[%expect_test]`

## Dependencies

- **Task 6 (Pruning)**: Memory-aware selector only makes sense if completed branches can be pruned. Without pruning, switching to greedy mode just delays the inevitable OOM.
- **Task 3/4 (Serialization)**: If we save state, the selector mode should be part of the saved state so it can resume correctly.

## Design Notes

### Why a Selector Function?

A selector function is the simplest possible API addition:
- Same signature as all existing selectors (`'a node -> int`)
- No new types, no changes to `create`, no changes to `run_with_progress`
- Users just pass a different selector function — everything else works the same

### Hysteresis (Optional)

To avoid thrashing between modes, consider hysteresis:
- Switch to greedy at 80% memory usage (threshold)
- Switch back to undersampled only when below 50% (after pruning freed memory)

This prevents rapid mode switching that could waste samples. Implementation: store the last-used strategy in a mutable ref and only switch when memory crosses a different threshold on each direction.

### Performance Considerations

- Memory readings should be cached (like `memfree` does) — not read from `/proc/meminfo` on every selector call
- The greedy path is O(n) per node (scans children for lowest estimate), vs O(1) for undersampled. This is acceptable since it only runs when memory pressure is detected, not on every sample
- The undersampled path delegates to the existing `undersampled_selector` — no performance regression when memory is plentiful
