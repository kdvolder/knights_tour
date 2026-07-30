# Task 10: Hybrid Memory Pressure Selector (Internal + External)

## Goal

Replace the current `memory_aware_selector` (which relies solely on OS-level free memory via `/proc/meminfo`) with a hybrid selector that combines **internal OCaml heap pressure** (`Gc.stat()`) and **external system memory pressure** (`/proc/meminfo`). The selector should use internal metrics as the primary trigger and external metrics to adjust thresholds, avoiding permanent "stuck in greedy" states.

## Background

### The Problem with External-Only Monitoring

The current `memory_aware_selector` uses `/proc/meminfo` to detect memory pressure:

```ocaml
let memory_aware_selector ?(threshold = 0.8) () (node : 'a node) : int =
  let free_ratio = Searchspace.memfree () in
  if 1.0 -. free_ratio > threshold then greedy_selector node
  else undersampled_selector node
```

This has a critical flaw discovered through experimentation: **OCaml 5.x does not return memory to the OS for small GC objects** (like solver nodes). Even after pruning and GC runs, RSS stays high because OCaml holds the memory in its heap for reuse.

**Experiment results:**
- Large contiguous blocks (10MB arrays): OCaml returns memory to OS ✅
- Many small objects (64KB chunks, GC records): OCaml does NOT return memory ❌
- `Gc.compact()` makes no difference in either case

This means:
1. RSS stays high even after pruning → external pressure never drops
2. Once the threshold triggers greedy mode, it may **never switch back** because external pressure is permanent
3. The selector gets stuck in greedy mode, preventing the estimator from ever growing its heap again

### Why Internal Metrics Are Self-Correcting

`Gc.stat()` provides metrics about OCaml's internal heap state:

```ocaml
type stat = {
  heap_words : int;        (* current major heap size in words *)
  live_words : int;        (* words used by live objects *)
  free_words : int;        (* words in free blocks within the heap *)
  free_blocks : int;       (* number of free blocks *)
  largest_free : int;      (* NOTE: always 0 in OCaml 5 — compaction removed *)
  fragments : int;         (* number of fragmented blocks *)
  top_heap_words : int;    (* maximum heap size ever reached *)
}
```

Key calculations:
- **Internal usage**: `live_words / heap_words` — how much of the current heap is actually used
- **Internal free**: `free_words / heap_words` — how much of the current heap is available for reuse
- **Debt**: `top_heap_words - live_words` — how much memory OCaml has "borrowed" from the OS but isn't using

When we're in greedy mode and prune nodes:
- `live_words` decreases → internal usage drops → naturally switches back to undersampled ✅
- RSS stays high (OCaml holds memory) → external pressure stays high → stuck in greedy forever ❌

### The Debt Model

The key insight: **external pressure is only a valid "alarm" if OCaml doesn't have enough internal slack to absorb it.**

Think of it like a bank account:
- **External pressure** = debt to the OS (how much memory OCaml has borrowed but isn't using)
- **Internal free** = available cash (free blocks within the heap that can be reused)

If `free_words >= debt`, then OCaml has enough internal slack to "pay back" the external pressure. The alarm should be **turned off** because OCaml can absorb more allocation without needing to grow the heap further.

## Design: Debt-Based Threshold Adjustment

### Core Idea

Use internal pressure as the **primary trigger** (self-correcting) and external pressure to **adjust the threshold** based on whether OCaml has enough internal slack:

```ocaml
let hybrid_selector ?(base_threshold = 0.8) () (node : 'a node) : int =
  let gc_stats = Gc.stat () in
  
  (* Internal metrics *)
  let internal_usage = Float.of_int gc_stats.Gc.live_words /. Float.of_int gc_stats.Gc.heap_words in
  let internal_free = Float.of_int gc_stats.Gc.free_words /. Float.of_int gc_stats.Gc.heap_words in
  
  (* External metrics *)
  let external_free = Searchspace.memfree () in
  let external_pressure = 1.0 -. external_free in
  
  (* Debt: how much memory OCaml has borrowed from OS but isn't using *)
  let debt_words = Float.of_int gc_stats.Gc.top_heap_words -. Float.of_int gc_stats.Gc.live_words in
  let debt_ratio = debt_words /. Float.of_int gc_stats.Gc.heap_words in
  
  (* Adjust threshold based on external pressure and internal slack *)
  let adjusted_threshold = 
    if debt_ratio < internal_free then (
      (* OCaml has enough internal slack to cover its debt *)
      (* External pressure is "paid for" — use base threshold or even higher *)
      if external_pressure > 0.5 then base_threshold -. 0.1 (* slightly more aggressive *)
      else base_threshold +. 0.1  (* lots of slack, be more aggressive in undersampled *)
    ) else (
      (* OCaml doesn't have enough internal slack — external pressure is real *)
      if external_pressure > 0.6 then base_threshold -. 0.2 (* switch to greedy sooner *)
      else if external_pressure > 0.4 then base_threshold -. 0.1 (* slightly sooner *)
      else base_threshold  (* normal, use default threshold *)
    )
  in
  
  if internal_usage > adjusted_threshold then greedy_selector node
  else undersampled_selector node
```

### Debt Calculation Details

**Debt**: `top_heap_words - live_words`
- This is the total "unused borrowed memory" — how much OCaml has asked from the OS but isn't actively using
- When `debt = 0`, OCaml's heap is fully utilized (no borrowed memory)
- When `debt > 0`, OCaml has idle capacity in its heap

**Internal free**: `free_words / heap_words`
- This is the fraction of the current heap that's available for reuse (free blocks)
- When `internal_free` is high, OCaml can allocate more without growing the heap

**Debt coverage**: `debt_ratio < internal_free`
- If true: OCaml has enough free blocks to cover its debt → external pressure is "paid for"
- If false: OCaml's free blocks aren't enough to cover debt → external pressure is real and concerning

### Threshold Adjustment Logic

| Condition | Internal Slack? | External Pressure | Action |
|-----------|-----------------|-------------------|--------|
| `debt < internal_free` | ✅ Yes (slack covers debt) | Low (< 40%) | `threshold + 0.1` — more undersampled (be aggressive) |
| `debt < internal_free` | ✅ Yes (slack covers debt) | High (> 50%) | `threshold - 0.1` — slightly more greedy (cautious) |
| `debt >= internal_free` | ❌ No (no slack) | Low (< 40%) | `threshold` — use base threshold |
| `debt >= internal_free` | ❌ No (no slack) | Medium (40-60%) | `threshold - 0.1` — slightly more greedy |
| `debt >= internal_free` | ❌ No (no slack) | High (> 60%) | `threshold - 0.2` — switch to greedy sooner |

### Why This Works

1. **Self-correcting**: Internal usage naturally drops when we prune → threshold comparison becomes false → switches back to undersampled
2. **External pressure is contextual**: Only matters when OCaml doesn't have internal slack to absorb it
3. **No permanent stuck states**: Once pruning reduces `live_words`, internal usage drops and we naturally switch back
4. **Responsive to system pressure**: When other processes need memory AND OCaml has no internal slack, we switch to greedy sooner

## Acceptance Criteria

### 10.1 Selector Function

1. **Same signature as existing selectors**:
   - `hybrid_selector : ?base_threshold:float -> unit -> 'a node -> int`
   - Can be passed directly to `create ~selector:hybrid_selector tree`

2. **Internal pressure is the primary trigger**:
   - `Gc.stat()` metrics determine when to switch modes
   - Internal usage (`live_words / heap_words`) compared against threshold

3. **External pressure adjusts the threshold**:
   - `/proc/meminfo` readings modify the effective threshold based on debt coverage
   - External pressure only matters when OCaml has no internal slack

4. **Debt calculation is correct**:
   - `debt = top_heap_words - live_words` (in words)
   - `debt_ratio = debt / heap_words` (normalized to 0-1 range)
   - `internal_free = free_words / heap_words` (normalized to 0-1 range)

5. **Threshold adjustment is smooth**:
   - No hard switches based on external pressure alone
   - External pressure only nudges the threshold up or down

### 10.2 Self-Correcting Behavior

6. **Switches back to undersampled after pruning**:
   - When in greedy mode and nodes are pruned, internal usage drops
   - Selector naturally switches back to undersampled behavior

7. **No permanent stuck states**:
   - Even if external pressure is high, internal usage must exceed threshold to trigger greedy
   - Once pruning reduces `live_words`, selector switches back

8. **Responsive to system pressure**:
   - When other processes need memory AND OCaml has no internal slack, switches to greedy sooner

### 10.3 Performance

9. **`Gc.stat()` is called efficiently**:
   - `Gc.stat()` triggers a full major collection — expensive operation
   - Cache results between selector calls (like `memfree` does)
   - Or use `Gc.quick_stat()` for lightweight readings (but it doesn't include `live_words`, `free_words`)

10. **No performance regression when memory is plentiful**:
    - When internal usage is low, selector delegates to `undersampled_selector` quickly

### 10.4 Testing

11. **Tests verify debt calculation**:
    - Create tree, allocate nodes, verify `debt = top_heap_words - live_words`
    - Verify `internal_free = free_words / heap_words`

12. **Tests verify threshold adjustment**:
    - Create tree, simulate different memory states
    - Verify threshold is adjusted correctly based on debt coverage

13. **Tests verify self-correcting behavior**:
    - Create estimator with hybrid_selector
    - Run until greedy mode triggers (high internal usage)
    - Prune nodes → verify selector switches back to undersampled

14. **Tests verify external pressure only matters when no slack**:
    - Create tree with high internal free (lots of slack)
    - Simulate external pressure → verify threshold is NOT adjusted significantly
    - Create tree with low internal free (no slack)
    - Simulate external pressure → verify threshold IS adjusted

## Implementation Process (TDD)

### Phase 1: Debt Calculation Tests

```ocaml
let%test_module "debt_calculation" = (module struct
  
  (* Test: debt calculation is correct *)
  let test_debt_calculation () = 
    (* Create tree, allocate nodes *)
    (* Call Gc.stat() and verify debt = top_heap_words - live_words *)
    ...
  
  (* Test: internal free calculation is correct *)
  let test_internal_free_calculation () = 
    (* Create tree, verify free_words / heap_words is correct *)
    ...
  
  (* Test: debt decreases when nodes are pruned *)
  let test_debt_decreases_on_prune () = 
    (* Create tree, allocate nodes *)
    (* Prune some nodes *)
    (* Call Gc.stat() and verify debt decreased *)
    ...
end)
```

### Phase 2: Threshold Adjustment Tests

```ocaml
let%test_module "threshold_adjustment" = (module struct
  
  (* Test: threshold adjusted when no internal slack *)
  let test_threshold_adjusted_when_no_slack () = 
    (* Create tree with low internal free *)
    (* Simulate external pressure *)
    (* Verify threshold is lowered *)
    ...
  
  (* Test: threshold NOT adjusted when internal slack covers debt *)
  let test_threshold_not_adjusted_when_slack_exists () = 
    (* Create tree with high internal free *)
    (* Simulate external pressure *)
    (* Verify threshold is NOT significantly adjusted *)
    ...
  
  (* Test: smooth threshold adjustment *)
  let test_smooth_threshold_adjustment () = 
    (* Create tree, vary external pressure *)
    (* Verify threshold adjusts smoothly, not in steps *)
    ...
end)
```

### Phase 3: Self-Correcting Behavior Tests

```ocaml
let%test_module "self_correcting" = (module struct
  
  (* Test: switches back to undersampled after pruning *)
  let test_switches_back_after_pruning () = 
    (* Create estimator with hybrid_selector *)
    (* Run until greedy mode triggers (high internal usage) *)
    (* Prune nodes → verify selector switches back to undersampled *)
    ...
  
  (* Test: no permanent stuck states *)
  let test_no_permanent_stuck_states () = 
    (* Create estimator with hybrid_selector, high external pressure *)
    (* Run until greedy mode triggers *)
    (* Prune nodes → verify selector switches back even with high external pressure *)
    ...
end)
```

### Phase 4: Integration Tests

```ocaml
let%test_module "integration" = (module struct
  
  (* Test: hybrid selector works with existing API *)
  let test_hybrid_selector_works_with_api () = 
    (* Create estimator with hybrid_selector *)
    (* Run samples, verify estimates are accurate *)
    ...
  
  (* Test: hybrid selector bounds memory growth *)
  let test_hybrid_selector_bounds_memory () = 
    (* Create estimator with hybrid_selector and low threshold *)
    (* Run until memory pressure triggers greedy mode *)
    (* Verify memory does not exceed threshold (due to pruning) *)
    ...
end)
```

## Implementation Details

### Gc.stat() Performance Consideration

`Gc.stat()` triggers a full major collection, which is expensive. Options:

1. **Cache results**: Call `Gc.stat()` once per batch (not per selector call)
2. **Use `Gc.quick_stat()`**: Faster but doesn't include `live_words`, `free_words` — **NOT useful for debt calculation**
3. **Call on schedule**: Call every N samples, cache result

**Critical finding from testing**: `Gc.quick_stat()` returns `heap_words` and `top_heap_words`, but these **never change** — OCaml's heap never shrinks. After growing to 10GB and shrinking to 256MB, both values stayed at the peak. The ratio `heap_words / top_heap_words` is always 1.000 regardless of actual live memory usage.

**Conclusion**: `Gc.quick_stat()` is **completely useless** for the debt model. We **must** use `Gc.stat()` to get `live_words` and `free_words`, which only come from the expensive full GC call.

**Recommendation**: Cache results between selector calls within a batch. Call `Gc.stat()` once per batch and reuse the result for all selector calls in that batch. The GC pause happens anyway during major collections, so we piggyback on it.

### Debt Coverage Calculation

```ocaml
let debt_coverage gc_stats =
  let debt_words = Float.of_int gc_stats.Gc.top_heap_words -. Float.of_int gc_stats.Gc.live_words in
  let heap_words = Float.of_int gc_stats.Gc.heap_words in
  let internal_free = Float.of_int gc_stats.Gc.free_words /. heap_words in
  let debt_ratio = debt_words /. heap_words in
  (debt_ratio, internal_free, debt_ratio < internal_free)
```

### Threshold Adjustment Function

```ocaml
let adjust_threshold base_threshold debt_coverage external_pressure =
  let debt_ratio, internal_free, has_slack = debt_coverage in
  if has_slack then (
    (* OCaml has enough internal slack to cover its debt *)
    if external_pressure > 0.5 then base_threshold -. 0.1
    else base_threshold +. 0.1
  ) else (
    (* OCaml doesn't have enough internal slack *)
    if external_pressure > 0.6 then base_threshold -. 0.2
    else if external_pressure > 0.4 then base_threshold -. 0.1
    else base_threshold
  )
```

### Full Selector Implementation Sketch

```ocaml
let hybrid_selector ?(base_threshold = 0.8) () (node : 'a node) : int =
  (* Cache Gc.stat() result — call once per batch *)
  let gc_stats = cached_gc_stat () in
  
  (* Internal metrics *)
  let internal_usage = Float.of_int gc_stats.Gc.live_words /. Float.of_int gc_stats.Gc.heap_words in
  
  (* External metrics *)
  let external_free = Searchspace.memfree () in
  let external_pressure = 1.0 -. external_free in
  
  (* Debt calculation *)
  let debt_ratio = 
    (Float.of_int gc_stats.Gc.top_heap_words -. Float.of_int gc_stats.Gc.live_words) 
    /. Float.of_int gc_stats.Gc.heap_words
  in
  let internal_free = Float.of_int gc_stats.Gc.free_words /. Float.of_int gc_stats.Gc.heap_words in
  let has_slack = debt_ratio < internal_free in
  
  (* Adjust threshold based on external pressure and internal slack *)
  let adjusted_threshold = 
    if has_slack then (
      if external_pressure > 0.5 then base_threshold -. 0.1
      else base_threshold +. 0.1
    ) else (
      if external_pressure > 0.6 then base_threshold -. 0.2
      else if external_pressure > 0.4 then base_threshold -. 0.1
      else base_threshold
    )
  in
  
  if internal_usage > adjusted_threshold then greedy_selector node
  else undersampled_selector node
```

## Files to Modify

- `searchspace/stochastic_estimator.ml` - implementation (add `hybrid_selector` function, replace `memory_aware_selector`)
- `searchspace/stochastic_estimator.mli` - interface update (export `hybrid_selector`, deprecate `memory_aware_selector`)

## Files to Create

- Tests inline in `stochastic_estimator.ml` using `%test_module` and `[%expect_test]`

## Dependencies

- **Task 8 (Memory-Aware Selector)**: This task replaces/enhances the existing `memory_aware_selector`
- **Task 6 (Pruning)**: Self-correcting behavior only works if completed branches can be pruned
- **`memfree.ml`**: External memory pressure measurement (reuse existing infrastructure)

## Design Notes

### Why Not "Either/Or" Trigger?

Using external pressure as a direct trigger (either internal OR external) would cause permanent stuck states:
- External pressure never drops (OCaml doesn't return memory for small objects)
- Once triggered, greedy mode never ends because external pressure is permanent

### Why Debt Model?

The debt model provides a principled way to combine internal and external metrics:
- **Internal pressure** is self-correcting (drops when we prune)
- **External pressure** is only relevant when OCaml has no internal slack to absorb it
- The debt calculation (`top_heap_words - live_words`) quantifies how much "borrowed" memory OCaml has idle
- If `free_words >= debt`, OCaml can absorb more allocation without growing the heap

### Why Not Just Use Internal Pressure?

Pure internal pressure would ignore system-wide memory pressure:
- If other processes need memory, we should be more conservative
- The debt model allows external pressure to nudge the threshold when OCaml has no internal slack

### Why Not Gc.compact()?

`Gc.compact()` was tested and found to make no difference:
- RSS stays high even with compaction for small GC objects
- Compaction is expensive (pauses GC) and doesn't solve the problem for small allocations
