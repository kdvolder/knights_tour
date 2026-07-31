# Task 10: Gradual Braking Memory-Aware Selector

## Goal

Replace the binary `memory_aware_selector` with a gradual-blend selector that eases off undersampled behavior as heap usage approaches a threshold, preventing the "freight train" overshoot problem. Uses the formula `U + (C mod T) < T` to smoothly transition between undersampled and greedy modes.

## Background: The Freight Train Problem

The current `memory_aware_selector` uses a binary switch at 30% free memory:
- Below threshold → undersampled (full speed)
- Above threshold → greedy (brakes applied)

**What happens in practice:**
1. Undersampled mode runs for hours, opening "cans of worms" (partially-explored subtrees)
2. At 30% free, the hard switch triggers greedy mode
3. But ~940K subtrees are already open — they don't close instantly
4. Memory keeps growing until it hits ~10% free (massive overshoot)
5. Eventually reaches steady state where pruning ≈ growth, but at a much higher memory level than necessary

**Root cause:** Binary switching is too abrupt. By the time brakes are applied, there's already enormous momentum building from hundreds of thousands of open branches.

## The Solution: Gradual Braking via `U + (C mod T) < T`

### The Formula

```
undersampled_fires = U + (C mod T) < T
```

Where:
- **U** = current OCaml heap usage in MB (from `Searchspace.heap_usage_mb()`)
- **C** = incrementing call counter (one per selector invocation, wraps on overflow)
- **T** = absolute memory threshold in MB (e.g., 8000 for 8GB)

### How It Works

The term `(C mod T)` ranges from `0` to `T-1`, so the expression ranges from `U` to `U + T - 1`.
The condition `< T` is true when `(C mod T) < T - U`, which happens with probability:

**`(T - U) / T = 1 - (U/T)`**

| U relative to T | Expression range | Undersampled probability | Behavior |
|-----------------|------------------|--------------------------|----------|
| U = 0           | [0, T-1]         | **100%**                 | Pure undersampled |
| U = T/4         | [T/4, 5T/4]     | **75%**                  | Mostly undersampled |
| U = T/2         | [T/2, 3T/2]     | **50%**                  | Half-and-half |
| U = 3T/4        | [3T/4, 7T/4]    | **25%**                  | Mostly greedy |
| U = T           | [T, 2T-1]       | **0%**                   | Pure greedy (safety valve) |
| U > T           | [U, U+T-1]       | **0%**                   | Pure greedy (safety valve) |

### Key Properties

1. **Linear decay**: undersampled probability decreases linearly from 100% to 0% as U goes from 0 to T
2. **Braking starts immediately**: no period of pure undersampled mode, preventing momentum buildup
3. **Never fully locked**: even at U = T, the counter C provides occasional undersampled chances
4. **Safety valve**: if memory spikes above T (external allocation), immediately go full greedy

### Why This Works Better Than Binary Switching

- **No momentum buildup**: braking starts from the first byte of memory usage, preventing freight trains
- **Linear decay**: smooth transition across the entire range [0, T], not just near the limit
- **No permanent stuck states**: even in greedy mode, undersampled gets occasional chances (counter C provides mixing)
- **Absolute thresholds**: based on OCaml heap usage in MB, not system memory percentage

## Design: Runtime_events-Based Measurement

The selector uses `Searchspace.heap_usage_mb()` (from the new `runtime_events_mem` module) for zero-overhead heap measurement:

```ocaml
(* Zero-impact monitoring via Runtime_events ring buffer *)
Searchspace.poll_runtime_events ();  (* update counters from ring buffer *)
let heap_mb = Searchspace.heap_usage_mb () in  (* pool_live + large_alloc words * 8 bytes *)
```

No GC pause required — counters are updated by the runtime during normal GC cycles.

## Selector Implementation

### Core Logic

```ocaml
let gradual_braking_selector ?(threshold_mb = 8000.0) () (node : 'a node) : int =
  (* Update Runtime_events counters *)
  Searchspace.poll_runtime_events ();
  
  (* Current heap usage in words (native int, full precision) *)
  let u_words = Searchspace.heap_usage_words () in
  
  (* Threshold converted from MB to words once at selector creation *)
  let t_words = int_of_float (threshold_mb *. 1024.0 *. 1024.0 /. Float.of_int Sys.word_size) in
  
  (* Increment call counter (wraps on overflow) *)
  incr selector_call_counter;
  let c = !selector_call_counter in
  
  (* Formula: U + (C mod T) < T, all in words *)
  let should_undersample = u_words + (c mod t_words) < t_words in
  
  if should_undersample then undersampled_selector node
  else greedy_selector node
```

### Counter Management

The counter `C` must be shared across all selector invocations (not per-selector-instance). It provides the "mixing" that makes the transition gradual:

- Without C (or with C=0 always): `U + 0 < T` → binary switch at U = T
- With incrementing C: `U + (C mod T)` → gradual linear blend as explained above

The counter wraps naturally on `int` overflow (63-bit on 64-bit OCaml), which is fine — the formula still works correctly with wrapped values.

### Threshold Selection

The threshold `T` is in absolute MB, so it's independent of system memory size:
- On oracle2 (24.5GB RAM): `T = 8000` (8GB) might be appropriate
- On smaller machines: `T = 4000` (4GB) might be better
- The gradual braking starts at `T/2 = 4000` MB (half the threshold)

## Acceptance Criteria

### 10.1 Selector Function

1. **Same signature as existing selectors**:
   - `gradual_braking_selector : ?threshold_mb:float -> unit -> 'a node -> int`
   - Can be passed directly to `create ~selector:gradual_braking_selector tree`

2. **Gradual blending behavior**:
   - At low heap usage (U << T): 100% undersampled
   - At U = T/2: ~50% undersampled, 50% greedy
   - At U = T: 0% undersampled (pure greedy)
   - At U > T: 0% undersampled (safety valve)

3. **Counter provides mixing**:
   - The call counter C must increment on each selector invocation
   - Without the counter, behavior degrades to binary switching at U = T

4. **Uses Runtime_events for measurement**:
   - Calls `Searchspace.poll_runtime_events()` before reading heap usage
   - Uses `Searchspace.heap_usage_mb()` for zero-overhead measurement

### 10.2 Gradual Braking Verification

5. **No sudden mode switches**:
   - Run selector with growing heap usage
   - Verify undersampled probability decreases smoothly, not in steps

6. **Less overshoot than binary selector**:
   - Compare memory usage of gradual braking vs binary switching
   - Gradual braking should stabilize at lower peak memory

7. **Safety valve works**:
   - If heap usage spikes above T (simulated), selector immediately goes greedy

### 10.3 Performance

8. **Zero overhead from measurement**:
   - `Searchspace.poll_runtime_events()` reads from ring buffer, no GC pause
   - Selector performance should match existing selectors

9. **Counter increment is cheap**:
   - Simple `incr` on a ref, no synchronization needed (single-threaded solver)

### 10.4 Testing

10. **Tests verify formula behavior**:
    - Simulate different U values and counter C values
    - Verify undersampled probability matches expected ranges

11. **Tests verify gradual transition**:
    - Run selector with increasing heap usage (simulated)
    - Verify smooth decrease in undersampled probability

12. **Integration test with solver**:
    - Run `estimate_polyomino` with gradual braking selector
    - Verify memory stays below threshold and estimates remain accurate

## Implementation Process (TDD)

### Phase 1: Formula Tests

```ocaml
let%test_module "gradual_braking_formula" = (module struct
  
  (* Test: at U=0, undersampled fires 100% of the time *)
  let test_undersampled_at_zero_usage () = 
    (* U=0, any C: 0 + (C mod T) = C mod T < T → always true *)
    ...
  
  (* Test: at U=T/4, undersampled fires 75% of the time *)
  let test_undersampled_at_quarter_threshold () = 
    (* U=T/4, expression ranges [T/4, 5T/4] *)
    (* C mod T takes values 0..T-1, so < T when value < 3T/4 *)
    (* Probability ≈ (3T/4) / T = 75% *)
    ...
  
  (* Test: at U=T/2, undersampled fires ~50% of the time *)
  let test_undersampled_at_half_threshold () = 
    (* U=T/2, expression ranges [T/2, 3T/2] *)
    (* C mod T takes values 0..T-1, so < T when value < T/2 *)
    (* Probability ≈ (T/2) / T = 50% *)
    ...
  
  (* Test: at U=3T/4, undersampled fires ~25% of the time *)
  let test_undersampled_at_three_quarters_threshold () = 
    (* U=3T/4, expression ranges [3T/4, 7T/4] *)
    (* C mod T takes values 0..T-1, so < T when value < T/4 *)
    (* Probability ≈ (T/4) / T = 25% *)
    ...
  
  (* Test: at U=T, undersampled fires 0% of the time *)
  let test_undersampled_at_threshold () = 
    (* U=T, expression ranges [T, 2T-1], always >= T *)
    ...
  
  (* Test: at U>T, undersampled fires 0% of the time (safety valve) *)
  let test_undersampled_above_threshold () = 
    (* U>T, expression ranges [U, U+T-1], always > T *)
    ...
end)
```

### Phase 2: Selector Integration Tests

**Testing Strategy**: Follow the same pattern as `memory_aware_selector` — inject a mockable
heap usage function so tests can simulate different memory states without needing Runtime_events.
This mirrors the `memfree` injection pattern in `memory_aware_selector`:

```ocaml
let memory_aware_selector ?(threshold = 0.8) ?(memfree=memfree) () (node : 'a node) : int =
  let free_ratio = memfree () in
  ...
```

The gradual braking selector uses the same pattern with an injectable `heap_usage_words`:

```ocaml
let gradual_braking_selector ?(threshold_mb = 8000.0) 
                             ?(heap_usage_words=Searchspace.heap_usage_words)
  () (node : 'a node) : int =
  let u_words = heap_usage_words () in
  ...
```

Tests mock `heap_usage_words` to simulate specific memory states:

```ocaml
let%test_module "gradual_braking_selector" = (module struct
  
  (* Test: selector uses injectable heap_usage_words *)
  let test_selector_uses_heap_usage_words () = 
    (* Create selector with mock heap_usage_words *)
    let mock_heap = ref 1000 in
    let selector = gradual_braking_selector ~threshold_mb:8000.0 
      ~heap_usage_words:(fun () -> !mock_heap) () in
    (* Verify selector reads from mock, not Runtime_events *)
    ...
  
  (* Test: gradual transition from undersampled to greedy *)
  let test_gradual_transition () = 
    (* Create selector with mock that returns increasing heap usage *)
    let current_heap = ref 0 in
    let selector = gradual_braking_selector ~threshold_mb:8000.0 
      ~heap_usage_words:(fun () -> !current_heap) () in
    (* Simulate heap growing from 0 to 16000 words *)
    let undersampled_count = ref 0 in
    for i = 1 to 1000 do
      current_heap := (i * 16) ; (* 0, 16, 32, ... 15984 *)
      (* Call selector on a dummy node and track result *)
      ...
    done;
    (* Verify undersampled probability decreases linearly from 100% to 0% *)
    ...
  
  (* Test: counter provides mixing *)
  let test_counter_provides_mixing () = 
    (* Without counter increment (C=0): binary switch at U=T *)
    (* With counter increment: gradual linear blend from 100% to 0% *)
    ...
end)
```

### Phase 3: Integration with Solver

Manual testing recommended for formula verification:
- Run `estimate_polyomino` with gradual braking selector
- Monitor heap usage via Runtime_events between batches
- Verify memory stabilizes below threshold, not overshooting
- Compare with binary switching to verify less overshoot

Automated tests can verify:
- Selector API works correctly with injectable heap_usage_words
- Counter increments on each selector invocation
- Threshold conversion from MB to words is correct

## Implementation Details

### Counter Scope

The counter must be **module-level** (shared across all selector invocations), not per-selector-instance:

```ocaml
let gradual_braking_call_counter : int ref = ref 0

let gradual_braking_selector ?(threshold_mb = 8000.0) 
                             ?(heap_usage_words=Searchspace.heap_usage_words)
  () (node : 'a node) : int =
  incr gradual_braking_call_counter;
  let c = !gradual_braking_call_counter in
  let u_words = heap_usage_words () in
  ...
```

This ensures the counter increments continuously across all selector calls, providing the mixing needed for gradual blending.

### Injectability (Testing Pattern)

Follow the same pattern as `memory_aware_selector` — inject a mockable heap usage function:

```ocaml
let gradual_braking_selector ?(threshold_mb = 8000.0) 
                             ?(heap_usage_words=Searchspace.heap_usage_words)
  () (node : 'a node) : int =
```

- **Default**: `heap_usage_words=Searchspace.heap_usage_words` — uses Runtime_events in production
- **Test mock**: `heap_usage_words:(fun () -> 4096)` — simulates fixed heap usage for testing
- Tests can vary `heap_usage_words` between calls to simulate memory growth without Runtime_events

### Counter Overflow

On 64-bit OCaml, `int` is 63 bits (one bit for the tag), so it wraps at ~4.6 quadrillion. This is fine — the formula still works correctly with wrapped values:

```
C mod T is periodic in C, so overflow just continues the cycle
```

### Runtime_events Polling Frequency

`Searchspace.poll_runtime_events()` should be called **once per selector invocation** (or cached within a batch). The Runtime_events ring buffer is updated by the runtime during normal GC cycles, so polling between batches is sufficient.

For performance, consider caching the heap usage within a batch:

```ocaml
let cached_heap_mb : float ref = ref 0.0
let cached_poll_batch : int ref = ref 0

let get_heap_mb current_batch =
  if !cached_poll_batch <> current_batch then begin
    Searchspace.poll_runtime_events ();
    cached_heap_mb := Searchspace.heap_usage_mb ();
    cached_poll_batch := current_batch
  end;
  !cached_heap_mb
```

### Threshold Tuning

The threshold `T` is in absolute MB, so it's machine-independent:
- **Default**: 8000 MB (8GB) — reasonable for oracle2's 24.5GB RAM
- **Lower threshold**: 4000 MB (4GB) — for smaller machines or more conservative operation
- **Braking starts**: immediately from U=0 — linear decay across the entire range

## Files to Modify

- `searchspace/stochastic_estimator.ml` - implementation (add `gradual_braking_selector` function, replace `memory_aware_selector`)
- `searchspace/stochastic_estimator.mli` - interface update (export `gradual_braking_selector`, deprecate `memory_aware_selector`)
- `estimate_polyomino.ml` - use new selector (change one line)

## Files to Create

- Tests inline in `stochastic_estimator.ml` using `%test_module` and `[%expect_test]`

## Dependencies

- **Task 8 (Memory-Aware Selector)**: This task replaces the existing `memory_aware_selector`
- **Runtime_events memory module**: New `searchspace/runtime_events_mem.ml` provides zero-overhead heap measurement
- **Task 6 (Pruning)**: Gradual braking only works if completed branches can be pruned

## Design Notes

### Why Not Binary Switching?

Binary switching causes massive overshoot:
- By the time brakes are applied, hundreds of thousands of subtrees are open
- Memory keeps growing until steady state is reached at a much higher level
- The "freight train" effect: momentum from undersampled mode can't be stopped instantly

### Why Gradual Braking?

Gradual braking prevents overshoot:
- Undersampled branches are phased out linearly as U approaches T
- Braking starts immediately, not at T — prevents momentum buildup from the start
- No period of pure undersampled mode → no freight train effect

### Why Runtime_events?

Runtime_events provides zero-overhead heap measurement:
- No GC pause (unlike `Gc.stat()`)
- Actual live memory counters (unlike `Gc.quick_stat()` which is useless)
- Updated by the runtime during normal GC cycles — no extra cost

### Why Absolute Thresholds?

Absolute thresholds (MB) are machine-independent:
- `/proc/meminfo` percentages vary by system RAM size
- OCaml heap usage is what matters for the selector, not system memory
- Same threshold works on oracle2 (24.5GB) and smaller machines

### Why the Formula `U + (C mod T) < T`?

The formula provides linear decay blending:
- **Linear decay**: undersampled probability decreases linearly from 100% to 0%
- **Braking starts immediately**: no period of pure undersampled mode, preventing momentum buildup
- **Self-regulating**: as U grows, undersampled probability decreases proportionally
- **Never fully locked**: counter C provides occasional undersampled chances even at U = T
- **Safety valve**: if U > T, immediately go greedy (no more cans of worms)
- **Simple to implement**: just one comparison, no complex threshold adjustments
