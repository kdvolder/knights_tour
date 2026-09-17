# Task 12: Asymptotically Sliding Threshold

> **Status**: Not Started
> **Date**: 2026-08-11

## Goal

Replace the fixed threshold in `gradual_braking_memory_aware_selector` with an asymptotically growing threshold that keeps receding over time. This ensures undersampled mode always has a non-zero probability of being selected, preventing the "completely stuck in greedy" scenario.

## Background

The current gradual braking selector uses a fixed threshold (e.g., 100,000 net nodes). The formula is:

```
P(undersampled) = 1 - (memory_pressure / threshold)
```

When `memory_pressure >= threshold`, undersampled probability hits 0 and the solver is stuck in greedy mode forever. In practice, this happened after ~150K batches on oracle2 — net nodes stabilized around 550K, threshold was 100K, and the solver found zero new solutions for ~35 days.

Raising the threshold just moves the goal post — you get another week of "growth" before hitting the same wall.

### The Sliding Threshold Idea

Instead of a fixed threshold, make it grow asymptotically over time:

```
threshold(t) = base_threshold * (1 - 1/(t + 1))
```

Where `t` is elapsed time or batch count. This gives:
- **Early on**: threshold is low → undersampled dominates (good, explore first)
- **Later**: threshold grows toward `base_threshold` → greedy gets more usage (good, exploit what you know)
- **Never**: threshold never reaches `base_threshold` → undersampled always has a non-zero chance

The formula `1 - 1/(t+1)` approaches 1 asymptotically:
- t=0 → factor = 0 (threshold = base_threshold * 0 = 0, pure undersampled)
- t=1 → factor = 0.5 (threshold = base_threshold * 0.5)
- t=9 → factor = 0.9 (threshold = base_threshold * 0.9)
- t=99 → factor = 0.99 (threshold = base_threshold * 0.99)
- t→∞ → factor → 1 (threshold → base_threshold, but never reaches it)

### Why This Works

The key insight: the threshold doesn't need to reach a maximum. It just needs to **never stop growing**. Even at 550K net nodes with base_threshold=1M, the sliding threshold would have grown (say) to 900K by now, keeping undersampled alive.

### Tradeoffs

- **Pros**: Guarantees you never fully stop exploring; simple formula; no external "diversity" mechanism needed
- **Cons**: Exploration is blind — it doesn't guide *where* to explore, just ensures you do explore
- **Best used with**: Task 11 (promising selector) — sliding threshold ensures new regions are discovered; promising selector guides *which* of those regions to pursue

## Acceptance Criteria

### 12.1 Sliding Threshold Implementation

1. **Threshold grows over time**:
   - `threshold(t) = base_threshold * (1 - 1/(t + 1))`
   - `t` can be batch count, elapsed time in seconds, or total samples — needs to monotonically increase
   - Threshold approaches `base_threshold` asymptotically but never reaches it

2. **Same interface as current selector**:
   - `asymptotic_braking_memory_aware_selector ~base_threshold ~memory_pressure`
   - Returns `(selector, stats_accessor)` tuple (same as current)

3. **Backwards compatible**:
   - When `t` is large (e.g., > 10,000), behavior approaches the fixed-threshold version
   - Existing code using gradual braking should work with minimal changes

### 12.2 Testing

4. **Tests verify threshold growth**:
   - At t=0, P(undersampled) = 1.0 (pure undersampled)
   - At t=9, P(undersampled) ≈ 0.1 (threshold = 0.9 * base_threshold)
   - At t=99, P(undersampled) ≈ 0.01 (threshold = 0.99 * base_threshold)
   - Threshold never reaches `base_threshold` exactly

5. **Tests verify selector behavior**:
   - Early batches: mostly undersampled selections
   - Later batches: mix of undersampled and greedy, with undersampled probability decreasing over time
   - Undersampled probability never reaches exactly 0

## Implementation Process (TDD)

### Phase 1: Sliding Threshold Tests

```ocaml
let%expect_test "sliding threshold grows over time" = begin
  (* Verify threshold(t) formula produces expected values *)
  (* t=0 → factor = 0.0, threshold = base * 0.0 *)
  (* t=1 → factor = 0.5, threshold = base * 0.5 *)
  (* t=9 → factor = 0.9, threshold = base * 0.9 *)
  (* t=99 → factor = 0.99, threshold = base * 0.99 *)
end

let%expect_test "selector: undersampled probability decreases over time" = begin
  (* Run selector across multiple batches with increasing t *)
  (* Verify undersampled % decreases but never hits exactly 0 *)
end

let%expect_test "selector: memory pressure still matters" = begin
  (* At same t, higher memory_pressure → lower undersampled probability *)
  (* Verify the ratio u/threshold(t) still controls P(undersampled) *)
end
```

### Phase 2: Integration Tests

```ocaml
let%expect_test "sliding threshold prevents stuck state" = begin
  (* Simulate long-running solver with stable memory pressure *)
  (* Verify undersampled mode continues to activate even after threshold would be exceeded with fixed value *)
end

let%expect_test "sliding threshold works with promising selector" = begin
  (* Use asymptotic braking with promising_selector instead of greedy *)
  (* Verify both undersampled and promising modes get usage over time *)
end
```

## Files to Modify

- `searchspace/stochastic_estimator.ml` - implement `asymptotic_braking_memory_aware_selector`
- `searchspace/stochastic_estimator.mli` - export new selector

## Dependencies

- **Task 10 (Gradual Braking Selector)**: This task replaces/enhances the fixed-threshold gradual braking mechanism
- **Task 11 (Promising Selector)**: Complementary — sliding threshold ensures new regions are discovered; promising selector guides which regions to pursue

## Notes

- This is a safety net, not a primary selection strategy. It guarantees exploration but doesn't direct it.
- The `t` parameter choice matters: batch count gives discrete steps, elapsed time is smoother, total samples is proportional to work done. Batch count or elapsed time are probably best.
- The formula `1 - 1/(t+1)` is one option; other asymptotic functions could work (e.g., `log(t+2)/log(2)` capped at some max). The key property is: monotonically increasing, bounded above, never reaches the bound.
- Could be combined with Task 11: use promising selector in greedy mode, undersampled for exploration, and sliding threshold to ensure undersampled never dies out.
