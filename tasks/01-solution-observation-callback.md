# Task 1: Solution Observation Callback

## Goal

Extend the incremental estimator API to allow an optional callback function that receives solutions as they are discovered during sampling. This enables real-time observation of solutions without waiting for the entire estimation to complete.

## Background

Currently, `stochastic_estimator.ml` has an incremental estimator (`'a t`) with:
- `create : ?selector -> 'a Searchspace.t -> 'a t` - creates estimator
- `sample : int -> 'a t -> bool` - performs n samples, returns true when complete
- `estimates : 'a t -> estimates` - returns current estimates

The internal `walk` function traverses the search tree and counts solutions, but never captures the actual solution values. When a `Result` leaf is reached during sampling, we need to invoke a user-provided callback with the solution value.

## Acceptance Criteria

1. **Callback is optional** - existing code without callback continues to work unchanged
2. **Callback receives solution values** - when a `Result` leaf is reached during sampling, the callback is invoked with the solution value
3. **Callback is called for every solution found** - if sampling finds N solutions, callback is invoked exactly N times
4. **Callback does not break estimator state** - side effects in callback should not corrupt the estimator's internal state
5. **Empty callback is a no-op** - passing `None` or empty callback has zero overhead (or negligible)

## Implementation Process (TDD)

Each phase follows the same rhythm: write tests that **fail** (red), then implement just enough to make them **pass** (green). Move to the next phase only when all tests in the current one pass.

### Phase 1: API Design Tests

**Goal**: Define the callback type and integrate it into `create` so that code compiles with or without a callback.

**Acceptance**: All tests pass — meaning the API exists, compiles, and `create` works in both modes.

```ocaml
(* Test: create with callback *)
let%test_module "solution_callback_api" = (module struct
  (* Test: callback type signature *)
  let test_callback_type () = ...
  
  (* Test: create without callback still works *)
  let test_create_without_callback () = ...
  
  (* Test: create with callback compiles *)
  let test_create_with_callback () = ...
end)
```

### Phase 2: Callback Invocation Tests

**Goal**: Make `walk` invoke the callback when a `Result` leaf is reached. Tests verify the callback fires at the right times with the right values.

**Acceptance**: All tests pass — callback is called exactly once per solution found, receives the correct value, is never called for failures or forks.

```ocaml
let%test_module "callback_invocation" = (module struct
  (* Test: callback is called when solution found in single-walk *)
  let test_callback_called_on_solution () = ...
  
  (* Test: callback receives correct solution value *)
  let test_callback_receives_correct_value () = ...
  
  (* Test: callback called for each solution in multi-solution space *)
  let test_callback_called_for_each_solution () = ...
  
  (* Test: callback not called for failures *)
  let test_callback_not_called_on_failure () = ...
  
  (* Test: empty callback is no-op *)
  let test_empty_callback_no_effect () = ...
end)
```

### Phase 3: Integration Tests

**Goal**: Verify the callback works correctly in realistic usage patterns — different selectors, multiple `sample()` calls, and solution ordering.

**Acceptance**: All tests pass — callback behaves correctly regardless of selector choice, accumulates across multiple `sample()` calls without losing or duplicating solutions.

```ocaml
let%test_module "callback_integration" = (module struct
  (* Test: callback works with undersampled_selector *)
  let test_callback_with_undersampled () = ...
  
  (* Test: callback works with probabilistic_undersampled_selector *)
  let test_callback_with_probabilistic () = ...
  
  (* Test: callback works across multiple sample() calls *)
  let test_callback_across_multiple_samples () = ...
  
  (* Test: callback receives solutions in sampling order *)
  let test_callback_ordering () = ...
end)
```

## Design Notes

### API Option A: Callback in `create`

```ocaml
val create : 
  ?selector:'a child_selector -> 
  ?on_solution:('a -> unit) -> 
  'a Searchspace.t -> 'a t
```

### API Option B: Callback in `sample`

```ocaml
val sample : 
  ?on_solution:('a -> unit) -> 
  int -> 'a t -> bool
```

### Recommendation: Option A (callback in create)

Rationale:
- Callback is typically set once and used throughout the estimation
- Simpler API - no need to pass callback on every sample call
- More natural for "observe solutions as they are found" use case

### Implementation Sketch

1. Add `on_solution` field to `'a node` type (or to `'a t` type)
2. In `walk` function, when reaching a `Result` leaf:
   - If callback exists, invoke it with the solution value
3. Store callback in `'a t` record and pass through to walk

## Files to Modify

- `searchspace/stochastic_estimator.ml` - implementation
- `searchspace/stochastic_estimator.mli` - interface update

## Files to Create

- Tests are inline in `stochastic_estimator.ml` using `[%expect_test]` and `%test_module`
