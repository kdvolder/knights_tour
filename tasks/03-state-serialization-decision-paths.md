# Task 3: State Serialization - Decision Path Encoding

## Goal

Implement serialization of the estimator's materialized tree state using decision paths. The serialized state should capture enough information to resume estimation without repeating already-explored nodes.

## Background

The estimator maintains a tree of `'a node` values, where each node represents a position in the search space. Currently, there's no way to save progress and resume later.

The key insight is: **we don't need to serialize the full node content, just the decision path that leads to each materialized node**. Given a search space and a sequence of decisions (e.g., `[0/3, 1/2, 0/4]`), we can recreate any node by following those decisions in the search space.

### Decision Path Example

For a search space like:
```ocaml
let* n1 = int_range 1 3 in    (* choices: [0/3], [1/3], [2/3] *)
let* n2 = int_range 1 3 in    (* choices: [0/3], [1/3], [2/3] *)
return (n1 + n2)
```

A decision path `[0/3, 2/3]` means:
- At first fork: chose index 0 (n1 = 1)
- At second fork: chose index 2 (n2 = 3)
- This leads to the Result node with value 4

## Acceptance Criteria

### 3.1 Decision Path Representation

1. **Decision path is serializable**:
   - Each decision records: `{chosen_index: int; total_choices: int}`
   - Path is a list of decisions from root to node

2. **Decision path uniquely identifies a position in the search tree**:
   - Same path + same search space = same node (deterministic)
   - Different paths = different nodes

3. **Decision path can be replayed**:
   - Given a search space and decision path, we can recreate the node at that position

### 3.2 Serialization Format

4. **Serialization produces a string or bytes**:
   - Prefer JSON or sexp format for human readability
   - Binary format acceptable if size is critical

5. **Serialization includes all necessary state**:
   - All materialized nodes (identified by their decision paths)
   - Node statistics: samples, estimates, isCompleted flag
   - Root-level metadata (selector type, etc.)

6. **Serialization is deterministic**:
   - Same state always produces same serialized output
   - Order of nodes in serialization should be consistent (e.g., BFS order)

### 3.3 Roundtrip Verification

7. **Serialize then deserialize produces equivalent state**:
   - Deserialized estimator has same estimates as original
   - Deserialized estimator can continue sampling from saved point

## Implementation Process (TDD)

### Phase 1: Decision Path Tests

```ocaml
let%test_module "decision_path" = (module struct
  (* Test: decision path for simple search space *)
  let test_simple_path () = 
    (* int_range 1 3 => paths: [0/3], [1/3], [2/3] *)
    ...
  
  (* Test: decision path for nested search space *)
  let test_nested_path () = 
    (* int_range 1 2 ++ int_range 1 2 => paths: [0/2, 0/2], etc. *)
    ...
  
  (* Test: replaying decision path recreates correct node *)
  let test_replay_path () = 
    (* Given path [1/3, 2/3], verify it leads to n1=2, n2=3 *)
    ...
  
  (* Test: empty path = root node *)
  let test_empty_path_is_root () = ...
end)
```

### Phase 2: Serialization Tests

```ocaml
let%test_module "serialization" = (module struct
  (* Test: serialize empty estimator *)
  let test_serialize_empty () = ...
  
  (* Test: serialize simple materialized tree *)
  let test_serialize_simple_tree () = 
    (* Create estimator, sample once, serialize *)
    (* Verify output contains expected decision paths and stats *)
    ...
  
  (* Test: serialization format is valid JSON/sexp *)
  let test_valid_format () = ...
  
  (* Test: serialization is deterministic *)
  let test_deterministic_serialization () = 
    (* Serialize same state twice, compare *)
    ...
end)
```

### Phase 3: Deserialization Tests

```ocaml
let%test_module "deserialization" = (module struct
  (* Test: deserialize empty state *)
  let test_deserialize_empty () = ...
  
  (* Test: deserialize produces valid estimator *)
  let test_deserialize_valid () = 
    (* Serialize, deserialize, verify can sample *)
    ...
  
  (* Test: roundtrip preserves estimates *)
  let test_roundtrip_estimates () = 
    (* Create -> sample -> serialize -> deserialize -> estimates *)
    (* Compare pre/post roundtrip estimates *)
    ...
  
  (* Test: resumed estimation continues correctly *)
  let test_resume_sampling () = 
    (* Create -> sample 100 -> serialize -> deserialize *)
    (* -> sample 100 more -> verify total samples = 200 *)
    ...
end)
```

## Design Notes

### Decision Path Structure

```ocaml
type decision = {
  chosen_index : int;    (* Which choice was made (0-indexed) *)
  total_choices : int;   (* How many choices were available *)
}

type decision_path = decision list  (* From root to node, oldest first *)
```

### State Structure for Serialization

```ocaml
type serialized_state = {
  version : int;                    (* Schema version for future compatibility *)
  materialized_nodes : node_entry list;
}

and node_entry = {
  path : decision_path;             (* Decision path to this node *)
  samples : int;
  nodes_estimate : float;
  fail_estimate : float;
  solution_estimate : float;
  materialized_nodes_count : int;
  is_completed : bool;
}
```

### Reconstruction Algorithm

To deserialize:
1. Parse serialized state
2. Create empty estimator (root node only)
3. For each `node_entry` in order:
   a. Replay the decision path to find/create nodes along the way
   b. Create child node if not already materialized
   c. Set statistics on the target node

### Important Considerations

1. **Lazy nodes**: The search space may contain `Lazy` nodes that need to be evaluated during replay. This is fine because we're replaying decisions in the same search space context.

2. **Non-deterministic search spaces**: If the search space has side effects or randomness, replaying decisions may not produce identical nodes. This is a fundamental limitation - the search space must be deterministic for state persistence to work correctly.

3. **Selector state**: If the selector uses random state (e.g., `Random.int`), we should either:
   - Save and restore the random seed, OR
   - Use a deterministic selector for resumable estimation

4. **Memory considerations**: For very large materialized trees, serialization could be expensive. Consider:
   - Streaming serialization for large states
   - Compression if file size matters

## Files to Modify

- `searchspace/stochastic_estimator.ml` - implementation
- `searchspace/stochastic_estimator.mli` - interface update

## Files to Create

- Tests inline in `stochastic_estimator.ml` using `%test_module` and `[%expect_test]`
