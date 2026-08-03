# Task 3: State Serialization - Decision Path Encoding

## Goal

Implement serialization of the estimator's materialized tree state using decision paths. The serialized state should capture enough information to resume estimation without repeating already-explored nodes.

## Background

The estimator maintains a tree of `'a node` values, where each node represents a position in the search space. The key insight is: **we don't need to serialize the full node content, just the decision path that leads to each materialized node**. Given a search space and a sequence of decisions (e.g., `[0/3, 1/2, 0/4]`), we can recreate any node by following those decisions in the search space.

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

## Acceptance Criteria — All Met ✓

### 3.1 Decision Path Representation ✅

1. **Decision path is serializable**:
   - Each decision records: `{chosen: int; choices: int}` — matches task spec (Q1)
   - Path is a list of decisions from root to node

2. **Decision path uniquely identifies a position in the search tree**:
   - Same path + same search space = same node (deterministic)
   - Different paths = different nodes

3. **Decision path can be replayed**:
   - `replay_path` navigates the tree using decisions, creating nodes as needed

### 3.2 Serialization Format ✅

4. **Serialization produces a line-based text format**:
   - Version header: `version 1`
   - Each entry on its own line, pipe-delimited fields
   - Human-readable (not sexp/JSON as originally suggested — simpler and sufficient)

5. **Serialization includes all necessary state**:
   - All materialized nodes (identified by their decision paths)
   - Node statistics: samples, estimates, isCompleted flag
   - `num_choices` per entry (needed because last decision describes the PARENT, not this node)

6. **Serialization is deterministic**:
   - DFS pre-order traversal — parents always before children
   - Same state always produces same output

7. **Streaming to file** (mandatory per task spec):
   - `collect_entries` walks the tree recursively, yielding entries via `Seq.t`
   - `save_state` iterates the sequence and writes each entry to output channel
   - No intermediate collection of all entries in memory

### 3.3 Roundtrip Verification ✅

8. **Serialize then deserialize produces equivalent state**:
   - Deserialized estimator has same estimates as original (before resume)
   - Deserialized estimator can continue sampling from saved point
   - Round-trip test proves this: `sample 100 → save → load → sample 1000` matches `sample 2000`

## Implementation Details

### Types (in `stochastic_estimator.ml`)

```ocaml
type decision = {
  chosen: int;    (* Which choice was made (0-indexed) *)
  choices: int;   (* How many choices were available — consistency check during deserialization *)
}

type node_entry = {
  path : decision_path;             (* Decision path to this node *)
  num_choices : int;                (* Number of children at THIS node *)
  samples : int;
  nodes_estimate : float;
  fail_estimate : float;
  solution_estimate : float;
  materialized_nodes_count : int;
  pruned_nodes : int;
  is_completed : bool;
}

and decision_path = decision list
```

### Interface (in `stochastic_estimator.mli`)

```ocaml
val save_state : string -> 'a t -> unit
(** [save_state filename est] serializes the estimator's tree state to a file. *)

val load_state : 'a Searchspace.t -> string -> 'a t
(** [load_state space filename] deserializes an estimator from a file saved by [save_state]. *)
```

### Key Design Decisions (from task spec Q&A)

- **Q1**: Keep `{ chosen; choices }` naming (not `chosen_index/total_choices`)
- **Q2**: Selector not serialized — passed separately during deserialization (defaults to `undersampled_selector`)
- **Q3**: Line-based text format chosen over sexp/JSON (simpler, still human-readable)
- **Q4**: Both `save_state` and `load_state` in this task (can't verify serialization without round-trip)
- **Q5**: Root node IS included in entries (no special case)
- **Q6**: `is_completed` sufficient — no separate "pruned" flag needed
- **Q7**: Unmaterialized children not serialized (implicit `None`)
- **Q8**: DFS pre-order traversal for deterministic ordering
- **Q9**: Version 1
- **Q10**: `on_solution` callback not serialized — defaults to no-op on load
- **Q11**: `pruned_nodes` IS serialized (important memory accounting stat)

### Serialization Algorithm

```
save_state(filename, est):
  open output file
  write "version 1" header
  for entry in collect_entries(est.root, []):   (* DFS pre-order *)
    write entry as pipe-delimited line
  close file

collect_entries(node, path):
  num_choices = count children from node.node_view (forces lazy view)
  yield { path, num_choices, ...node.stats }
  if not node.isCompleted:
    for each child at index i:
      yield collect_entries(child, path + [{chosen=i; choices=num_choices}])
```

### Deserialization Algorithm (see Task 4)

Basic deserialization is included here for round-trip testing. Advanced lazy reconstruction (Task 4) enhances it further.

## Tests

All tests are inline `[%expect_test]` in `stochastic_estimator.ml`:

- **roundtrip**: Main proof — serialize/deserialize/resume produces same result
- **edge case: single-node tree**: No forks, just root
- **edge case: deep tree**: 5 levels of nesting
- **edge case: wide tree**: Many choices at root
- **edge case: partial tree with unmaterialized children**
- **edge case: resume after load accumulates samples correctly**
- **edge case: multiple round-trips**: Save → load → save → load
- **edge case: completed tree survives round-trip**
- **edge case: pruned nodes survive round-trip**
- **edge case: empty file should fail**
- **edge case: wrong version should fail**
- **edge case: malformed line should fail**

## Files Modified

- `searchspace/stochastic_estimator.ml` — implementation + tests
- `searchspace/stochastic_estimator.mli` — interface update
