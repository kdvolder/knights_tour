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

## Testing Strategy

**Top-down, round-trip first.** The workflow is `let%expect_test` with empty `[%expect{||}]`, run `dune test`, inspect output, promote when correct.

### Primary Test: Round-trip (the proof)

One end-to-end test that creates a tree, samples it, serializes to file, deserializes from file, resumes sampling, and proves the result matches a single-run completion:

```ocaml
let%expect_test "roundtrip: serialize/deserialize/resume produces same result as single run" =
  (* Create a non-trivial search space *)
  let* n1 = int_range 1 3 in
  let* n2 = int_range 1 3 in
  return (n1 + n2) |?> (fun x -> x > 3)

  (* Run A: sample to completion in one shot *)
  let est_a = create space in
  ignore (sample 1000 est_a);
  let results_a = estimates est_a in

  (* Run B: sample partway, serialize to file, deserialize, resume *)
  let est_b = create space in
  ignore (sample 100 est_b);
  save_state "test_save.sexp" est_b;
  let est_b_resumed = load_state space "test_save.sexp" in
  ignore (sample 1000 est_b_resumed);
  let results_b = estimates est_b_resumed in

  (* Compare *)
  Printf.printf "Run A (single shot): nodes=%.0f fails=%.0f sols=%.0f mat=%d\n"
    results_a.nodes results_a.fails results_a.solutions results_a.materialized_nodes;
  Printf.printf "Run B (roundtrip):   nodes=%.0f fails=%.0f sols=%.0f mat=%d\n"
    results_b.nodes results_b.fails results_b.solutions results_b.materialized_nodes;
  [%expect{| |}]
```

### Edge Case Tests (insurance)

A few tests for important edge cases:
- Empty estimator (no samples, just root)
- Already-complete tree
- Tree with pruned branches
- Resume with different selector than original

### Isolated Tests (debugging aids only)

Add isolated tests **only when needed** for debugging during implementation. For example:
- If you need to debug path replay, add an isolated test for that
- If serialization output looks wrong, print it to verify format

These are not "important" — they're debugging tools. The round-trip test is the proof of correctness.

### Test Data

Use small synthetic search spaces for fast tests (e.g., `int_range 1..3`). The goal is correctness, not scale. A tree with ~20 nodes is sufficient to prove the round-trip works.

## Design Notes

### Decision Path Structure

```ocaml
type decision = {
  chosen : int;    (* Which choice was made (0-indexed) *)
  choices : int;   (* How many choices were available — consistency check during deserialization *)
}

type decision_path = decision list  (* From root to node, oldest first *)
```

### State Structure for Serialization

**`node_entry` is a real OCaml type — small enough to create and write immediately.**
**`serialized_state` is conceptual only — it does not exist as an implementation type.** The serialization function writes entries one by one directly to the output stream, never collecting them into a wrapper record.

```ocaml
type node_entry = {
  path : decision_path;             (* Decision path to this node *)
  num_choices : int;                (* Number of children at THIS node — needed because the last decision in path describes the PARENT, not this node *)
  samples : int;
  nodes_estimate : float;
  fail_estimate : float;
  solution_estimate : float;
  materialized_nodes_count : int;
  pruned_nodes : int;               (* Number of nodes freed by pruning in this subtree *)
  is_completed : bool;
}
```

**Note**: `num_choices` IS needed per entry. The last decision in a path tells you about the PARENT node (how many choices it had), not the target node itself. For leaf nodes (Result/Fail), `num_choices = 0`. For fork nodes, it's the number of children. During deserialization, this lets us size the children array without calling `inspect`.

### Reconstruction Algorithm

**Serialization**: Walk the tree (BFS or DFS), and for each materialized node, create a `node_entry` and immediately write it as sexp to the output channel. No intermediate collection of entries, no `serialized_state` wrapper.

**Basic Deserialization (included in this task for round-trip testing)**:
1. Parse serialized state from file — read entries one by one
2. Create root node from search space (call `inspect` once)
3. Sort entries by path depth (shallow first) — or write them in BFS order during serialization
4. For each entry:
   a. Replay the decision path to find/create nodes along the way
   b. Create child node if not already materialized (call `inspect` on each new node)
   c. Set statistics on the target node
5. Return new estimator with reconstructed root

### Important Considerations

1. **Lazy nodes**: The search space may contain `Lazy` nodes that need to be evaluated during replay. This is fine because we're replaying decisions in the same search space context.

2. **Non-deterministic search spaces**: If the search space has side effects or randomness, replaying decisions may not produce identical nodes. This is a fundamental limitation — the search space must be deterministic for state persistence to work correctly.

3. **Streaming is mandatory**: Serialization MUST stream directly to disk — no in-memory collection of entries, no building giant strings or byte buffers. For a tree with 260K+ nodes, buffering everything in memory would double the process's memory footprint and could hit OCaml string length limits. The implementation walks the tree and writes each `node_entry` to an output channel as it goes.

## Files to Modify

- `searchspace/stochastic_estimator.ml` - implementation
- `searchspace/stochastic_estimator.mli` - interface update

## Files to Create

- Tests inline in `stochastic_estimator.ml` using `%test_module` and `[%expect_test]`

## Design Questions (Answered/Decided)

### Q1: Decision Path Structure — `chosen` vs `chosen_index`
**Question**: The existing `decision` type in the code uses `{ chosen: int; choices: int }`. The task doc proposes `{ chosen_index: int; total_choices: int }`. Should we rename to match the task doc, or keep the existing names?

**Answer**: Keep existing names `{ chosen: int; choices: int }`. Only `chosen` is strictly needed for replay, but keeping `choices` provides a consistency check during deserialization — if paths come from a different search space structure, the `choices` values will mismatch and reveal the problem.

### Q2: Selector Serialization — How to handle the function type?
**Question**: The estimator stores a `selector : 'a child_selector` which is a function (`'a t -> 'a node -> int`). Functions can't be serialized. Options:
- (a) Serialize selector as a string tag (`"undersampled"`, `"greedy"`, etc.) and reconstruct on load
- (b) Pass selector separately during deserialization, don't serialize it at all
- (c) Always use a default selector on resume

**Answer**: (b) — The selector is not part of the tree structure. It's passed separately during deserialization, allowing you to reconstruct a tree state and continue exploring it with a different selector. This doesn't change the solutions that exist in the search space — only the order of exploration for resumed work.

### Q3: Serialization Format — sexp vs JSON?
**Question**: The task mentions "prefer JSON or sexp format for human readability". The project already has `ppx_sexp_conv` available (sexplib0 in opam). Which format?

**Answer**: Sexp, written via **streaming to file** — not building in-memory blobs. The approach:
- Walk the tree (BFS or DFS) and serialize each node entry as we visit it
- Use `Format` with a formatter backed by an output channel (file) to stream sexp directly
- No intermediate collection of all entries, no sorting in memory, no giant string
- For 450K nodes: we walk the tree once and write each entry to disk as we go
- `ppx_sexp_conv` generates `to_sexp_t` functions that work with any `Format.formatter`, so we just point it at a file formatter
- This keeps memory usage proportional to the tree depth (recursion stack), not tree size

### Q4: Scope Boundary — Should Task 3 define `deserialize` signature?
**Question**: Task 3 is serialization only. Should it also declare the `deserialize` function signature in the interface (implementation deferred to Task 4), or leave it entirely for Task 4?

**Answer**: Basic deserialization is included in this task — you can't verify serialization works correctly without round-trip tests. The interface will include both `serialize` and a basic `deserialize`. Task 4 is re-scoped to handle advanced aspects (lazy node views, error handling, file I/O helpers).

### Q5: What Gets Serialized — Root Node Included?
**Question**: Should the root node be included in `materialized_nodes` list, or is it implicit? The root always exists and its stats are derived from children. But the root also has `samples` which is important for resuming.

**Answer**: Always include the root node in the entries list. No special cases — same treatment as every other materialized node. The tiny overhead of serializing one extra entry is not worth the added complexity and risk of bugs from treating root differently.

### Q6: Pruned Nodes — How to represent?
**Question**: When a branch is pruned, `children` becomes `[||]`. Should we serialize:
- (a) Just `is_completed = true` with empty children array, OR
- (b) A special "pruned" flag that distinguishes pruned from naturally completed?

**Answer**: `is_completed` is sufficient — a pruned node IS a completed node. No special flag needed.

### Q7: Unmaterialized Children — Implicit or Explicit?
**Question**: Unmaterialized children are `None` in the array. Should they be explicitly listed as entries with no statistics, or just omitted (implied by missing paths)?

**Answer**: Not serialized at all. Unmaterialized children don't exist in the tree — there's nothing to write about them. They're implicitly `None` because no path leads to them. During deserialization, children arrays are sized by `num_choices`, and only the slots filled in by replayed paths get populated. Everything else stays `None`.

### Q8: Deterministic Ordering — BFS vs DFS?
**Question**: For deterministic serialization, node entries must be in a consistent order. Options:
- (a) BFS by path depth, then lexicographic by path
- (b) DFS pre-order traversal of the tree
- (c) Sorted by path length, then by path elements

**Answer**: DFS pre-order traversal. Since we stream entries directly to disk during the walk (no intermediate collection), the traversal order IS the output order. DFS is natural for a recursive tree walk and requires no sorting or reordering.

### Q9: Version Number — What to start at?
**Question**: The `version` field in `serialized_state`. Start at 1? Or 0?

**Answer**: Version 1 — first version starts at 1.

### Q10: `on_solution` Callback — Serialize or not?
**Question**: The estimator stores an `on_solution : 'a -> unit` callback. This is a function and can't be serialized. Should:
- (a) It be passed separately during deserialization, OR
- (b) Be omitted from serialization (solutions found after resume won't trigger the callback, which is fine since they're new samples)?

**Answer**: Like the selector — not serialized. It has nothing to do with tree structure, only with processing of future events (solutions found after resume). Passed separately during deserialization.

### Q11: `pruned_nodes` Field — Serialize or not?
**Question**: The task doc's `node_entry` includes `pruned_nodes`. This is a running counter of freed nodes. Should it be serialized?

**Answer**: Yes — `pruned_nodes` is an important statistic that should be serialized and deserialized. It tracks how many nodes were freed by pruning, which is relevant for understanding the estimator's memory usage history.

**Implementation note**: Add `pruned_nodes : int` to the `node_entry` type.

### Q12: Lazy Node View Reconstruction During Deserialization
**Question**: Materializing a node's `node_view` (via `inspect`) is expensive — it involves updating board positions, checking validity, etc. With 260K materialized nodes taking ~3 hours to build, eagerly reconstructing all node views during deserialization would take equally long. Should:
- (a) Reconstruction eagerly call `inspect` on every node along the paths (O(nodes), still slow)
- (b) Reconstruction create a skeleton only — store `num_choices` per node_entry, size children arrays accordingly, defer all `inspect` calls until sampling reaches that node
- (c) Something else?

**Answer**: Split into two phases:
- **Phase 1 (this task)**: Eager reconstruction — call `inspect` on every node during deserialization. Simple, correct baseline for round-trip testing.
- **Phase 2 (Task 4)**: Lazy node views — store `num_choices` in the serialized format, create skeleton nodes during deserialization, defer `inspect` until sampling reaches a node. This is an optimization for frequent resume scenarios.
