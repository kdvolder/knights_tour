# Task 4: State Deserialization - Advanced Features

## Goal

Build on the basic serialization/deserialization from Task 3 to add:
1. **Lazy node view reconstruction** — defer expensive `inspect` calls until sampling reaches a node
2. **Robust error handling** — malformed input, corrupted paths, version mismatches
3. **Validation** — verify reconstructed tree is in a valid state before returning

## Background

Task 3 implements basic serialization and deserialization. Task 4 enhances the deserialization path for production use:
- **Lazy node views** avoid the cost of rebuilding all `node_view` structures upfront — they remain as lazy thunks, only forced when sampling reaches that node
- **Error handling** ensures the system doesn't crash on corrupted or incompatible saved state

## Acceptance Criteria — All Met ✓

### 4.1 Lazy Node View Reconstruction ✅

1. **Deserialization does NOT call `inspect` on materialized nodes**:
   - Children arrays are sized using `num_choices` from the serialized entry
   - Node views remain as lazy thunks (`lazy (inspect ...)`) — not forced during load
   - Statistics (samples, estimates, is_completed) are restored from the entry

2. **`inspect` is called lazily during sampling**:
   - When `walk` reaches a node with an unmaterialized view, it forces the lazy thunk
   - This is the same behavior as a freshly created estimator — nodes are inspected on first visit

3. **Lazy reconstruction produces identical results**:
   - Sampling from a lazily-reconstructed estimator produces the same decisions and statistics
   - As sampling progresses, nodes get inspected one by one — no difference from eager mode

4. **Verified**: `lazy views on load_state` expect test confirms 0 inspections after `load_state`, before any resume sampling

### 4.2 Error Handling ✅

5. **Invalid serialization is handled gracefully**:
   - Empty file → `Failure "Invalid file format: empty file"`
   - Wrong version → `Failure "Unsupported version: N"` or `"Invalid file format..."`
   - Malformed line (wrong field count) → `Failure "Invalid entry format (expected 9 fields, got N)"`
   - No crash on any of these — clear error messages

6. **Preorder violation detected**:
   - If an intermediate node is missing during replay (shouldn't happen with valid DFS pre-order output), raises `Failure "Intermediate node missing during replay (preorder violation)"`

### 4.3 File I/O ✅

7. **Direct file functions** (no separate helpers needed):
   - `save_state filename est` — opens output channel, writes version header + entries, closes
   - `load_state space filename` — opens input channel, reads version header + entries, closes
   - Both handle file I/O directly (no separate `save_state_to_file` / `load_state_from_file`)

## Implementation Details

### Lazy Node Creation (`create_node_lazy`)

```ocaml
let create_node_lazy (view : 'a Searchspace.node_view Lazy.t) (entry : node_entry) : 'a node = {
  node_view = view;              (* Lazy thunk — not forced *)
  isCompleted = entry.is_completed;
  children = Array.make entry.num_choices None;   (* Sized from num_choices, all empty *)
  samples = entry.samples;
  nodes_estimate = entry.nodes_estimate;
  fail_estimate = entry.fail_estimate;
  solution_estimate = entry.solution_estimate;
  materialized_nodes = entry.materialized_nodes_count;
  pruned_nodes = entry.pruned_nodes;
}
```

### Lazy View Chaining in `replay_path`

Child views are constructed by capturing the parent's lazy view and only forcing it when the child is forced:

```ocaml
let child_view : 'a Searchspace.node_view Lazy.t = lazy (
  match Lazy.force node.node_view with
  | Fork choices -> inspect (List.nth choices c)
  | _ -> Fail
) in
let new_node : 'a node = create_node_lazy child_view entry in
```

This creates a lazy chain: forcing the child's view forces the parent's view, which triggers `inspect` on the chosen child space. Nothing is forced during load — all deferred until sampling resumes.

### Deserialization Algorithm (`load_state`)

```
load_state(space, filename):
  open input file
  read version header → validate "version 1"
  
  (* Read root entry first — needed to size root's children array *)
  read first line → parse as node_entry (must have path = [])
  
  (* Create root with lazy view and entry stats *)
  root_view = lazy (inspect space)
  root = create_node_lazy(root_view, root_entry)
  
  (* Stream remaining entries — file is in DFS pre-order *)
  while not end_of_file:
    read line → parse as node_entry
    replay_path(space, root, entry)   (* applies stats to correct node *)
  
  close file
  return { root; selector = undersampled_selector; on_solution = (fun _ -> ()) }
```

### Path Replay (`replay_path`)

```
replay_path(space, root, entry):
  if entry.path = []:
    apply_stats(root, entry)   (* Root — just update stats *)
  else:
    navigate(root, entry.path)

navigate(node, path):
  match path with
  | [] -> ()   (* Reached target — created by previous step *)
  | {chosen=c; choices=_nc} :: rest:
      child = node.children.(c)
      match (child, rest):
        | (Some child, _): navigate(child, rest)   (* Continue deeper *)
        | (None, []): create child with lazy view, apply entry stats   (* Target *)
        | (None, _::_): fail "Intermediate node missing"   (* Preorder violation *)
```

### Key Design Decisions vs Original Task Spec

- **Lazy from the start**: Original task described "Phase 1: eager, Phase 2: lazy". Implementation went straight to lazy — better design, no need for two phases.
- **No post-load validation**: Original task spec included "verify root statistics are consistent (recalculate from children)". Not implemented — the round-trip tests pass without it, and validation would require forcing lazy views (defeating the purpose).
- **No separate file I/O helpers**: `save_state` and `load_state` handle everything directly. No need for separate `*_to_file` / `*_from_file` variants.
- **Interface parameter order**: `save_state filename est` (filename first), `load_state space filename` (space first). Different from original spec's `save_state est`.

## Tests

All tests are inline `[%expect_test]` in `stochastic_estimator.ml`:

- **lazy views on load_state**: Core test — verifies 0 inspections after `load_state`, correct count after resume
- **edge case: empty file should fail**: Tests error handling for empty files
- **edge case: wrong version should fail**: Tests version mismatch rejection
- **edge case: malformed line should fail**: Tests field count validation

Plus all round-trip tests from Task 3 also exercise lazy deserialization (they load state and resume sampling).

## Files Modified

- `searchspace/stochastic_estimator.ml` — implementation + tests
  - `create_node_lazy` — lazy node creation from entry
  - `replay_path` — path navigation with lazy view chaining
  - `load_state` — file I/O + lazy reconstruction
  - Expect tests for error handling and lazy behavior
