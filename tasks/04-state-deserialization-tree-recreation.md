# Task 4: State Deserialization - Tree Recreation

## Goal

Implement deserialization of the serialized estimator state, reconstructing the materialized tree so estimation can continue from where it left off.

## Background

Task 3 defines the serialization format and tests for roundtrip correctness. This task focuses on the **deserialization logic** - taking serialized decision paths and statistics, and rebuilding the internal tree structure.

The key challenge is that we need to:
1. Parse decision paths and recreate the tree structure
2. Restore node statistics (samples, estimates)
3. Ensure the reconstructed tree is in a valid state for continued sampling

## Acceptance Criteria

### 4.1 Tree Reconstruction

1. **Reconstructed tree has correct structure**:
   - All materialized nodes from serialization are present
   - Unmaterialized children remain as `None` in the array
   - Parent-child relationships are preserved

2. **Node statistics are restored correctly**:
   - `samples` count matches serialized value
   - `nodes_estimate`, `fail_estimate`, `solution_estimate` match
   - `materialized_nodes` count matches
   - `is_completed` flag is restored

3. **Reconstructed tree is in valid state**:
   - Root statistics are consistent with children
   - `is_completed` flags are transitive (parent completed iff all materialated children completed)
   - Can call `sample` on reconstructed estimator without errors

### 4.2 Continued Sampling

4. **Sampling continues from saved point**:
   - New samples are added to existing statistics
   - Previously materialized nodes are not re-materialized
   - Total samples = saved_samples + new_samples

5. **Completion detection works after resume**:
   - If estimator was complete when saved, `sample 0` returns true immediately
   - If estimator was incomplete, sampling continues until completion

### 4.3 Error Handling

6. **Invalid serialization is handled gracefully**:
   - Malformed JSON/sexp produces clear error message
   - Unknown schema version is rejected or handled with warning
   - Corrupted decision paths don't crash the program

## Implementation Process (TDD)

### Phase 1: Tree Reconstruction Tests

```ocaml
let%test_module "tree_reconstruction" = (module struct
  (* Test: reconstruct single-level tree *)
  let test_reconstruct_single_level () = 
    (* Serialize estimator with one level of materialization *)
    (* Deserialize and verify tree structure *)
    ...
  
  (* Test: reconstruct multi-level tree *)
  let test_reconstruct_multi_level () = 
    (* Serialize deeper tree, verify all levels restored *)
    ...
  
  (* Test: reconstruct preserves unmaterialized children *)
  let test_preserve_unmaterialized () = 
    (* Serialize tree with some unmaterialized children *)
    (* Verify None entries in child arrays *)
    ...
  
  (* Test: reconstruct with empty state *)
  let test_reconstruct_empty () = 
    (* Serialize just-created estimator (no samples) *)
    (* Deserialize and verify root-only tree *)
    ...
end)
```

### Phase 2: Statistics Restoration Tests

```ocaml
let%test_module "statistics_restoration" = (module struct
  (* Test: sample counts restored *)
  let test_sample_counts () = 
    (* Create, sample 100 times, serialize *)
    (* Deserialize, verify root.samples = 100 *)
    ...
  
  (* Test: estimate values restored *)
  let test_estimate_values () = 
    (* Serialize with known estimates *)
    (* Deserialize, verify estimates match *)
    ...
  
  (* Test: is_completed flags restored *)
  let test_completed_flags () = 
    (* Serialize completed estimator *)
    (* Deserialize, verify is_completed = true *)
    ...
  
  (* Test: materialized_nodes count restored *)
  let test_materialized_count () = 
    ...
end)
```

### Phase 3: Continued Sampling Tests

```ocaml
let%test_module "continued_sampling" = (module struct
  (* Test: new samples add to existing *)
  let test_samples_accumulate () = 
    (* Create, sample 100, serialize *)
    (* Deserialize, sample 50 more *)
    (* Verify total samples = 150 *)
    ...
  
  (* Test: estimates update after resume *)
  let test_estimates_update () = 
    (* Create, sample, serialize, deserialize, sample more *)
    (* Verify estimates changed from saved values *)
    ...
  
  (* Test: completion after resume *)
  let test_completion_after_resume () = 
    (* Create, sample until nearly complete, serialize *)
    (* Deserialize, sample remaining *)
    (* Verify completion detected *)
    ...
  
  (* Test: already-complete estimator resumes immediately *)
  let test_already_complete () = 
    (* Create, sample until complete, serialize *)
    (* Deserialize, sample 0 -> should return true *)
    ...
end)
```

### Phase 4: Error Handling Tests

```ocaml
let%test_module "error_handling" = (module struct
  (* Test: malformed input produces error *)
  let test_malformed_input () = 
    (* Try to deserialize garbage string *)
    (* Should raise or return error, not crash *)
    ...
  
  (* Test: unknown version handled *)
  let test_unknown_version () = 
    (* Serialize with version=999 *)
    (* Deserialize should fail gracefully *)
    ...
  
  (* Test: invalid decision path handled *)
  let test_invalid_path () = 
    (* Path with chosen_index >= total_choices *)
    (* Should fail gracefully *)
    ...
end)
```

## Design Notes

### Reconstruction Algorithm

```
deserialize(serialized_state, search_space):
  1. Create root node from search_space (unmaterialized)
  
  2. Sort node_entries by path depth (shallow first)
  
  3. For each entry in sorted order:
     a. Start at root
     b. For each decision in path:
        i.  Get child index from decision.chosen_index
        ii. If child is None, create it and store in parent's children array
        iii. Move to child node
     c. Set statistics on target node:
        - samples, estimates, is_completed, materialized_nodes_count
  
  4. Verify root statistics are consistent
     (recalculate from children and compare)
  
  5. Return new estimator with reconstructed root
```

### Key Implementation Details

1. **Node creation during replay**: When following a decision path, if a child doesn't exist yet, we create it by inspecting the search space at that position. This is why we need the original `search_space` passed to deserialization.

2. **Statistics consistency**: After reconstruction, we should verify that parent statistics are consistent with children. If not, recalculate from children (children are the source of truth).

3. **Random state**: If using a random selector, consider:
   - Saving/restoring `Random.state` for reproducibility
   - Or using a deterministic selector when resuming

### Interface Design

```ocaml
val save_state : 'a t -> string  (* Serialize to JSON/sexp string *)

val load_state : 
  'a Searchspace.t -> 
  string -> 'a t  (* Deserialize and return new estimator *)

val save_state_to_file : 
  string -> 'a t -> unit  (* Save to file path *)

val load_state_from_file : 
  'a Searchspace.t -> string -> 'a t  (* Load from file path *)
```

## Files to Modify

- `searchspace/stochastic_estimator.ml` - implementation
- `searchspace/stochastic_estimator.mli` - interface update

## Dependencies

This task depends on Task 3 (serialization format and decision path encoding). Tests should be written after Task 3's API is defined.
