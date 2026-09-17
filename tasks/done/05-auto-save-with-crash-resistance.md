# Task 5: Auto-save with Crash Resistance

## Goal

Implement periodic auto-saving of estimator state that survives crashes and system shutdowns. The save mechanism must be crash-resistant: a crash during save should never corrupt the saved state, and the process can always resume from the last known-good checkpoint.

## Background

For estimations that may run for days, weeks, or longer, periodic saves are essential. The key challenge is **atomicity** - if the process crashes mid-write, we must not end up with a corrupted state file that looks valid but is half-written.

### Crash Resistance Strategy: Backup + Atomic Swap

The approach uses two files and an atomic rename:
1. `state.backup` - the last known-good saved state (always valid)
2. `state.tmp` - temporary file being written to

Process:
1. Write new state to `state.tmp`
2. If write succeeds, atomically rename: `state.backup` → `state.old` (delete), then `state.tmp` → `state.backup`
3. On startup, always load from `state.backup`

The atomic rename (`rename()` syscall) is the key - it's either fully done or not at all. A crash during rename leaves `state.backup` intact (the old version).

### Alternative: Double Backup Strategy

Even simpler - always keep two backups:
1. `state.backup` - previous checkpoint (always valid)  
2. `state.current` - latest checkpoint being written

Write to `state.current.tmp`, then atomic rename to `state.current`.
On crash recovery: if `state.current` is corrupted, fall back to `state.backup`.

## Acceptance Criteria

### 5.1 Periodic Save Mechanism

1. **Save interval is configurable**:
   - Default: every 60 seconds (or some reasonable default)
   - Can be set via CLI flag or config

2. **Save is triggered by progress reporter**:
   - The `progress_reporter` callback can optionally include a save action
   - Save is triggered at most once per interval (no duplicate saves)

3. **Save does not block progress reporting**:
   - Progress reports continue during save (save happens in background or is fast enough to not matter)
   - If save takes too long, it should not delay the next progress report

### 5.2 Crash Resistance

4. **Crash during write never corrupts state**:
   - After crash, the saved state file is always valid (parseable and loadable)
   - No partial/half-written files left behind

5. **Atomic swap strategy works**:
   - Write to temp file first, then atomic rename
   - On crash mid-write: old state remains intact (temp file may be orphaned, but that's OK)
   - On crash during rename: atomicity guarantees either old or new state exists

6. **Orphaned temp files are cleaned up**:
   - On startup, clean any leftover `.tmp` or `.old` files

### 5.3 Resume After Crash

7. **Resumed estimation continues correctly**:
   - All solutions found before crash are preserved (via callback or log)
   - Estimates from before crash are restored and continue updating
   - No duplicate solutions or double-counting

8. **State file location is configurable**:
   - Default: `state.json` in current working directory
   - Can be overridden via CLI flag

## Implementation Process (TDD)

### Phase 1: Atomic Save Tests

```ocaml
let%test_module "atomic_save" = (module struct
  (* Test: write to temp then rename produces valid file *)
  let test_atomic_write () = 
    (* Write state to .tmp, rename to actual file *)
    (* Verify file is valid and parseable *)
    ...
  
  (* Test: orphaned temp file does not affect valid state *)
  let test_orphaned_temp () = 
    (* Create .tmp file, leave it orphaned *)
    (* Load state - should ignore .tmp and load valid backup *)
    ...
  
  (* Test: cleanup removes orphaned files *)
  let test_cleanup_orphans () = 
    (* Create .tmp and .old files *)
    (* Call cleanup *)
    (* Verify they are removed *)
    ...
end)
```

### Phase 2: Periodic Save Tests

```ocaml
let%test_module "periodic_save" = (module struct
  (* Test: save is triggered at correct interval *)
  let test_save_interval () = 
    (* Create estimator with reporter that saves every N seconds *)
    (* Run for 2*N seconds *)
    (* Verify exactly 2 saves occurred *)
    ...
  
  (* Test: save does not skip samples *)
  let test_save_does_not_skip_samples () = 
    (* Run estimator with periodic saves *)
    (* Verify all samples were processed, none lost during save *)
    ...
  
  (* Test: consecutive saves produce valid state *)
  let test_consecutive_saves () = 
    (* Save, then save again after interval *)
    (* Both files should be valid and loadable *)
    ...
end)
```

### Phase 3: Crash Recovery Tests

```ocaml
let%test_module "crash_recovery" = (module struct
  (* Test: load corrupted file falls back to backup *)
  let test_fallback_to_backup () = 
    (* Create valid state, save it *)
    (* Corrupt the current file by writing garbage *)
    (* Load should succeed using backup *)
    ...
  
  (* Test: no duplicate solutions after resume *)
  let test_no_duplicate_solutions () = 
    (* Run estimator, collect solutions via callback *)
    (* Save state *)
    (* Simulate crash (kill process) *)
    (* Resume from saved state, collect more solutions *)
    (* Verify no solution appears in both batches *)
    ...
  
  (* Test: estimates are consistent after resume *)
  let test_estimates_consistent_after_resume () = 
    (* Create, sample, save *)
    (* Load and verify estimates match saved values *)
    ...
end)
```

### Phase 4: Integration Tests

```ocaml
let%test_module "auto_save_integration" = (module struct
  (* Test: full workflow - create, sample, auto-save, resume *)
  let test_full_workflow () = 
    (* Create estimator with auto-save enabled *)
    (* Sample for a while *)
    (* Verify state file exists and is valid *)
    (* Resume from saved state *)
    (* Continue sampling to completion *)
    (* Verify all solutions found *)
    ...
  
  (* Test: auto-save with progress reporting *)
  let test_auto_save_with_progress () = 
    (* Both save and progress reporter active *)
    (* Verify both work without interference *)
    ...
end)
```

## Design Notes

### File Naming Convention

```
state.backup      - Last known-good checkpoint (always valid)
state.current     - Latest checkpoint (may be incomplete if crash during write)
state.backup.old  - Previous backup, cleaned up after successful new save
```

Actually, simpler approach:

```
state.json        - Current state (atomic swap target)
state.json.tmp    - Temporary file for writing new state
```

On crash: `state.json` is always valid (either old or fully-written new version).
No need for explicit backup file - the atomic swap IS the backup mechanism.

### Save Trigger Design

```ocaml
type auto_save_config = {
  interval_seconds : float;
  state_file : string;
}

type progress_reporter = {
  interval_seconds : float;
  on_progress : progress -> unit;
  auto_save : auto_save_config option;  (* Optional periodic save *)
}
```

### Save Implementation Sketch

```ocaml
let save_state_to_file est filepath =
  let tmp_path = filepath ^ ".tmp" in
  let serialized = serialize_state est in
  (* Write to temp file *)
  Out_channel.with_open_text tmp_path (fun ch ->
    output_string ch serialized;
    flush ch  (* Ensure data is on disk *)
  );
  (* Atomic rename - this is the crash-resistant step *)
  Sys.rename tmp_path filepath

let load_state_from_file est filepath search_space =
  try
    let serialized = In_channel.read_text filepath in
    deserialize_state search_space serialized
  with _ ->
    (* File missing or corrupted - start fresh *)
    create search_space
```

### When to Save

- On progress reporter interval (e.g., every 60 seconds)
- Optionally: on solution found (for small state, but could be expensive for large trees)
- On completion (save final state even if not resuming)

### Performance Considerations

For now, accept that save may take time as tree grows. Future optimization:
- Incremental saves (only serialize changed nodes)
- Compressed serialization
- Async save in separate thread

## Files to Modify

- `searchspace/stochastic_estimator.ml` - implementation
- `searchspace/stochastic_estimator.mli` - interface update

## Files to Create

- Tests inline in `stochastic_estimator.ml` using `%test_module` and `[%expect_test]`
