# Task 13: Concurrent Execution Lock

> **Status**: Not Started
> **Date**: 2026-09-18

## Goal

Prevent multiple instances of `estimate_polyomino` from running on the same puzzle directory simultaneously. On startup, check if another instance is already running and exit with a clear error message instead of corrupting shared state.

## Background

Currently, two instances running on the same puzzle directory will both read and write `estimate-state.backup`, leading to:
- Corrupted state files (truncated writes from one process overwriting the other's data)
- Lost progress (one instance's samples silently discarded)
- Unpredictable behavior (both processes modifying the same tree concurrently)

The solver state is associated with the puzzle directory — all artifacts (state file, logs, solutions) live there. The lock should be at the directory level, not per-file.

## Design

### Lock File Location
- Create `.estimate.lock` in the puzzle's directory (e.g., `saves/pinned/.estimate.lock`)
- One lock per directory, preventing any concurrent solvers in that directory

### Mechanism: `Unix.lockf` (Standard Library)
- OCaml's standard library has `lockf` — no external dependencies needed
- Open the lock file, call `Unix.lockf fd Unix.F_TLOCK 0L` (non-blocking exclusive lock)
- If it fails → another instance holds the lock, exit with error: `"Another instance is already running on this puzzle"`
- Lock held for the entire run, **automatically released** when process exits or crashes — no stale lock cleanup needed

### Implementation Location
- In `estimate_polyomino.ml`, early in the startup sequence (after parsing args, before loading state)
- Create/open `.estimate.lock` in the puzzle directory
- Attempt non-blocking lock; if it fails, print error and exit with code 1

### Error Message
```
Error: Another instance is already running on this puzzle (directory: <dir>)
       Lock file: <path-to-lock>
```

## Implementation Process (TDD)

### Phase 1: Lock Acquisition Test

```ocaml
let%expect_test "lockf prevents concurrent access" = begin
  (* Create a lock file and acquire the lock *)
  let fd = Unix.openfile "/tmp/test_lock.lock" [Unix.O_CREAT; Unix.O_RDWR] 0o644 in
  try
    Unix.lockf fd Unix.F_TLOCK 0L;
    Printf.printf "Lock acquired\n";
    
    (* Try to acquire again in the same process — should succeed (same process) *)
    Unix.lockf fd Unix.F_TLOCK 0L;
    Printf.printf "Re-lock succeeded (same process)\n";
    
    Unix.lockf fd Unix.F_ULOCK 0L;
    Unix.lockf fd Unix.F_ULOCK 0L;
    Unix.close fd;
    Sys.remove "/tmp/test_lock.lock"
  with Unix.Unix_error (Unix.EAGAIN, _, _) ->
    Printf.printf "Lock already held\n"
  [%expect{|
    Lock acquired
    Re-lock succeeded (same process)
  |}]
end
```

### Phase 2: Integration Test — Lock Prevents Second Instance

```ocaml
let%expect_test "lockf blocks non-blocking lock from different process" = begin
  (* This test would need to spawn a subprocess — may be deferred *)
  (* For now, verify the lock file is created and cleaned up properly *)
end
```

### Phase 3: Integration with estimate_polyomino.ml

- Add lock acquisition in `main()` after arg parsing
- Create `.estimate.lock` if it doesn't exist
- Attempt `F_TLOCK`; on failure, print error and exit 1
- Lock is held for the entire run (no explicit unlock needed — OS releases on exit)

## Acceptance Criteria

### 13.1 Lock Acquisition
1. On startup, a `.estimate.lock` file is created in the puzzle directory
2. A non-blocking exclusive lock (`F_TLOCK`) is acquired on this file
3. If the lock cannot be acquired (another instance holds it), the program exits with code 1 and a clear error message

### 13.2 Lock Release
4. The lock is automatically released when the process exits (normal exit, signal, or crash)
5. No stale lock cleanup is needed — the OS handles this

### 13.3 Error Handling
6. The error message clearly identifies:
   - That another instance is running
   - Which puzzle directory is affected
7. Exit code is 1 (non-zero, indicating failure)

### 13.4 No External Dependencies
8. Uses only OCaml's standard library (`Unix.lockf`) — no opam packages needed

### 13.5 Directory-Level Locking
9. The lock is per-directory, not per-file — prevents concurrent solvers even if they use different state file names
10. The lock file is hidden (starts with `.`) so it doesn't clutter the directory

## Notes
- `lockf` is POSIX standard, available on Linux/macOS/Unix
- On Windows, `lockf` may not be available (but the solver is primarily used on Linux)
- The lock file itself can be left behind after crash — it doesn't matter because the OS releases the lock when the process dies, and a new instance will simply fail to acquire it (which is correct behavior — no stale locks)
