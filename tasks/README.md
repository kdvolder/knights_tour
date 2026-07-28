# Tasks for Stochastic Estimator Enhancements

This folder contains task files for the stochastic estimator enhancement project.
Each task is a self-contained unit of work with clear goals and acceptance criteria.

## Overview

The goal is to enhance the existing `stochastic_estimator.ml` module with:
1. **Solution observation** - callback to receive solutions as they are found during sampling
2. **Progress monitoring** - periodic logging of estimates, progress %, and ETA
3. **State persistence** - serialize/deserialize estimator state for restart capability
4. **Auto-save with crash resistance** - periodic saves that survive crashes via atomic swap
5. **Memory management** - prune fully-explored branches to reduce memory usage
6. **CLI integration** - command-line executable similar to solve_file.ml

## Task Index

| # | Task | Status |
|---|------|--------|
| 1 | [Solution Observation Callback](01-solution-observation-callback.md) | Done ✓ |
| 2 | [Progress Monitoring & Logging](02-progress-monitoring-and-logging.md) | Not Started |
| 3 | [State Serialization - Decision Path Encoding](03-state-serialization-decision-paths.md) | Not Started |
| 4 | [State Deserialization - Tree Recreation](04-state-deserialization-tree-recreation.md) | Not Started |
| 5 | [Auto-save with Crash Resistance](05-auto-save-with-crash-resistance.md) | Not Started |
| 6 | [Memory Management - Tree Pruning](06-memory-management-tree-pruning.md) | Not Started |
| 7 | [CLI Integration](07-cli-integration.md) | Not Started |

## Task Dependencies

```
Task 1 (Callback)
    │
    ▼
Task 2 (Progress) ──▶ Task 5 (Auto-save)
    │                       │
    ▼                       ▼
Task 3 (Serialize) ──▶ Task 4 (Deserialize)
    │                       │
    ▼                       ▼
Task 6 (Pruning) ────────┘
    │
    ▼
Task 7 (CLI) - depends on all above
```

- **Tasks 1-2**: Can be done in parallel (both modify the estimator API)
- **Task 3→4**: Serialization must come before deserialization
- **Task 5**: Depends on Tasks 3+4 (needs serialization to save)
- **Task 6**: Can be done in parallel with Tasks 3-5 (modifies tree structure)
- **Task 7**: Depends on all other tasks (integrates everything)

## Task Format

Each task file follows this structure:
- **Goal**: What we want to achieve
- **Acceptance Criteria**: Concrete, testable conditions
- **Implementation Process**: TDD approach - write tests first, then make them pass

## Status Markers

Use these markers in task files:
- `[ ]` - Not Started
- `[>]` - In Progress (tests written, implementation pending)
- `[-]` - Tests Passing, Implementation Complete
- `[x]` - Task Complete (code reviewed and merged)

## Notes on Large State Files

As noted during task design, serialization of very large materialized trees may become slow.
This is acknowledged as a future optimization concern. For now:
- Accept that save time increases with tree size
- The atomic swap strategy (Task 5) protects against corruption regardless of save duration
- Future optimizations could include: incremental saves, compression, or async saving
