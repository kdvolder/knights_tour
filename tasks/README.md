# Tasks for Stochastic Estimator Enhancements

This folder contains task files for the stochastic estimator enhancement project.
Each task is a self-contained unit of work with clear goals and acceptance criteria.

## Overview

The goal is to enhance the existing `stochastic_estimator.ml` module with:
1. **Solution observation** - callback to receive solutions as they are found during sampling ✅
2. **Progress monitoring** - periodic logging of estimates, progress %, and ETA ✅
3. **State persistence** - serialize/deserialize estimator state for restart capability ✅
4. **Memory management** - prune fully-explored branches to reduce memory usage ✅
5. **Adaptive selector selection** - monitor memory pressure, switch between broad/narrow exploration to enable pruning ✅
6. **Greedy completion selector** - pick child with least remaining work to drive branches to completion ✅
7. **Gradual braking selector** - smooth transition between undersampled and greedy modes to prevent memory overshoot ✅
8. **CLI integration** - command-line executable with auto-save, resume, and progress reporting (in progress)

## Task Index

| # | Task | Status |
|---|------|--------|
| 1 | [Solution Observation Callback](done/01-solution-observation-callback.md) | Done ✓ |
| 2 | [Progress Monitoring & Logging](done/02-progress-monitoring-and-logging.md) | Done ✓ |
| 3 | [State Serialization - Decision Path Encoding](done/03-state-serialization-decision-paths.md) | Done ✓ |
| 4 | [State Deserialization - Tree Recreation](done/04-state-deserialization-tree-recreation.md) | Done ✓ |
| 5 | [Auto-save with Crash Resistance](05-auto-save-with-crash-resistance.md) | Superseded by Task 9 |
| 6 | [Memory Management - Tree Pruning](done/06-memory-management-tree-pruning.md) | Done ✓ |
| 7 | [Greedy Completion Selector](done/07-greedy-completion-selector.md) | Done ✓ |
| 8 | [Adaptive Selector Selection (Hard Braking)](done/08-adaptive-selector-selection.md) | Done ✓ |
| 9 | [CLI Integration](09-cli-integration.md) | In Progress |
| 10 | [Gradual Braking Selector](done/10-gradual-braking-selector.md) | Done ✓ |

## Task Dependencies

```
Task 1 (Callback)          Task 6 (Pruning)
    │                            │
    ▼                            ▼
Task 2 (Progress)              Task 7 (Greedy Selector)
    │                               │
    ▼                               ▼
Task 3 (Serialize) ──▶ Task 4 (Deserialize)    Task 8 (Hard Braking Selector)
    │                       │                        │
    ▼                       ▼                        ▼
Task 9 (CLI) - integrates all above              Task 10 (Gradual Braking Selector)
```

- **Tasks 1-2**: Done — both modify the estimator API, implemented in parallel
- **Task 3→4**: Serialization + lazy deserialization — both done. Lazy views mean `load_state` triggers 0 inspections; all deferred until sampling resumes.
- **Task 5**: Superseded by Task 9 — auto-save is integrated into the CLI, not a separate library feature
- **Task 6→7**: Pruning must come before greedy selector (selector needs pruning to be effective) — both done
- **Task 8**: Depends on Task 6+7 — adaptive selection only makes sense if completed branches can be pruned and selector exists
- **Task 10**: Depends on Task 8 — gradual braking replaces/enhances hard braking with smoother transition
- **Task 9**: Depends on Tasks 1-4, 6-8 (integrates everything into CLI) — in progress

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
- The atomic swap strategy (Task 9) protects against corruption regardless of save duration
- Future optimizations could include: incremental saves, compression, or async saving
