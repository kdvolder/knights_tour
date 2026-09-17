# Tasks for Stochastic Estimator Enhancements

## Active Work

| # | Task | Status |
|---|------|--------|
| 11 | [Promising Selector](11-promising-selector.md) — pick child with highest solution density instead of least remaining work | Not Started |
| 12 | [Asymptotic Sliding Threshold](12-asymptotic-sliding-threshold.md) — ensure undersampled mode never completely dies out | Not Started |

## Done (for reference)

Tasks 1–10 are complete and live in `tasks/done/`. Key accomplishments:
- **Core estimator**: solution callbacks, progress monitoring, state serialization/deserialization
- **Memory management**: tree pruning, greedy completion selector, gradual braking selector
- **CLI**: `estimate_polyomino` with auto-save, resume, CTRL-C handling, multi-resolution logging

## Task Format

Each task file follows this structure:
- **Goal**: What we want to achieve
- **Acceptance Criteria**: Concrete, testable conditions
- **Implementation Process**: TDD approach - write tests first, then make them pass

## Status Markers

- `[ ]` - Not Started
- `[>]` - In Progress (tests written, implementation pending)
- `[-]` - Tests Passing, Implementation Complete
- `[x]` - Task Complete (code reviewed and merged)
