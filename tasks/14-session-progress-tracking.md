# Task 14: Solution-based ETA

## Core idea
Add a second ETA to the progress table — one based on **solutions found** rather than **nodes explored**.

The current ETA is a *materialization ETA*: it estimates how long until all nodes are fully explored (400 million kalpas). A *solution-based ETA* would estimate how long until we find all solutions, based on:
- Total estimated solutions in the tree (from estimator)
- Already-found solutions (computed from tree state on load)
- Current solve rate (solutions per unit time, tracked across sessions)

Formula: `(total_solutions_estimate - already_found) / solve_rate`

This tells us when we might hit the "drop-off" point — when solutions stop coming and the remainder of the tree is mostly dead ends.

## How to get "already found"
- Walk the tree after loading, sum `solution_estimate` of all complete nodes (completed nodes have no children so recursion stops naturally)
- This captures ALL historical solutions, regardless of when the save file was created (works for 6-week-old trees)
- No new state to persist — one source of truth = no desync bugs
- Zero cost: we're already walking all nodes during load anyway

## Implementation approach
- Add a function to compute total solutions found from tree structure (no persistence)
- Track current solve rate in the logger (solutions per unit time, across sessions)
- Add a "Sols Remaining" and "Solution-based ETA" column to the progress table
