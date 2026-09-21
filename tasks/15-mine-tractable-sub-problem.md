# Task 15: Mine tree for tractable sub-problem

## Core idea
Instead of creating an artificial puzzle, mine the existing tree for a sub-problem with tractable search space (days/weeks instead of kalpas).

## Approach
1. Load the save state from oracle2 (the 6-week-old tree)
2. Walk through nodes looking for ones where:
   - Estimated search space is "tractable" (e.g., < 10^20 nodes instead of 10^28)
   - Non-zero estimated solutions (so there's something to find)
3. Pick a node and treat it as the new root — run the solver on this sub-problem

## Why this is better than artificial puzzles
- Real density estimates (already computed from 6 weeks of exploration)
- Real solution-rich regions to explore and validate against
- Already partially explored — we know the tree structure is valid
- No guessing about board sizes or piece counts

## Implementation approach
- Add a function to walk the tree and find nodes with estimated search space in a target range
- Create a way to "resume from node" — either by:
  - Creating a wrapper that treats the chosen node as root, or
  - Extracting just the subtree and serializing it as a new state file
- Run the solver on this sub-problem and watch the full lifecycle: first solution → steady rate → drop-off point

## Goal
A tractable puzzle where we can:
- Actually see the drop-off point (solutions stop coming)
- Validate solution-based ETA works correctly
- Test the algorithm end-to-end without waiting for kalpas
