# Task 16: Node inspector API

## Core idea
Add an optional capability to inspect any node in the tree and get back a description of what that state represents. Not all nodes can be meaningfully described — some are intermediate choices (piece orientation, rotation) while others correspond to concrete sub-problems (board state with remaining pieces).

## Design
The searchspace library is abstract and doesn't know about boards. The description type needs to be a parameter of the searchspace abstraction:

- **Library**: defines the inspector interface with a generic description type parameter
  - `inspect_node : 'a node -> 'desc option` — returns Some(description) when possible, None otherwise
  - Works with any description type the concrete solver provides

- **Concrete solver** (estimate_polyomino):
  - Defines its own description types: `board_state`, `piece_orientation`, etc.
  - Provides the inspector implementation that translates decisions into those types
  - Knows which decision types map to describable states vs intermediate choices

## Description types (example)
- `Board_state of board * placed_pieces * remaining_pieces` — for nodes representing concrete sub-problems
- `Piece_orientation of piece * orientation * position` — for intermediate placement decisions
- String fallback for nodes we can only describe abstractly ("choice 3 of 7")

## Use cases
- **Debugging/visualization**: understand what's going on at different depths in the tree
- **Mining sub-problems** (Task 15): find nodes with tractable search space and extract concrete puzzle definitions
- **Exploration**: compare different parts of the tree, understand what kinds of decisions are being made

## Implementation approach
- Add inspector function to searchspace/estimator module with generic description type parameter
- Concrete solver provides the actual types and implementation
- Inspector walks from root to target node, applying each decision's transformation
