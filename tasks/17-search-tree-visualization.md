# Task 17: Search tree visualization

## Goal
Visualize the oracle2 search tree as a fractal-like radial layout where:
- Each node is represented by a colored dot (color = node type)
- Subtrees get proportional arc space (larger subtrees = wider angle)
- Radial spacing follows geometric progression: `R_d = R_max × (1 - r^d)` where `r < 1`
- Deeper levels compress toward the edge, giving fractal self-similarity
- Zoom/pan from "see the whole tree shape" down to individual nodes

## Three separate problems

### 1 & 2. Data extraction + Layout (single OCaml program)
- Write an OCaml script that loads the saved state using our existing `load_state` function from `stochastic_estimator.ml`
- The tree is now in memory — no intermediate file needed between extraction and layout
- Walk the tree, compute radial layout positions (proportional arc subdivision + adaptive spacing), and output a **flat array of nodes** with `(x, y, color, parent_index)` directly to `tree-data.json` for Deck.gl
- No intermediate JSON — load, process, output in one program
- Reuse our existing serialization/parsing code instead of writing a separate parser

### 3. Rendering (Deck.gl)
- **Deck.gl** — Uber's WebGL visualization framework
  - Handles GPU-accelerated rendering for 500K+ nodes automatically
  - Built-in **smart aggregation**: when dots cluster into a single pixel, blends colors (no custom shader needed)
  - Built-in **zoom/pan**: `controller: true` gives smooth interaction out of the box
  - Minimal JS code (~50-100 lines) — just load data and configure layers
- **Two layers**:
  - `ScatterplotLayer` for nodes (colored dots)
  - `PathLayer` or custom layer for edges (line segments from parent to child)
- Color dots by node type (Result=green, Fail=red, Fork=blue, Completed=gray)
- **Data format**: OCaml outputs `tree-data.json` (flat arrays), Deck.gl loads it via URL
  - HTML file is tiny (~KB) — data lives in separate JSON file (tens of MB)
  - Deck.gl fetches and parses the JSON, converts to GPU buffers automatically
- **Live updates**: OCaml can overwrite `tree-data.json` periodically, browser refreshes to see progress

## Implementation approach
1. **OCaml program**: Write script that loads saved state via `load_state`, walks tree, computes radial layout (proportional arc subdivision + adaptive spacing), outputs flat array (`tree-data.json`) with `(x, y, color, parent_index)`
2. **Deck.gl rendering**: Minimal HTML/JS that loads `tree-data.json` and renders with `ScatterplotLayer` (nodes) + edge layer
3. **Iterate**: Start with static file, add live-update capability if desired

## Notes
- **OCaml does steps 1 & 2** (data extraction + layout in one program), Deck.gl handles step 3 (rendering)
- No intermediate file between extraction and layout — tree is in memory, processed directly
- Layout computation in OCaml is fast — no need for D3.js
- Deck.gl handles GPU acceleration, smart aggregation, and zoom/pan automatically — minimal JS code needed
- Data format is flat arrays (not nested JSON) for efficiency with 500K+ nodes
- Edges are implicit via parent pointers — renderer draws lines from each node to its parent
