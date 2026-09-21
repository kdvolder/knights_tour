# Task 17: Search tree visualization

## Goal
Visualize the oracle2 search tree as a fractal-like radial layout where:
- Each node is represented by a colored dot (color = node type)
- Subtrees get proportional arc space (larger subtrees = wider angle)
- Radial spacing follows geometric progression: `R_d = R_max × (1 - r^d)` where `r < 1`
- Deeper levels compress toward the edge, giving fractal self-similarity
- Zoom/pan from "see the whole tree shape" down to individual nodes

## Three separate problems

### 1. Data extraction
- Write an OCaml script that loads the saved state using our existing `load_state` function from `stochastic_estimator.ml`
- Walk the reconstructed tree and output node data in a visualization-friendly format (JSON/CSV)
- Output includes: nodes, edges (parent → child), and properties (node type, solution_estimate, depth, etc.)
- No spatial info yet — just the graph structure
- Reuse our existing serialization/parsing code instead of writing a separate parser

### 2. Layout algorithm
- **Proportional arc subdivision**: each subtree gets angle proportional to its *max width* (number of nodes at the widest level within the subtree) via `d3.tree().node.sum(weight)`
  - Max width directly measures how many nodes need to be displayed side-by-side
  - Wide, shallow branches get wide slices; deep, narrow branches get thin slices
  - Alternative: number of leaves as a simpler proxy for max width
- **Node-local adaptive radial spacing** (no global concentric circles):
  - Each subtree is its own independent radial tree with spacing based on *its* local branching
  - Recurrence: `R_{d+1} = R_d × (1 + 2π/N_d)` where `N_d` is the number of siblings at this node
  - Large subtrees spread out more, small ones pack tighter — each scales independently
  - No special case for first few layers — formula applied uniformly at every level
- This is fractal-ish because each subtree is self-contained and scales on its own terms
- **D3.js d3-hierarchy** for proportional arc allocation, then apply adaptive radius formula
- Alternative: implement from first principles if needed

### 3. Rendering
- GPU-accelerated rendering for 500K+ nodes
- **Smart aggregation**: when 1000 dots cluster into a single pixel, draw one pixel (not 1000 overlapping ones)
- Zoom/pan support — smooth interaction at all scales
- **Cosmos.gl** (WebGL, handles 100K+ nodes) or custom Three.js/Pixi.js renderer
- Color dots by node type (Result=green, Fail=red, Fork=blue, Completed=gray)

## Implementation approach
1. Write an OCaml script that loads the saved state using `load_state` from our existing code, walks the tree, and outputs visualization data (nodes, edges, properties)
2. Use D3.js d3-hierarchy to compute proportional arc allocation (`d3.tree().node.sum(weight)`)
3. Apply node-local adaptive radial spacing: `R_{d+1} = R_d × (1 + 2π/N_d)` at every level, no special cases
4. Export layout data (x, y coordinates + colors) as JSON/CSV
5. Render with WebGL library supporting zoom/pan and smart aggregation

## Notes
- The layout computation (step 2) is fast — D3 handles it fine even for large trees
- The rendering (step 3) is where GPU acceleration matters — CPU-bound tools like D3.js with SVG will choke
- Smart aggregation is key: at zoomed out, clusters of 1000 nodes should render as a single colored pixel, not 1000 overlapping dots
