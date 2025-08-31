import React from 'react';
import type { JSX } from 'react';

const CELL_SIZE = 32; // px

// Simple color mapping for demo; can be improved to match OCaml logic
const colorMap: Record<string, string> = {
  '#': '#222', // Blocked
  '.': '#e8e8e8', // Vacant - more noticeable grey
  // Add more mappings for each piece/letter as needed
};

function getCellColor(ch: string) {
  if (colorMap[ch]) return colorMap[ch];
  // Assign a color based on char code for now
  const code = ch.charCodeAt(0);
  const hue = (code * 47) % 360;
  return `hsl(${hue},70%,70%)`;
}

interface BoardProps {
  board: string[];
}

export const Board: React.FC<BoardProps> = ({ board }) => {
  const rows = board;
  const width = rows[0]?.length || 0;
  const height = rows.length;

  // Helper to check if two cells are different (for boundary drawing)
  function isBoundary(x1: number, y1: number, x2: number, y2: number) {
    if (
      x1 < 0 || x1 >= width || y1 < 0 || y1 >= height ||
      x2 < 0 || x2 >= width || y2 < 0 || y2 >= height
    ) return false;
    return rows[y1][x1] !== rows[y2][x2];
  }

  // Collect SVG lines for boundaries
  const boundaries: JSX.Element[] = [];
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      // Right boundary
      if (isBoundary(x, y, x + 1, y)) {
        boundaries.push(
          <line
            key={`v-${x}-${y}`}
            x1={(x + 1) * CELL_SIZE}
            y1={y * CELL_SIZE}
            x2={(x + 1) * CELL_SIZE}
            y2={(y + 1) * CELL_SIZE}
            stroke="#000"
            strokeWidth={2}
          />
        );
      }
      // Bottom boundary
      if (isBoundary(x, y, x, y + 1)) {
        boundaries.push(
          <line
            key={`h-${x}-${y}`}
            x1={x * CELL_SIZE}
            y1={(y + 1) * CELL_SIZE}
            x2={(x + 1) * CELL_SIZE}
            y2={(y + 1) * CELL_SIZE}
            stroke="#000"
            strokeWidth={2}
          />
        );
      }
    }
  }

  return (
    <div className="board">
      <svg
        width={width * CELL_SIZE}
        height={height * CELL_SIZE}
        style={{ border: '2px solid #333', background: '#eee' }}
      >
        {/* Draw cells */}
        {rows.map((row: string, y: number) =>
          row.split('').map((ch: string, x: number) => (
            <rect
              key={x + '-' + y}
              x={x * CELL_SIZE}
              y={y * CELL_SIZE}
              width={CELL_SIZE}
              height={CELL_SIZE}
              fill={getCellColor(ch)}
            />
          ))
        )}
        {/* Draw boundaries on top */}
        {boundaries}
      </svg>
    </div>
  );
};
