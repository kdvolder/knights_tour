import React from 'react';
import type { JSX } from 'react';
import './Board.css';

const CELL_SIZE = 32; // px

// Simple color mapping for demo; can be improved to match OCaml logic
const colorMap: Record<string, string> = {
  '#': '#222', // Blocked
  '.': '#e8e8e8', // Vacant - more noticeable grey
  // Add more mappings for each piece/letter as needed
};

function getCellColor(ch: string, percentile?: number) {
  if (colorMap[ch]) {
    // Apply frosting to special colors like blocked cells too
    if (percentile !== undefined) {
      const baseColor = colorMap[ch];
      return applyFrosting(baseColor, percentile);
    }
    return colorMap[ch];
  }
  
  // Assign a base color based on char code for now
  const code = ch.charCodeAt(0);
  const hue = (code * 47) % 360;
  const baseColor = `hsl(${hue},70%,70%)`;
  
  // If we have percentile data, apply frosting effect
  if (percentile !== undefined) {
    return applyFrosting(baseColor, percentile);
  }
  
  // No percentile data, use original color
  return baseColor;
}

// Apply frosting effect to any color
function applyFrosting(color: string, percentile: number): string {
  // percentile 0 = hot/changing (no frosting), 1 = cold/stable (frosted)
  const whiteMixAmount = percentile * 0.8; // 0% white (hot) to 60% white (cold) - stronger frosting
  
  // Parse different color formats
  let r: number, g: number, b: number;
  
  if (color.startsWith('#')) {
    // Handle hex colors like #222
    const hex = color.slice(1);
    if (hex.length === 3) {
      r = parseInt(hex[0] + hex[0], 16);
      g = parseInt(hex[1] + hex[1], 16);
      b = parseInt(hex[2] + hex[2], 16);
    } else {
      r = parseInt(hex.slice(0, 2), 16);
      g = parseInt(hex.slice(2, 4), 16);
      b = parseInt(hex.slice(4, 6), 16);
    }
  } else if (color.startsWith('hsl')) {
    // Convert HSL to RGB
    const hslMatch = color.match(/hsl\((\d+),\s*(\d+)%,\s*(\d+)%\)/);
    if (hslMatch) {
      const [, h, s, l] = hslMatch.map(Number);
      const rgb = hslToRgb(h, s, l);
      r = rgb.r;
      g = rgb.g;
      b = rgb.b;
    } else {
      return color; // fallback
    }
  } else {
    return color; // fallback for other formats
  }
  
  // Mix with white
  const white = { r: 255, g: 255, b: 255 };
  const mixedRgb = {
    r: Math.round(r * (1 - whiteMixAmount) + white.r * whiteMixAmount),
    g: Math.round(g * (1 - whiteMixAmount) + white.g * whiteMixAmount),
    b: Math.round(b * (1 - whiteMixAmount) + white.b * whiteMixAmount)
  };
  
  return `rgb(${mixedRgb.r},${mixedRgb.g},${mixedRgb.b})`;
}

// Helper function to convert HSL to RGB
function hslToRgb(h: number, s: number, l: number): { r: number; g: number; b: number } {
  h /= 360;
  s /= 100;
  l /= 100;
  
  const a = s * Math.min(l, 1 - l);
  const f = (n: number) => {
    const k = (n + h * 12) % 12;
    const color = l - a * Math.max(Math.min(k - 3, 9 - k, 1), -1);
    return Math.round(255 * color);
  };
  
  return { r: f(0), g: f(8), b: f(4) };
}

interface BoardProps {
  board: string[];
  percentileMatrix?: number[][];
}

export const Board: React.FC<BoardProps> = ({ board, percentileMatrix }) => {
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
        // Get average percentile of adjacent cells for boundary frosting
        const leftPercentile = percentileMatrix?.[y]?.[x] ?? 0;
        const rightPercentile = percentileMatrix?.[y]?.[x + 1] ?? 0;
        const avgPercentile = (leftPercentile + rightPercentile) / 2;
        const strokeColor = applyFrosting("#000", avgPercentile);
        
        boundaries.push(
          <line
            key={`v-${x}-${y}`}
            x1={(x + 1) * CELL_SIZE}
            y1={y * CELL_SIZE}
            x2={(x + 1) * CELL_SIZE}
            y2={(y + 1) * CELL_SIZE}
            stroke={strokeColor}
            strokeWidth={2}
          />
        );
      }
      // Bottom boundary
      if (isBoundary(x, y, x, y + 1)) {
        // Get average percentile of adjacent cells for boundary frosting
        const topPercentile = percentileMatrix?.[y]?.[x] ?? 0;
        const bottomPercentile = percentileMatrix?.[y + 1]?.[x] ?? 0;
        const avgPercentile = (topPercentile + bottomPercentile) / 2;
        const strokeColor = applyFrosting("#000", avgPercentile);
        
        boundaries.push(
          <line
            key={`h-${x}-${y}`}
            x1={x * CELL_SIZE}
            y1={(y + 1) * CELL_SIZE}
            x2={(x + 1) * CELL_SIZE}
            y2={(y + 1) * CELL_SIZE}
            stroke={strokeColor}
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
          row.split('').map((ch: string, x: number) => {
            // Get percentile value for this cell if available
            const percentile = percentileMatrix?.[y]?.[x];
            
            return (
              <rect
                key={x + '-' + y}
                x={x * CELL_SIZE}
                y={y * CELL_SIZE}
                width={CELL_SIZE}
                height={CELL_SIZE}
                fill={getCellColor(ch, percentile)}
              />
            );
          })
        )}
        {/* Draw boundaries on top */}
        {boundaries}
      </svg>
    </div>
  );
};
