import React from 'react';
import './HeatmapThumbnail.css';

interface HeatmapThumbnailProps {
  /** 2D array of normalized age values (0-1 range) */
  normalizedAgeMatrix: number[][];
  /** 2D array of raw age values for displaying numbers */
  rawAgeMatrix: number[][];
  /** 2D array of percentile values (0-1 range) for color distribution */
  percentileMatrix: number[][];
  /** Width of the thumbnail in pixels */
  width?: number;
  /** Height of the thumbnail in pixels */
  height?: number;
  /** Title to display above the heatmap */
  title?: string;
  /** Show statistics below the heatmap */
  showStats?: boolean;
  /** Heatmap statistics for display */
  stats?: {
    min: number;
    max: number;
    average: number;
    totalCells: number;
    maxAgeEver: number;
  };
}

/**
 * HeatmapThumbnail - Displays a small thumbnail visualization of board stability
 * 
 * Colors:
 * - Red squares = Hot (recently changed, age 0)
 * - White squares = Cold (stable/frozen, high age)
 */
export const HeatmapThumbnail: React.FC<HeatmapThumbnailProps> = ({
  normalizedAgeMatrix,
  rawAgeMatrix,
  percentileMatrix,
  width = 200,
  height = 200,
  title = "Board Stability Heatmap",
  showStats = true,
  stats
}) => {
  const boardHeight = normalizedAgeMatrix.length;
  const boardWidth = boardHeight > 0 ? normalizedAgeMatrix[0].length : 0;

  if (boardHeight === 0 || boardWidth === 0) {
    return (
      <div className="heatmap-thumbnail">
        <h4>{title}</h4>
        <div className="heatmap-empty">No data available</div>
      </div>
    );
  }

  const cellWidth = width / boardWidth;
  const cellHeight = height / boardHeight;

  // Calculate font size based on cell size - make it more generous
  const fontSize = Math.min(cellWidth, cellHeight) * 0.5;
  const showNumbers = fontSize >= 6; // Lower threshold for smaller boards

  console.log(`Heatmap debug: cellSize=${Math.min(cellWidth, cellHeight).toFixed(1)}, fontSize=${fontSize.toFixed(1)}, showNumbers=${showNumbers}`);

  /**
   * Convert percentile value (0-1) to a heat color
   * 0 = Red (youngest pieces)
   * 1 = White (oldest pieces)
   */
  const getHeatColor = (percentile: number): string => {
    // Clamp value between 0 and 1
    const clamped = Math.max(0, Math.min(1, percentile));
    
    // Interpolate from red (255,0,0) to white (255,255,255)
    const red = 255;
    const green = Math.round(255 * clamped);
    const blue = Math.round(255 * clamped);
    
    return `rgb(${red}, ${green}, ${blue})`;
  };

  return (
    <div className="heatmap-thumbnail">
      <h4 className="heatmap-title">{title}</h4>
      
      <svg 
        width={width} 
        height={height} 
        className="heatmap-svg"
        style={{ border: '1px solid #ccc' }}
      >
        {normalizedAgeMatrix.map((row, y) =>
          row.map((normalizedAge, x) => {
            const rawAge = rawAgeMatrix[y][x];
            const percentile = percentileMatrix[y][x];
            return (
              <g key={`${x}-${y}`}>
                {/* Colored rectangle showing heat map with percentile-based coloring */}
                <rect
                  x={x * cellWidth}
                  y={y * cellHeight}
                  width={cellWidth}
                  height={cellHeight}
                  fill={getHeatColor(percentile)}
                  stroke="#ccc"
                  strokeWidth="0.5"
                />
                {/* Numbers overlaid on top of colors */}
                {showNumbers && (
                  <text
                    x={x * cellWidth + cellWidth / 2}
                    y={y * cellHeight + cellHeight / 2 + fontSize / 3}
                    textAnchor="middle"
                    dominantBaseline="middle"
                    fontSize={fontSize}
                    fill="#000"
                    fontFamily="Arial, sans-serif"
                    fontWeight="bold"
                    style={{ textShadow: '0 0 2px white, 0 0 2px white, 0 0 2px white' }}
                  >
                    {rawAge}
                  </text>
                )}
              </g>
            );
          })
        )}
      </svg>

      {showStats && stats && (
        <div className="heatmap-stats">
          <div className="stat-row">
            <span>Max Age:</span>
            <span>{stats.maxAgeEver}</span>
          </div>
          <div className="stat-row">
            <span>Current Range:</span>
            <span>{stats.min} - {stats.max}</span>
          </div>
          <div className="stat-row">
            <span>Average:</span>
            <span>{stats.average.toFixed(1)}</span>
          </div>
          <div className="heatmap-legend">
            <div className="legend-item">
              <div className="legend-color" style={{ backgroundColor: 'rgb(255, 0, 0)' }}></div>
              <span>Hot (changing)</span>
            </div>
            <div className="legend-item">
              <div className="legend-color" style={{ backgroundColor: 'rgb(255, 255, 255)', border: '1px solid #ccc' }}></div>
              <span>Cold (stable)</span>
            </div>
          </div>
        </div>
      )}
    </div>
  );
};
