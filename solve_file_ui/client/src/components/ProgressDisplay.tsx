import React, { useEffect, useState } from 'react';
import './ProgressDisplay.css';

interface ProgressDisplayProps {
  stats: string;
  metric: number | null;
  lastUpdated: Date | null;
  csvStats: string | null;
}

interface ParsedStats {
  totalSteps: number;
  stepsSinceDequeue: number;
  queueSize: number;
  totalDequeues: number;
}

interface ParsedCSVStats {
  solutionsFound: number;
  solveRate: number;
  stepsPerSecond: number;
}

function parseSnapshotStats(stats: string): ParsedStats | null {
  // Format: "<total-solver-steps>: <steps-since-last-dequeue> / <queue-size> / <total-dequeues>"
  const match = stats.match(/(\d+):\s*(\d+)\s*\/\s*(\d+)\s*\/\s*(\d+)/);
  if (!match) return null;
  
  return {
    totalSteps: parseInt(match[1]),
    stepsSinceDequeue: parseInt(match[2]),
    queueSize: parseInt(match[3]),
    totalDequeues: parseInt(match[4])
  };
}

function parseCSVStats(csvLine: string): ParsedCSVStats | null {
  // Format: "steps,steps-since-dequeue,total-dequeues,queue-size,queue-growth-rate,solutions,avg-steps-per-solution,avg-steps-per-second"
  const parts = csvLine.split(',');
  if (parts.length < 8) return null;
  
  return {
    solutionsFound: parseFloat(parts[5]),      // Col 6: number of solutions
    solveRate: parseFloat(parts[6]),           // Col 7: avg steps per solution  
    stepsPerSecond: parseFloat(parts[7])       // Col 8: avg steps per second
  };
}

function formatNumber(num: number): string {
  return num.toLocaleString();
}

function getQueueStatus(metric: number | null): { status: string, color: string } {
  if (metric === null) return { status: 'unknown', color: '#666' };
  
  if (metric > 1.1) return { status: 'growing rapidly', color: '#ff6b35' };
  if (metric > 1.0) return { status: 'growing', color: '#ffa726' };
  if (metric > 0.9) return { status: 'stable', color: '#42a5f5' };
  return { status: 'shrinking', color: '#66bb6a' };
}

export const ProgressDisplay: React.FC<ProgressDisplayProps> = ({
  stats,
  metric,
  lastUpdated,
  csvStats
}) => {
  const [timeAgo, setTimeAgo] = useState<string>('');

  // Update time ago display every second
  useEffect(() => {
    const interval = setInterval(() => {
      if (lastUpdated) {
        const now = new Date();
        const diffMs = now.getTime() - lastUpdated.getTime();
        const diffSeconds = Math.floor(diffMs / 1000);
        
        if (diffSeconds < 60) {
          setTimeAgo(`${diffSeconds}s ago`);
        } else {
          const diffMinutes = Math.floor(diffSeconds / 60);
          const remainingSeconds = diffSeconds % 60;
          setTimeAgo(`${diffMinutes}m ${remainingSeconds}s ago`);
        }
      }
    }, 1000);

    return () => clearInterval(interval);
  }, [lastUpdated]);

  const parsedStats = parseSnapshotStats(stats);
  const parsedCSV = csvStats ? parseCSVStats(csvStats) : null;
  const queueStatus = getQueueStatus(metric);

  return (
    <div className="progress-display">
      <div className="progress-header">
        <h3>Solver Progress</h3>
        {lastUpdated && (
          <div className="last-updated-badge">
            Last updated: {timeAgo}
          </div>
        )}
      </div>
      
      {parsedStats ? (
        <div className="parsed-stats">
          <div className="stat-row">
            <span className="stat-label">Total Steps:</span>
            <span className="stat-value">{formatNumber(parsedStats.totalSteps)}</span>
          </div>
          <div className="stat-row">
            <span className="stat-label">Queue Size:</span>
            <span className="stat-value">{formatNumber(parsedStats.queueSize)}</span>
          </div>
          <div className="stat-row">
            <span className="stat-label">Queue Status:</span>
            <span className="stat-value" style={{ color: queueStatus.color }}>
              {queueStatus.status}
            </span>
          </div>
          <div className="stat-row">
            <span className="stat-label">Items Processed:</span>
            <span className="stat-value">{formatNumber(parsedStats.totalDequeues)}</span>
          </div>
          <div className="stat-row">
            <span className="stat-label">Steps Since Queue Activity:</span>
            <span className="stat-value">{formatNumber(parsedStats.stepsSinceDequeue)}</span>
          </div>
        </div>
      ) : (
        <div className="progress-stats">{stats}</div>
      )}
      
      {metric !== null && (
        <div className="progress-metric">
          Queue Ratio: {metric.toFixed(4)}
        </div>
      )}
      
      {parsedCSV && (
        <div className="csv-stats">
          <h4>Performance Metrics</h4>
          <div className="stat-row">
            <span className="stat-label">Solutions Found:</span>
            <span className="stat-value highlight">{formatNumber(parsedCSV.solutionsFound)}</span>
          </div>
          <div className="stat-row">
            <span className="stat-label">Solutions per Day:</span>
            <span className="stat-value">{Math.round(parsedCSV.solutionsFound / 730).toLocaleString()}/day</span>
          </div>
          <div className="stat-row">
            <span className="stat-label">Solve Rate:</span>
            <span className="stat-value">{formatNumber(Math.round(parsedCSV.solveRate))} steps/solution</span>
          </div>
          <div className="stat-row">
            <span className="stat-label">Processing Speed:</span>
            <span className="stat-value">{parsedCSV.stepsPerSecond.toFixed(1)} steps/sec</span>
          </div>
        </div>
      )}
    </div>
  );
};
