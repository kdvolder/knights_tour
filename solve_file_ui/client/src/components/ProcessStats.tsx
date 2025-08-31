import React from 'react';
import { ProcessStats as ProcessStatsType } from '../../../shared/types';
import './ProcessStats.css';

interface ProcessStatsProps {
  processStats: ProcessStatsType | null;
}

function formatBytes(bytes: number): string {
  if (bytes === 0) return '0 B';
  const k = 1024;
  const sizes = ['B', 'KB', 'MB', 'GB'];
  const i = Math.floor(Math.log(bytes) / Math.log(k));
  return parseFloat((bytes / Math.pow(k, i)).toFixed(1)) + ' ' + sizes[i];
}

function formatUptime(seconds: number): string {
  const days = Math.floor(seconds / 86400);
  const hours = Math.floor((seconds % 86400) / 3600);
  const minutes = Math.floor((seconds % 3600) / 60);
  const secs = Math.floor(seconds % 60);
  
  if (days > 0) {
    return `${days}d ${hours}h ${minutes}m ${secs}s`;
  } else if (hours > 0) {
    return `${hours}h ${minutes}m ${secs}s`;
  } else if (minutes > 0) {
    return `${minutes}m ${secs}s`;
  } else {
    return `${secs}s`;
  }
}

export const ProcessStats: React.FC<ProcessStatsProps> = ({ processStats }) => {
  return (
    <div className="process-stats">
      <h4>Solver Process Statistics</h4>
      {processStats ? (
        <>
          <div className="process-stat-row">
            <span className="stat-label">Runtime:</span>
            <span className="stat-value">{formatUptime(processStats.uptime)}</span>
          </div>
          <div className="process-stat-row">
            <span className="stat-label">Memory (RSS):</span>
            <span className="stat-value">{formatBytes(processStats.memoryUsage.rss)}</span>
          </div>
          {processStats.memoryUsage.heapUsed > 0 && (
            <div className="process-stat-row">
              <span className="stat-label">Heap Used:</span>
              <span className="stat-value">{formatBytes(processStats.memoryUsage.heapUsed)}</span>
            </div>
          )}
          <div className="process-stat-row">
            <span className="stat-label">Process ID:</span>
            <span className="stat-value">{processStats.pid}</span>
          </div>
        </>
      ) : (
        <div className="stat-value" style={{ color: '#999', fontStyle: 'italic' }}>
          solve_file process not found
        </div>
      )}
    </div>
  );
};
