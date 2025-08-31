import React, { useEffect, useState } from 'react';

interface ProgressDisplayProps {
  stats: string;
  metric: number | null;
  lastUpdated: Date | null;
}

export const ProgressDisplay: React.FC<ProgressDisplayProps> = ({ 
  stats, 
  metric, 
  lastUpdated 
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
      <div className="progress-stats">{stats}</div>
      {metric !== null && (
        <div className="progress-metric">
          Metric: {metric.toFixed(4)}
        </div>
      )}
    </div>
  );
};
