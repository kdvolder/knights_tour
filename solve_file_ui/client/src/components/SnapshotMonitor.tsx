import React, { useEffect, useState } from 'react';
import { Snapshot } from '../../../shared/types';
import { Board } from './Board';
import { ProgressDisplay } from './ProgressDisplay';
import './SnapshotMonitor.css';

export const SnapshotMonitor: React.FC = () => {
  const [snapshot, setSnapshot] = useState<Snapshot | null>(null);
  const [lastUpdated, setLastUpdated] = useState<Date | null>(null);

  useEffect(() => {
    console.log('Setting up SSE connection...');
    
    // Set up Server-Sent Events for real-time updates
    const eventSource = new EventSource('/api/snapshotstream');
    
    eventSource.onopen = () => {
      console.log('SSE connection opened');
    };
    
    eventSource.onmessage = (event) => {
      console.log('Received SSE message:', event.data);
      try {
        const newSnapshot = JSON.parse(event.data);
        setSnapshot(newSnapshot);
        setLastUpdated(new Date());
      } catch (error) {
        console.error('Error parsing snapshot data:', error);
      }
    };

    eventSource.onerror = (error) => {
      console.error('SSE connection error:', error);
      console.log('SSE readyState:', eventSource.readyState);
    };

    // Cleanup on unmount
    return () => {
      console.log('Closing SSE connection');
      eventSource.close();
    };
  }, []);

  if (!snapshot) {
    return <div className="loading-message">Loading board...</div>;
  }

  return (
    <div className="snapshot-monitor-container">
      <Board board={snapshot.board} />
      <ProgressDisplay 
        stats={snapshot.stats}
        metric={snapshot.metric}
        lastUpdated={lastUpdated}
      />
    </div>
  );
};
