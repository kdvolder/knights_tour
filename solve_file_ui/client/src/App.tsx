import React, { useEffect, useState } from 'react';
import './App.css';
import { Board } from './components/Board';
import { ProgressDisplay } from './components/ProgressDisplay';
import { ProcessStats } from './components/ProcessStats';
import { TrendsChart } from './components/TrendsChart';
import { Snapshot, ProcessStats as ProcessStatsType } from '../../shared/types';

function App() {
  const [snapshot, setSnapshot] = useState<Snapshot | null>(null);
  const [processStats, setProcessStats] = useState<ProcessStatsType | null>(null);
  const [lastUpdated, setLastUpdated] = useState<Date | null>(null);
  const [currentView, setCurrentView] = useState<'dashboard' | 'charts'>('dashboard');

  // Fetch process stats
  const fetchProcessStats = async () => {
    try {
      const response = await fetch('/api/process-stats');
      if (response.ok) {
        const data = await response.json();
        // Check if we got actual process stats or an error message
        if (data.error) {
          console.log('Solver process not found:', data.message);
          setProcessStats(null);
        } else {
          setProcessStats(data);
        }
      } else {
        console.error('Failed to fetch process stats:', response.status);
        setProcessStats(null);
      }
    } catch (error) {
      console.error('Error fetching process stats:', error);
      setProcessStats(null);
    }
  };

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

    // Fetch initial process stats and set up periodic updates
    fetchProcessStats();
    const processStatsInterval = setInterval(fetchProcessStats, 2000); // Update every 2 seconds

    // Cleanup on unmount
    return () => {
      console.log('Closing SSE connection');
      eventSource.close();
      clearInterval(processStatsInterval);
    };
  }, []);

  return (
    <div className="App">
      <header className="App-header">
        <div className="header-content">
          <div>
            <h1>Polyomino Puzzle Solver Monitor</h1>
            <p>Real-time monitoring of polyomino puzzle solver progress</p>
          </div>
          <div className="view-toggle">
            <button 
              className={currentView === 'dashboard' ? 'active' : ''}
              onClick={() => setCurrentView('dashboard')}
            >
              Dashboard
            </button>
            <button 
              className={currentView === 'charts' ? 'active' : ''}
              onClick={() => setCurrentView('charts')}
            >
              Charts
            </button>
          </div>
        </div>
      </header>
      <main className="App-main">
        {currentView === 'dashboard' ? (
          !snapshot ? (
            <div className="loading-message">Loading board...</div>
          ) : (
            <div className="content-container">
              <Board board={snapshot.board} />
              <div className="progress-section">
                <ProgressDisplay 
                  stats={snapshot.stats}
                  metric={snapshot.metric}
                  lastUpdated={lastUpdated}
                  csvStats={snapshot.csvStats}
                  processStats={processStats}
                />
                <ProcessStats processStats={processStats} />
              </div>
            </div>
          )
        ) : (
          <div className="charts-container">
            <TrendsChart />
          </div>
        )}
      </main>
    </div>
  );
}

export default App;
