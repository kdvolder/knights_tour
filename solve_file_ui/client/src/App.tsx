import React from 'react';
import './App.css';
import { SnapshotMonitor } from './components/SnapshotMonitor';

function App() {
  return (
    <div className="App">
      <header className="App-header">
        <h1>Polyomino Puzzle Solver Monitor</h1>
        <p>Real-time monitoring of polyomino puzzle solver progress</p>
      </header>
      <main className="App-main">
        <SnapshotMonitor />
      </main>
    </div>
  );
}

export default App;
