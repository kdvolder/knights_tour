import React from 'react';
import './App.css';
import { SnapshotBoard } from './components/SnapshotBoard';

function App() {
  return (
    <div className="App">
      <header className="App-header">
        <h1>Knights Tour Solver Monitor</h1>
        <p>Real-time monitoring of polyomino puzzle solver progress</p>
      </header>
      <main className="App-main">
        <SnapshotBoard />
      </main>
    </div>
  );
}

export default App;
