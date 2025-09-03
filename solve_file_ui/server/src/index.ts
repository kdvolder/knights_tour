import express from 'express';
import cors from 'cors';
import path from 'path';
import { getSnapshot, getProcessStats, startWatching, onSnapshotChange, offSnapshotChange } from './snapshot';
import { getTrendData } from './trends';

const app = express();
const PORT = parseInt(process.env.PORT || '8080', 10);

// Middleware
app.use(cors());
app.use(express.json());

// Serve static files from React build, but only for non-API routes
// app.use((req, res, next) => {
//   if (req.path.startsWith('/api/')) {
//     next(); // Skip static serving for API routes
//   } else {
//     express.static(path.join(__dirname, '../../client/build'))(req, res, next);
//   }
// Serve static files from React build
app.use(express.static(path.join(__dirname, '../../client/build')));

// Basic route
app.get('/api/health', (req, res) => {
  res.json({ 
    status: 'OK', 
    timestamp: new Date().toISOString(),
    message: 'Polyomino Puzzle Solver Monitor API is running' 
  });
});

// Placeholder routes for puzzle solver data
app.get('/api/stats', (req, res) => {
  res.json({ message: 'Stats endpoint - to be implemented' });
});

app.get('/api/solutions', (req, res) => {
  res.json({ message: 'Solutions endpoint - to be implemented' });
});

// Process stats endpoint
app.get('/api/process-stats', async (req, res) => {
  try {
    const processStats = await getProcessStats();
    if (processStats) {
      res.json(processStats);
    } else {
      res.json({ 
        error: 'solve_file process not found',
        message: 'Could not locate the solve_file process. Make sure the solver is running.'
      });
    }
  } catch (error) {
    console.error('Error getting process stats:', error);
    res.status(500).json({ 
      error: 'Failed to get process stats',
      message: error instanceof Error ? error.message : 'Unknown error'
    });
  }
});

// Trends data endpoint
app.get('/api/trends', async (req, res) => {
  try {
    const maxPoints = parseInt(req.query.maxPoints as string) || 1000;
    const timeRange = req.query.timeRange as string;
    const trendData = await getTrendData(maxPoints, timeRange);
    res.setHeader('Content-Type', 'text/csv');
    res.send(trendData);
  } catch (error) {
    console.error('Error getting trend data:', error);
    res.status(500).json({ 
      error: 'Failed to get trend data',
      message: error instanceof Error ? error.message : 'Unknown error'
    });
  }
});

// SSE endpoint for real-time snapshot updates
app.get('/api/snapshotstream', (req, res) => {
  console.log('SSE client connected');
  
  res.writeHead(200, {
    'Content-Type': 'text/event-stream',
    'Cache-Control': 'no-cache',
    'Connection': 'keep-alive',
    'Access-Control-Allow-Origin': '*',
    'Access-Control-Allow-Headers': 'Cache-Control'
  });

  // Listen for snapshot changes
  const handleSnapshotChange = async () => {
    try {
      const snapshot = await getSnapshot();
      const data = JSON.stringify(snapshot);
      res.write(`data: ${data}\n\n`);
    } catch (error) {
      console.error('Error in SSE snapshot update:', error);
      // Don't send error events to client - just log and skip
    }
  };

  // Set up file watching using the proper API
  onSnapshotChange(handleSnapshotChange);

  // Send initial snapshot immediately
  handleSnapshotChange();

  // Clean up on client disconnect
  req.on('close', () => {
    console.log('SSE client disconnected');
    offSnapshotChange(handleSnapshotChange);
  });
});

app.get('/api/snapshot', async (req, res) => {
  try {
    const snapshot = await getSnapshot();
    res.json(snapshot);
  } catch (error) {
    console.error('Error reading snapshot:', error);
    res.status(500).json({ 
      error: 'Failed to read snapshot data',
      message: error instanceof Error ? error.message : 'Unknown error'
    });
  }
});

// Serve React app for all non-API routes (this must be last)
app.get('/*splat', (req, res) => {
  res.sendFile(path.join(__dirname, '../../client/build', 'index.html'));
});

app.listen(PORT, '0.0.0.0', () => {
  console.log(`Server running on http://0.0.0.0:${PORT}`);
  console.log(`Local access: http://localhost:${PORT}`);
  
  // Start watching for snapshot changes
  startWatching();
});
