import express from 'express';
import cors from 'cors';
import path from 'path';
import { getSnapshot } from './snapshot';

const app = express();
const PORT = parseInt(process.env.PORT || '8080', 10);

// Middleware
app.use(cors());
app.use(express.json());

// Serve static files from React build
app.use(express.static(path.join(__dirname, '../../client/build')));

// Basic route
app.get('/api/health', (req, res) => {
  res.json({ 
    status: 'OK', 
    timestamp: new Date().toISOString(),
    message: 'Knights Tour Monitor API is running' 
  });
});

// Placeholder routes for puzzle solver data
app.get('/api/stats', (req, res) => {
  res.json({ message: 'Stats endpoint - to be implemented' });
});

app.get('/api/solutions', (req, res) => {
  res.json({ message: 'Solutions endpoint - to be implemented' });
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

// Serve React app for all non-API routes
app.get('/*splat', (req, res) => {
  res.sendFile(path.join(__dirname, '../../client/build', 'index.html'));
});

app.listen(PORT, '0.0.0.0', () => {
  console.log(`Server running on http://0.0.0.0:${PORT}`);
  console.log(`Local access: http://localhost:${PORT}`);
});
