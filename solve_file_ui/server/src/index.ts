import express from 'express';
import cors from 'cors';
import path from 'path';

const app = express();
const PORT = parseInt(process.env.PORT || '8080', 10);

// Middleware
app.use(cors());
app.use(express.json());

// Serve static files from React build in production
if (process.env.NODE_ENV === 'production') {
  app.use(express.static(path.join(__dirname, '../../client/build')));
}

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

app.get('/api/snapshot', (req, res) => {
  res.json({ message: 'Snapshot endpoint - to be implemented' });
});

// Serve React app for all non-API routes in production
if (process.env.NODE_ENV === 'production') {
  app.get('/*splat', (req, res) => {
    res.sendFile(path.join(__dirname, '../../client/build', 'index.html'));
  });
}

app.listen(PORT, '0.0.0.0', () => {
  console.log(`Server running on http://0.0.0.0:${PORT}`);
  console.log(`Local access: http://localhost:${PORT}`);
});
