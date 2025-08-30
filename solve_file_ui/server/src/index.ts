import express from 'express';
import cors from 'cors';

const app = express();
const PORT = process.env.PORT || 3001;

// Middleware
app.use(cors());
app.use(express.json());

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

app.listen(PORT, () => {
  console.log(`Server running on http://localhost:${PORT}`);
});
