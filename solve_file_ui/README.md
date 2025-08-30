# Knights Tour Puzzle Solver Monitor

A web-based dashboard for monitoring the progress of a long-running polyomino puzzle solver.

## Project Structure

```
solve_file_ui/
├── client/          # React TypeScript frontend
├── server/          # Node.js TypeScript backend
└── package.json     # Root package with scripts to run both
```

## Quick Start

1. **Install dependencies:**
   ```bash
   # Install server dependencies
   cd server && npm install
   
   # Install client dependencies  
   cd ../client && npm install
   ```

2. **Development mode (runs both client and server):**
   ```bash
   npm run dev
   ```
   - Frontend: http://localhost:3000
   - Backend: http://localhost:3001

3. **Production build:**
   ```bash
   npm run build
   npm start
   ```

## API Endpoints

- `GET /api/health` - Server health check
- `GET /api/stats` - Puzzle solver statistics (to be implemented)
- `GET /api/solutions` - Found solutions (to be implemented)  
- `GET /api/snapshot` - Current puzzle state (to be implemented)

## Features (Planned)

- Real-time progress monitoring
- Performance metrics visualization
- Solution browsing
- Board state visualization
- Historical trend analysis

## Development

- Frontend: React 18 + TypeScript
- Backend: Node.js + Express + TypeScript
- Build tool: Create React App + TypeScript compiler
