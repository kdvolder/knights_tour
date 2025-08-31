#!/bin/bash

# Knights Tour Moniecho "✅ Deployment complete!"
echo ""
echo "🌐 Application running on: http://localhost:8080"
echo "📺 View server console: screen -r knights-tour-server"
echo "🛑 Stop server: screen -S knights-tour-server -X quit"
echo "📋 List sessions: screen -list" Production Deployment Script

echo "🚀 Starting production deployment..."

# Build the applications
echo "📦 Building applications..."
npm run build

# Start the production server
echo "🌟 Starting production server..."
cd server

# Kill any existing screen session for the server
screen -S knights-tour-server -X quit 2>/dev/null || true

# Start server in a detached screen session
echo "🚀 Server starting at http://localhost:8080"
screen -dmS knights-tour-server bash -c 'NODE_ENV=production node dist/index.js'

# Give server a moment to start
sleep 2

# Check if screen session is running
if screen -list | grep -q knights-tour-server; then
    echo "✅ Server started successfully in screen session"
else
    echo "❌ Server failed to start"
    exit 1
fi

echo "✅ Deployment complete!"
echo ""
echo "🌐 Application running on: http://localhost:3001"
echo "� To stop: Press Ctrl+C"
echo "🔄 To restart: Run ./deploy.sh again"
