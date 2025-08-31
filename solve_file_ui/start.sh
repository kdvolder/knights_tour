#!/bin/bash

# Simple start script for Knights Tour Monitor
# Kills any existing processes, builds everything, and starts both client and server

echo "🧹 Cleaning up any existing processes..."
pkill -f "node.*knights_tour" 2>/dev/null || true
pkill -f "npm.*dev" 2>/dev/null || true
pkill -f "react-scripts" 2>/dev/null || true
pkill -f "nodemon" 2>/dev/null || true

echo "🔨 Building server..."
cd server && npm run build
if [ $? -ne 0 ]; then
    echo "❌ Server build failed"
    exit 1
fi

echo "🚀 Starting server..."
npm start &
SERVER_PID=$!

# Wait a moment for server to start
sleep 2

echo "🌐 Starting client..."
cd ../client && npm start &
CLIENT_PID=$!

echo "✅ Both services started!"
echo "🔗 Client: http://localhost:3000"
echo "🔗 Server: http://localhost:8080"
echo ""
echo "To stop both services, press Ctrl+C"

# Wait for either process to exit
wait $SERVER_PID $CLIENT_PID
