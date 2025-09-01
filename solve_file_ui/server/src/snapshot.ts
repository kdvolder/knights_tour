
import { readFile } from 'fs/promises';
import fs from 'fs';
import { exec } from 'child_process';
import { promisify } from 'util';
import { SNAPSHOT_PATH, STATS_PATH } from './paths';
import { Snapshot, ProcessStats } from '../../shared/types';

const execAsync = promisify(exec);

// Event emitter for snapshot changes
import { EventEmitter } from 'events';

const snapshotEmitter = new EventEmitter();
let isWatching = false;
let lastSnapshotContent: string | null = null;

export async function getSnapshot(): Promise<Snapshot> {
  const content = (await readFile(SNAPSHOT_PATH, 'utf8')).trim();
  const lines = content.split('\n');
  let board: string[] = [];
  let i = 0;
  while (i < lines.length && lines[i].trim() !== '') {
    board.push(lines[i]);
    i++;
  }
  // Skip the empty line
  i++;
  // Stats is the next line
  const stats = lines[i] || '';
  // Metric is the next line (parse as float)
  const metric = lines[i + 1] ? parseFloat(lines[i + 1]) : null;
  
  // Read the last row from the CSV stats file efficiently
  let csvStats: string | null = null;
  try {
    // Use tail command to get just the last line efficiently
    const { stdout: tailOutput } = await execAsync(`tail -n 1 "${STATS_PATH}"`);
    csvStats = tailOutput.trim() || null;
  } catch (error) {
    console.error('Error reading last line of CSV stats file:', error);
    csvStats = null;
  }
  
  return { board, stats, metric, csvStats };
}

export async function getProcessStats(): Promise<ProcessStats | null> {
  try {
    // Look for the solve_file process
    const { stdout: psOutput } = await execAsync('ps aux');
    const lines = psOutput.split('\n');
    
    // Look for the solve_file command
    const solverProcess = lines.find(line => 
      line.includes('solve_file')
    );
    
    if (!solverProcess) {
      console.log('solve_file process not found');
      return null;
    }
    
    // Parse the ps output to get the PID
    const parts = solverProcess.trim().split(/\s+/);
    const pid = parseInt(parts[1]);
    
    if (isNaN(pid)) {
      console.log('Could not parse PID from ps output');
      return null;
    }
    
    console.log(`Found solve_file process with PID: ${pid}`);
    
    // Get detailed process information using the PID
    const { stdout: statOutput } = await execAsync(`cat /proc/${pid}/stat`);
    const { stdout: statusOutput } = await execAsync(`cat /proc/${pid}/status`);
    
    // Parse /proc/pid/stat for basic info
    const statParts = statOutput.trim().split(' ');
    const userTime = parseInt(statParts[13]) || 0; // utime in clock ticks
    const systemTime = parseInt(statParts[14]) || 0; // stime in clock ticks
    const startTime = parseInt(statParts[21]) || 0; // Process start time in clock ticks
    
    // Parse /proc/pid/status for memory info
    const statusLines = statusOutput.split('\n');
    const vmRSSLine = statusLines.find(line => line.startsWith('VmRSS:'));
    const vmSizeLine = statusLines.find(line => line.startsWith('VmSize:'));
    
    const rssKB = vmRSSLine ? parseInt(vmRSSLine.split(/\s+/)[1]) || 0 : 0;
    const vmSizeKB = vmSizeLine ? parseInt(vmSizeLine.split(/\s+/)[1]) || 0 : 0;
    
    // Convert to bytes
    const rss = rssKB * 1024;
    const vmSize = vmSizeKB * 1024;
    
    // Calculate uptime (approximate)
    const systemUptimeSeconds = parseFloat((await execAsync('cat /proc/uptime')).stdout.split(' ')[0]);
    const clockTicksPerSecond = 100; // Usually 100 on Linux, but can vary
    const processStartSeconds = startTime / clockTicksPerSecond;
    const processUptime = systemUptimeSeconds - processStartSeconds;
    
    return {
      memoryUsage: {
        rss: rss,
        heapUsed: 0, // Not available for external processes
        heapTotal: vmSize,
        external: 0 // Not available for external processes
      },
      uptime: Math.max(0, processUptime),
      cpuUsage: {
        user: userTime,
        system: systemTime
      },
      pid: pid
    };
    
  } catch (error) {
    console.error('Error getting solve_file process stats:', error);
    return null;
  }
}

// Start watching the snapshot file for changes
export function startWatching(): void {
  if (isWatching) return;
  
  isWatching = true;
  console.log(`Starting to watch snapshot file: ${SNAPSHOT_PATH}`);
  
  const watcher = fs.watch(SNAPSHOT_PATH, async (eventType) => {
    if (eventType === 'change') {
      try {
        // Read current content
        const currentContent = await readFile(SNAPSHOT_PATH, 'utf8');
        
        // Only emit if content actually changed
        if (currentContent !== lastSnapshotContent) {
          lastSnapshotContent = currentContent;
          console.log('📝 Snapshot file content changed, emitting event');
          snapshotEmitter.emit('change');
        } else {
          console.log('⚠️ File watcher triggered but content unchanged, skipping');
        }
      } catch (error) {
        console.error('Error reading snapshot file during watch:', error);
      }
    }
  });

  // Handle watcher errors
  watcher.on('error', (error) => {
    console.error('Snapshot file watcher error:', error);
    isWatching = false;
    lastSnapshotContent = null;
  });
}

// Subscribe to snapshot changes
export function onSnapshotChange(callback: () => void): void {
  snapshotEmitter.on('change', callback);
}

// Unsubscribe from snapshot changes
export function offSnapshotChange(callback: () => void): void {
  snapshotEmitter.off('change', callback);
}
