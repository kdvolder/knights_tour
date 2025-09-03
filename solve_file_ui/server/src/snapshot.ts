
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
let lastValidSnapshot: Snapshot | null = null;

export async function getSnapshot(): Promise<Snapshot> {
  const content = (await readFile(SNAPSHOT_PATH, 'utf8')).trim();
  
  // Validate that we have actual content
  if (!content || content.length < 10) {
    throw new Error('Snapshot file is empty or too short');
  }
  
  const lines = content.split('\n');
  let board: string[] = [];
  let i = 0;
  while (i < lines.length && lines[i].trim() !== '') {
    board.push(lines[i]);
    i++;
  }
  
  // Validate board
  if (board.length === 0) {
    throw new Error('No board data found in snapshot');
  }
  
  // Validate all board rows have same length
  const firstRowLength = board[0].length;
  if (board.some(row => row.length !== firstRowLength)) {
    throw new Error('Board rows have inconsistent lengths');
  }
  
  // Skip the empty line
  i++;
  // Stats is the next line
  const stats = lines[i] || '';
  
  // Validate we have stats
  if (!stats.trim()) {
    throw new Error('No stats data found in snapshot');
  }
  
  // Validate stats format - should contain numbers and colons
  if (!stats.match(/\d+:\s*\d+\s*\/\s*\d+\s*\/\s*\d+/)) {
    throw new Error('Stats line has invalid format');
  }
  
  // Metric is the next line (parse as float)
  const metricLine = lines[i + 1];
  if (!metricLine || metricLine.trim() === '') {
    throw new Error('No metric data found in snapshot');
  }
  
  const metric = parseFloat(metricLine);
  if (isNaN(metric)) {
    throw new Error('Metric is not a valid number');
  }
  
  // Ensure we have enough lines in the file
  if (lines.length < i + 2) {
    throw new Error('Snapshot file appears truncated');
  }
  
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
  
  // Initialize with current valid snapshot to avoid false positives
  getSnapshot().then(snapshot => {
    lastValidSnapshot = snapshot;
    console.log('📚 Initialized lastValidSnapshot for duplicate detection');
  }).catch(error => {
    console.error('Error initializing snapshot:', error);
    lastValidSnapshot = null;
  });
  
  const watcher = fs.watch(SNAPSHOT_PATH, async (eventType) => {
    if (eventType === 'change') {
      try {
        // Parse the current snapshot
        const currentSnapshot = await getSnapshot();
        
        // Compare parsed content, not raw file content
        const snapshotChanged = !lastValidSnapshot || 
          JSON.stringify(currentSnapshot) !== JSON.stringify(lastValidSnapshot);
        
        if (snapshotChanged) {
          lastValidSnapshot = currentSnapshot;
          console.log('📝 Snapshot content changed (parsed), emitting event');
          snapshotEmitter.emit('change');
        } else {
          console.log('⚠️ File watcher triggered but parsed content unchanged, skipping');
        }
      } catch (error) {
        console.error('Error parsing snapshot file during watch (likely partial read):', error);
        // Don't emit event on parse failures - likely partial file read
      }
    }
  });

  // Handle watcher errors
  watcher.on('error', (error) => {
    console.error('Snapshot file watcher error:', error);
    isWatching = false;
    lastValidSnapshot = null;
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
