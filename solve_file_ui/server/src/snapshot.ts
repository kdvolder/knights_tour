
import { readFile } from 'fs/promises';
import fs from 'fs';
import { SNAPSHOT_PATH } from './paths';
import { Snapshot } from '../../shared/types';

// Event emitter for snapshot changes
import { EventEmitter } from 'events';

const snapshotEmitter = new EventEmitter();
let isWatching = false;

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
  return { board, stats, metric };
}

// Start watching the snapshot file for changes
export function startWatching(): void {
  if (isWatching) return;
  
  isWatching = true;
  console.log(`Starting to watch snapshot file: ${SNAPSHOT_PATH}`);
  
  const watcher = fs.watch(SNAPSHOT_PATH, (eventType) => {
    if (eventType === 'change') {
      // Emit snapshot change event
      snapshotEmitter.emit('change');
    }
  });

  // Handle watcher errors
  watcher.on('error', (error) => {
    console.error('Snapshot file watcher error:', error);
    isWatching = false;
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
