
import { readFile } from 'fs/promises';
import { SNAPSHOT_PATH } from './paths';
import { Snapshot } from '../../shared/types';

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
