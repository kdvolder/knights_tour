import path from 'path';

// Root directory containing the important log files
export const LOG_DIR = path.resolve(__dirname, '../../../saves/2023.09.30-13.40.25');

// Derived paths for each important file
export const SNAPSHOT_PATH = path.join(LOG_DIR, 'snapshot-treequence.txt');
export const STATS_PATH = path.join(LOG_DIR, 'stats-treequence.csv');
export const SOLUTIONS_PATH = path.join(LOG_DIR, 'solutions.txt');

// Add more paths here as needed
