// Shared type definitions for the knights tour monitoring application

export interface ProcessStats {
  memoryUsage: {
    rss: number;        // Resident Set Size (total memory)
    heapUsed: number;   // Heap memory used
    heapTotal: number;  // Heap memory allocated
    external: number;   // External memory
  };
  uptime: number;       // Process uptime in seconds
  cpuUsage: {
    user: number;       // CPU time spent in user code
    system: number;     // CPU time spent in system calls
  };
  pid: number;          // Process ID
}

export interface Snapshot {
  board: string[];
  stats: string;
  metric: number | null;
  csvStats: string | null; // Last row from stats CSV file
}
