// Shared type definitions for the knights tour monitoring application

export interface Snapshot {
  board: string[];
  stats: string;
  metric: number | null;
}
