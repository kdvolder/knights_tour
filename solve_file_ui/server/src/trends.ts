import { exec } from 'child_process';
import { promisify } from 'util';
import { STATS_PATH } from './paths';

const execAsync = promisify(exec);

export interface TrendDataPoint {
  steps: number;           // Total steps (x-axis)
  queueSize: number;       // Queue size (y-axis for now)
  queueGrowthRate: number; // Future use
  solutions: number;       // Future use
  stepsPerSecond: number;  // Future use
}

export async function getTrendData(maxPoints: number = 1000): Promise<string> {
  try {
    // First, get the total number of lines to calculate sampling interval
    const { stdout: wcOutput } = await execAsync(`wc -l "${STATS_PATH}"`);
    const totalLines = parseInt(wcOutput.split(' ')[0]);
    
    if (totalLines === 0) {
      return 'steps,queueSize,queueGrowthRate,solutions,stepsPerSecond\n';
    }
    
    // Calculate sampling interval - take every Nth line to get ~maxPoints
    const interval = Math.max(1, Math.floor(totalLines / maxPoints));
    
    console.log(`Sampling CSV: ${totalLines} total lines, taking every ${interval}th line for ${Math.floor(totalLines / interval)} points`);
    
    // Use awk to efficiently sample every Nth line and extract relevant columns
    // Extract columns: 1=steps, 4=queue-size, 5=queue-growth-rate, 6=solutions, 8=avg-steps-per-second
    const { stdout: sampledOutput } = await execAsync(
      `awk -F',' 'NR % ${interval} == 0 { print $1","$4","$5","$6","$8 }' "${STATS_PATH}"`
    );
    
    // Add header and return
    const header = 'steps,queueSize,queueGrowthRate,solutions,stepsPerSecond\n';
    return header + sampledOutput;
    
  } catch (error) {
    console.error('Error reading trend data from CSV:', error);
    return 'steps,queueSize,queueGrowthRate,solutions,stepsPerSecond\n';
  }
}
