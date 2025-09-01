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

export async function getTrendData(maxPoints: number = 1000, timeRange?: string): Promise<string> {
  try {
    // First, get the total number of lines to calculate sampling interval
    const { stdout: wcOutput } = await execAsync(`wc -l "${STATS_PATH}"`);
    const totalLines = parseInt(wcOutput.split(' ')[0]);
    
    if (totalLines === 0) {
      return 'steps,queueSize,queueGrowthRate,solutions,stepsPerSecond\n';
    }
    
    let startLine = 1;
    let endLine = totalLines;
    
    // If a time range is specified, calculate which lines to sample from
    if (timeRange && timeRange !== 'all') {
      const timeRangeMultipliers: { [key: string]: number } = {
        '1day': 0.001,    // Last 0.1% of data
        '1week': 0.01,    // Last 1% of data
        '1month': 0.05,   // Last 5% of data
        '3months': 0.15,  // Last 15% of data
        '6months': 0.3,   // Last 30% of data
        '1year': 0.6      // Last 60% of data
      };
      
      const multiplier = timeRangeMultipliers[timeRange] || 1;
      const linesToInclude = Math.floor(totalLines * multiplier);
      startLine = Math.max(1, totalLines - linesToInclude + 1);
      
      console.log(`Time range ${timeRange}: Using lines ${startLine} to ${endLine} (${linesToInclude} lines)`);
    }
    
    // Calculate sampling interval - take every Nth line to get ~maxPoints from the selected range
    const rangeLines = endLine - startLine + 1;
    const interval = Math.max(1, Math.floor(rangeLines / maxPoints));
    
    console.log(`Sampling CSV: ${totalLines} total lines, range ${startLine}-${endLine} (${rangeLines} lines), taking every ${interval}th line for ~${Math.floor(rangeLines / interval)} points`);
    
    // Use awk to efficiently sample every Nth line from the specified range
    // Extract columns: 1=steps, 4=queue-size, 5=queue-growth-rate, 6=solutions, 8=avg-steps-per-second
    const { stdout: sampledOutput } = await execAsync(
      `awk -F',' 'NR >= ${startLine} && NR <= ${endLine} && (NR - ${startLine}) % ${interval} == 0 { print $1","$4","$5","$6","$8 }' "${STATS_PATH}"`
    );
    
    // Add header and return
    const header = 'steps,queueSize,queueGrowthRate,solutions,stepsPerSecond\n';
    return header + sampledOutput;
    
  } catch (error) {
    console.error('Error reading trend data from CSV:', error);
    return 'steps,queueSize,queueGrowthRate,solutions,stepsPerSecond\n';
  }
}
