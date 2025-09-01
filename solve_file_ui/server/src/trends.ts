import { exec } from 'child_process';
import { promisify } from 'util';
import { STATS_PATH } from './paths';
import { getProcessStats } from './snapshot';

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
      // Convert time range to days for calculation
      const timeRangeDays: { [key: string]: number } = {
        '1day': 1,
        '1week': 7,
        '1month': 30,
        '3months': 90,
        '6months': 180,
        '1year': 365
      };
      
      const requestedDays = timeRangeDays[timeRange];
      if (requestedDays) {
        // Get the actual process uptime to determine total data timespan
        const processStats = await getProcessStats();
        if (processStats && processStats.uptime) {
          const actualTotalDays = processStats.uptime / (24 * 60 * 60);
          const fraction = Math.min(1, requestedDays / actualTotalDays);
          const linesToInclude = Math.max(50, Math.floor(totalLines * fraction));
          startLine = Math.max(1, totalLines - linesToInclude + 1);
          
          console.log(`Time range ${timeRange} (${requestedDays} days): Process running for ${actualTotalDays.toFixed(1)} days, using ${linesToInclude} lines (${(fraction * 100).toFixed(1)}% of data)`);
        } else {
          console.log(`Time range ${timeRange}: Could not determine process uptime, time range filtering disabled - returning all data`);
          // If we can't determine the actual timespan, we can't do meaningful time range filtering
          // So just return all data rather than making up arbitrary estimates
        }
      }
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
