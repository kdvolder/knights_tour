import React, { useState, useEffect, useCallback, useMemo } from 'react';
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer } from 'recharts';
import './TrendsChart.css';

interface TrendDataPoint {
  steps: number;
  queueSize: number;
  queueGrowthRate: number;
  solutions: number;
  stepsPerSecond: number;
}

interface TrendsChartProps {
  // For now, no props needed - component will fetch its own data
}

type MetricType = 'queueSize' | 'queueGrowthRate' | 'solutions' | 'stepsPerSecond';

const METRIC_OPTIONS = [
  { value: 'queueSize' as MetricType, label: 'Queue Size', color: '#42a5f5' },
  { value: 'queueGrowthRate' as MetricType, label: 'Queue Growth Rate', color: '#ff6b35' },
  { value: 'solutions' as MetricType, label: 'Solutions Found', color: '#66bb6a' },
  { value: 'stepsPerSecond' as MetricType, label: 'Processing Speed (steps/sec)', color: '#ffa726' }
];

export const TrendsChart: React.FC<TrendsChartProps> = React.memo(() => {
  const [data, setData] = useState<TrendDataPoint[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [selectedMetric, setSelectedMetric] = useState<MetricType>('queueSize');

  const fetchTrendData = useCallback(async () => {
    try {
      setLoading(true);
      const response = await fetch('/api/trends');
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }
      
      const csvText = await response.text();
      const parsedData = parseCsvData(csvText);
      setData(parsedData);
      setError(null);
    } catch (err) {
      console.error('Error fetching trend data:', err);
      setError(err instanceof Error ? err.message : 'Failed to fetch trend data');
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchTrendData();
  }, [fetchTrendData]);

  const parseCsvData = useCallback((csvText: string): TrendDataPoint[] => {
    const lines = csvText.trim().split('\n');
    const dataLines = lines.slice(1); // Skip header
    
    return dataLines.map(line => {
      const [steps, queueSize, queueGrowthRate, solutions, stepsPerSecond] = line.split(',').map(Number);
      return {
        steps,
        queueSize,
        queueGrowthRate,
        solutions,
        stepsPerSecond
      };
    }).filter(point => !isNaN(point.steps) && !isNaN(point.queueSize));
  }, []);

  const formatSteps = useCallback((value: number) => {
    if (value >= 1e9) return `${(value / 1e9).toFixed(1)}B`;
    if (value >= 1e6) return `${(value / 1e6).toFixed(1)}M`;
    if (value >= 1e3) return `${(value / 1e3).toFixed(1)}K`;
    return value.toString();
  }, []);

  const formatMetricValue = useCallback((value: number, metric: MetricType) => {
    switch (metric) {
      case 'queueSize':
      case 'solutions':
        return value.toLocaleString();
      case 'queueGrowthRate':
        return value.toFixed(2);
      case 'stepsPerSecond':
        return value.toFixed(1);
      default:
        return value.toString();
    }
  }, []);

  const getCurrentMetricOption = useCallback(() => {
    return METRIC_OPTIONS.find(option => option.value === selectedMetric) || METRIC_OPTIONS[0];
  }, [selectedMetric]);

  const chartData = useMemo(() => data, [data]);

  if (loading) {
    return (
      <div className="trends-chart">
        <div className="chart-header">
          <h3>Queue Size Trends</h3>
        </div>
        <div className="chart-loading">Loading historical data...</div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="trends-chart">
        <div className="chart-header">
          <h3>Queue Size Trends</h3>
        </div>
        <div className="chart-error">Error: {error}</div>
      </div>
    );
  }

  return (
    <div className="trends-chart">
      <div className="chart-header">
        <h3>Solver Trends</h3>
        <div className="chart-controls">
          <select 
            value={selectedMetric} 
            onChange={(e) => setSelectedMetric(e.target.value as MetricType)}
            className="metric-selector"
          >
            {METRIC_OPTIONS.map(option => (
              <option key={option.value} value={option.value}>
                {option.label}
              </option>
            ))}
          </select>
          <div className="chart-info">
            {data.length} data points across 2+ years
          </div>
        </div>
      </div>
      <div className="chart-container">
        <ResponsiveContainer width="100%" height={400}>
          <LineChart data={chartData} margin={{ top: 20, right: 30, left: 20, bottom: 20 }}>
            <CartesianGrid strokeDasharray="3 3" stroke="#333" />
            <XAxis 
              dataKey="steps" 
              tickFormatter={formatSteps}
              stroke="#666"
            />
            <YAxis 
              tickFormatter={(value) => formatMetricValue(value, selectedMetric)}
              stroke="#666"
            />
            <Tooltip 
              formatter={(value: number, name: string) => [
                formatMetricValue(value, selectedMetric),
                getCurrentMetricOption().label
              ]}
              labelFormatter={(value: number) => `Steps: ${formatSteps(value)}`}
              contentStyle={{
                backgroundColor: '#2a2a2a',
                border: '1px solid #444',
                borderRadius: '4px',
                color: '#fff'
              }}
            />
            <Line 
              type="monotone" 
              dataKey={selectedMetric}
              stroke={getCurrentMetricOption().color}
              strokeWidth={2}
              dot={false}
              activeDot={{ r: 4, fill: getCurrentMetricOption().color }}
            />
          </LineChart>
        </ResponsiveContainer>
      </div>
    </div>
  );
});
