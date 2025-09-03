/**
 * BoardHeatmap - Tracks how long each position on the board has contained the same piece
 * 
 * This creates a "stability heatmap" showing which areas of the board are:
 * - Active/changing (low age values)
 * - Stable/frozen (high age values)
 */
export class BoardHeatmap {
  private id: string;
  private previousBoard: string[][] | null = null;
  private ageMatrix: number[][] = [];
  private width: number = 0;
  private height: number = 0;
  private maxAge: number = 1; // Initialize to 1 to avoid division by zero
  
  // Cached percentile data
  private cachedPercentileMatrix: number[][] | null = null;

  constructor() {
    this.id = Math.random().toString(36).substr(2, 9);
    console.log(`🔥 BoardHeatmap created with ID: ${this.id}`);
  }

  /**
   * Update the heatmap with a new board state
   * @param board Array of strings representing the current board
   */
  updateBoard(board: string[]): void {
    console.log(`📊 Heatmap ${this.id} updating with board:`, board.length, 'rows');
    
    const height = board.length;
    const width = height > 0 ? board[0].length : 0;

    // Initialize or resize age matrix if needed
    if (this.width !== width || this.height !== height) {
      this.initializeAgeMatrix(width, height);
    }

    // Convert string array to 2D character array for easier comparison
    const currentBoard = board.map(row => row.split(''));

    // Check if we had any ages before this update (for reset detection)
    const hadAgesBeforeUpdate = this.previousBoard !== null && 
      this.ageMatrix.some(row => row.some(age => age > 0));

    // Track changes for debugging
    let changedCells = 0;
    let totalCells = width * height;

    // Update age values
    for (let y = 0; y < height; y++) {
      for (let x = 0; x < width; x++) {
        if (this.previousBoard === null) {
          // First update - initialize all positions to age 0
          this.ageMatrix[y][x] = 0;
        } else {
          const currentPiece = currentBoard[y][x];
          const previousPiece = this.previousBoard[y][x];

          if (currentPiece === previousPiece) {
            // Same piece - increment age
            this.ageMatrix[y][x]++;
            // Update maxAge if this cell now has the highest age
            this.maxAge = Math.max(this.maxAge, this.ageMatrix[y][x]);
          } else {
            // Different piece - reset age to 0
            this.ageMatrix[y][x] = 0;
            changedCells++;
          }
        }
      }
    }

    // Debug logging about changes
    if (this.previousBoard !== null) {
      console.log(`🔄 Heatmap ${this.id}: ${changedCells}/${totalCells} cells changed`);
      const zeroAges = this.ageMatrix.flat().filter(age => age === 0).length;
      console.log(`🔢 Heatmap ${this.id}: ${zeroAges} cells with age 0`);
      
      // Detect potential duplicate events (no changes when we expect some)
      if (changedCells === 0) {
        console.warn(`🔁 DUPLICATE EVENT DETECTED in ${this.id} - No cells changed! Same board sent again.`);
        console.log('📋 Board content (first 3 rows):', currentBoard.slice(0, 3).map(row => row.join('')));
      }
    }

    // Check if all ages became 0 after update (indicating a solution or major board change)
    const allAgesAreZeroAfterUpdate = this.ageMatrix.every(row => row.every(age => age === 0));
    
    if (hadAgesBeforeUpdate && allAgesAreZeroAfterUpdate) {
      console.warn(`🎯 SOLUTION DETECTED in ${this.id} - All ages reset to 0! This likely indicates a solved puzzle.`);
      console.log(`� Changed cells: ${changedCells}/${totalCells} (${((changedCells/totalCells)*100).toFixed(1)}%)`);
      console.log('📋 PREVIOUS board (first 3 rows):', this.previousBoard?.slice(0, 3).map(row => row.join('')));
      console.log('📋 NEW board (first 3 rows):', currentBoard.slice(0, 3).map(row => row.join('')));
      
      // Check if board is completely different (solution state) vs just some changes
      if (changedCells === totalCells) {
        console.log('🎉 COMPLETE BOARD CHANGE - Likely solution found!');
      } else {
        console.log('🤔 PARTIAL RESET - May be backtracking or state change');
      }
    }

    // Store current board for next comparison
    this.previousBoard = currentBoard;
    
    // Invalidate percentile cache since age matrix changed
    this.cachedPercentileMatrix = null;
  }

  /**
   * Get the current age matrix
   * @returns 2D array of age values
   */
  getAgeMatrix(): number[][] {
    return this.ageMatrix.map(row => [...row]); // Return a copy
  }

  /**
   * Get the age value at a specific position
   * @param x Column position
   * @param y Row position
   * @returns Age value or -1 if position is invalid
   */
  getAgeAt(x: number, y: number): number {
    if (y >= 0 && y < this.height && x >= 0 && x < this.width) {
      return this.ageMatrix[y][x];
    }
    return -1;
  }

  /**
   * Get the maximum age value ever reached
   * @returns Maximum age value across all positions and all time
   */
  getMaxAge(): number {
    return this.maxAge;
  }

  /**
   * Get statistics about the current heatmap
   * @returns Object with min, max, and average age values
   */
  getStats(): { min: number; max: number; average: number; totalCells: number; maxAgeEver: number } {
    if (this.ageMatrix.length === 0) {
      return { min: 0, max: 0, average: 0, totalCells: 0, maxAgeEver: this.maxAge };
    }

    let min = Infinity;
    let max = -Infinity;
    let sum = 0;
    let count = 0;

    for (let y = 0; y < this.height; y++) {
      for (let x = 0; x < this.width; x++) {
        const age = this.ageMatrix[y][x];
        min = Math.min(min, age);
        max = Math.max(max, age);
        sum += age;
        count++;
      }
    }

    return {
      min: min === Infinity ? 0 : min,
      max: max === -Infinity ? 0 : max,
      average: count > 0 ? sum / count : 0,
      totalCells: count,
      maxAgeEver: this.maxAge
    };
  }

  /**
   * Reset the heatmap (useful for testing or restarting tracking)
   */
  reset(): void {
    this.previousBoard = null;
    this.ageMatrix = [];
    this.width = 0;
    this.height = 0;
    this.maxAge = 1; // Reset to 1 to avoid division by zero
    this.cachedPercentileMatrix = null;
  }

  /**
   * Get normalized age values (0-1 range) for visualization
   * Uses maxAge for consistent scaling across all updates
   * @returns 2D array of normalized age values
   */
  getNormalizedAgeMatrix(): number[][] {
    // maxAge is initialized to 1, so no division by zero
    return this.ageMatrix.map(row => 
      row.map(age => age / this.maxAge)
    );
  }

  /**
   * Get percentile-based color values (0-1 range) for each cell
   * This distributes colors evenly across the board based on age rankings
   * @returns 2D array of percentile values (0 = youngest, 1 = oldest)
   */
  getPercentileMatrix(): number[][] {
    // Return cached result if available (for multiple calls within same render cycle)
    if (this.cachedPercentileMatrix) {
      return this.cachedPercentileMatrix;
    }

    // Calculate percentiles
    const flatAges = this.ageMatrix.flat();
    const sortedAges = [...flatAges].sort((a, b) => a - b);
    const maxAge = Math.max(...flatAges);
    
    // Create percentile matrix
    this.cachedPercentileMatrix = this.ageMatrix.map(row =>
      row.map(age => {
        // Special case: max age pieces are always white (1.0)
        if (age === maxAge) {
          return 1.0;
        }
        
        // Count how many ages are less than this age
        const lowerCount = sortedAges.filter(sortedAge => sortedAge < age).length;
        return flatAges.length > 1 ? lowerCount / (flatAges.length - 1) : 0;
      })
    );
    
    return this.cachedPercentileMatrix;
  }

  /**
   * Get a color value (0-1) for a specific cell using percentile ranking
   * @param x Column position
   * @param y Row position  
   * @returns Color value (0 = red, 1 = white) or -1 if position invalid
   */
  getPercentileColorAt(x: number, y: number): number {
    if (y >= 0 && y < this.height && x >= 0 && x < this.width) {
      const percentileMatrix = this.getPercentileMatrix();
      return percentileMatrix[y][x];
    }
    return -1;
  }

  /**
   * Get the current board state as stored by the heatmap
   * @returns 2D array of characters representing the board, or null if no board has been set
   */
  getCurrentBoard(): string[][] | null {
    return this.previousBoard ? this.previousBoard.map(row => [...row]) : null; // Return a copy
  }

  private initializeAgeMatrix(width: number, height: number): void {
    this.width = width;
    this.height = height;
    this.ageMatrix = Array(height).fill(null).map(() => Array(width).fill(0));
    this.previousBoard = null; // Reset previous board when dimensions change
    this.cachedPercentileMatrix = null;
  }
}
