# swaps to rows of a matrix
swap <- function(matrix, rules) {
  matrix[rules[1],] = (matrix[rules[2],] - matrix[rules[1],]) + (matrix[rules[2],] = matrix[rules[1],]);
  return(matrix);
};

GaussJordanElimination <- function(augcoeff) {
  size = length(augcoeff$variables);
  result = list(solutionSet = NA, variables = augcoeff$variables, matrix = augcoeff$matrix);

  for (row in 1:size) {
    # Pivoting
    if (row != size) {
      pivot = order(abs(result$matrix[row:size, row]), decreasing = TRUE)[1] + row - 1; # get the max value

      if (result$matrix[pivot, row] == 0) {
        # no unique soluution exists
        return (result);
      }
      
      result$matrix = swap(result$matrix, c(pivot, row));
    }

    result$matrix[row,] = result$matrix[row,] / result$matrix[row, row]; # divide row with normalized cell

    for (col in 1:size) {
      if (col == row) next;

      result$matrix[col,] = result$matrix[col,] - (result$matrix[col, row] * result$matrix[row,]); # get new value of cell
    }
  }

  result$solutionSet = c();
  for (row in 1:size) {
    result$solutionSet[row] = result$matrix[row, size + 1]; # place the results in a vector
  }

  return(result);
}

GaussianElimination <- function(augcoeff) {
  size = length(augcoeff$variables);
  result = list(solutionSet = NA, variables = augcoeff$variables, matrix = augcoeff$matrix);

  # Forward Elimination
  for (row in 1:(size - 1)) {
    # Pivoting
    pivot = order(abs(result$matrix[row:size, row]), decreasing = TRUE)[1] + row - 1;

    if (result$matrix[pivot, row] == 0) {
      # no unique soluution exists
      return (result);
    }
    
    result$matrix = swap(result$matrix, c(pivot, row));

    # Get the new values of the row
    for (col in (row + 1):size) {
      normalized = (result$matrix[col, row] / result$matrix[row, row]) * result$matrix[row,];
      result$matrix[col,] = result$matrix[col,] - normalized;
    }
  }

  # Backward Substitution
  result$solutionSet = integer(size);
  result$solutionSet[size] = result$matrix[size, size + 1] / result$matrix[size, size]; # bottom value
  for (eq in (size - 1):1) {
    result$solutionSet[eq] = (result$matrix[eq, size + 1] - sum(result$matrix[eq, (eq + 1):size] * result$solutionSet[(eq + 1):size])) / result$matrix[eq, eq];
  }

  return(result);
}
