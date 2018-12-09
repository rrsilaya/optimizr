Simplex <- function(matr) {
  ROWS = nrow(matr)
  COLS = ncol(matr)

  tableus = list()
  iterations = 0

  tableus[[1]] = matr # initial tableu

  while (any(matr[ROWS, 1:(COLS - 1)] < 0) || any(matr[1:(ROWS - 1), COLS] < 0)) {
    # While last row has negative values
    if (any(matr[1:(ROWS - 1), COLS] < 0)) {
      pivotRow = order(matr[1:(ROWS - 1), COLS])[1]

      testRatio = matr[pivotRow, 1:(COLS - 1)] / matr[pivotRow, COLS]
      testRatio[testRatio <= 0] = -Inf
      pivotCol = order(testRatio)[length(testRatio)]
    } else {
      pivotCol = order(matr[ROWS, 1:(COLS - 1)])[1]

      testRatio = matr[1:(ROWS - 1), COLS] / matr[1:(ROWS - 1), pivotCol]
      testRatio[testRatio <= 0] = Inf
      pivotRow = order(testRatio)[1]
    }

    # Normalize Row
    matr[pivotRow,] = matr[pivotRow,] / matr[pivotRow, pivotCol]

    for (row in 1:ROWS) {
      if (row == pivotRow) next

      zero = matr[row, pivotCol]
      matr[row,] = matr[row,] - zero * matr[pivotRow,]
    }

    iterations = iterations + 1
    tableus[[ iterations + 1 ]] = matr
  }

  result = matrix(0, nrow=3, ncol=5)

  # Get the values
  for (col in 1:15) {
    soln = which(matr[, col] == 1)

    if (length(soln) == 1) {
      # Only one instance of a number == 1

      if ((col %% 5) == 0) {
        c = 5
        r = col %/% 5
      } else {
        c = col %% 5
        r = (col %/% 5) + 1
      }

      result[r, c] = matr[soln, COLS]
    }
  }
  
  return(list(tableus=tableus, iterations=iterations, result=result, profit=matr[ROWS, COLS] * -1))
}
