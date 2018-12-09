PolynomialRegression <- function(x, y, degree) {
  augcoeff = matrix(0, degree + 1, degree + 2);

  # Get summation of values
  summations = c();
  for (power in 0:(2 * degree)) {
    summations[power + 1] = sum(x^power);
  }
  print(summations)

  # Augmented Coefficient matrix
  for (row in 1:(degree + 1)) {
    for (col in 1:(degree + 1)) {
      augcoeff[row, col] = summations[(row - 1) + (col - 1) + 1];
    }
  }

  # RHS Summations
  for (row in 1:(degree + 1)) {
    augcoeff[row, degree + 2] = sum((x^(row - 1)) * y)
  }

  result = GaussJordanElimination(list(variables=paste('x', 0:degree, sep=''), matrix=augcoeff));

  # Create the function
  fxn = 'function(x) ';
  for (deg in degree:1) {
    fxn = paste(fxn, result$solutionSet[deg + 1], ' * (x^', deg, ') + ', sep='');
  }
  fxn = paste(fxn, result$solutionSet[1], sep='');

  return(list(coefficients=result$solutionSet, 'function'=eval(parse(text=fxn))));
}
