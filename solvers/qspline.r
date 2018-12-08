# source('../utils/elimination.r')

QuadraticSplineInterpolation <- function(x, y) {
  n = length(x)
  intervals = n - 1
  unknowns = (3 * intervals) - 1

  matr = matrix(0, unknowns, unknowns + 1)
  fxnOffset = 1

  # CASE 1 Functions
  for (i in 2:(n - 1)) {
    offset = 3 * (i - 2)

    matr[fxnOffset, offset] = x[i] ^ 2 # a
    matr[fxnOffset, offset + 1] = x[i] # b
    matr[fxnOffset, offset + 2] = 1 # c
    matr[fxnOffset, unknowns + 1] = y[i] # RHS

    fxnOffset = fxnOffset + 1

    offset = 3 * (i - 1)
    matr[fxnOffset, offset] = x[i] ^ 2 # a
    matr[fxnOffset, offset + 1] = x[i] # b
    matr[fxnOffset, offset + 2] = 1 # c
    matr[fxnOffset, unknowns + 1] = y[i]
    
    fxnOffset = fxnOffset + 1
  }

  # CASE 2 Functions
  matr[fxnOffset, 1] = x[1] # b
  matr[fxnOffset, 2] = 1 # c
  matr[fxnOffset, unknowns + 1] = y[1] # RHS
  fxnOffset = fxnOffset + 1

  matr[fxnOffset, unknowns - 2] = x[n]^2 # a
  matr[fxnOffset, unknowns - 1] = x[n] # b
  matr[fxnOffset, unknowns] = 1 # c
  matr[fxnOffset, unknowns + 1] = y[n] # RHS
  fxnOffset = fxnOffset + 1

  # CASE 3 Functions
  for (i in 2:(n - 1)) {
    offset = 3 * (i - 2)

    matr[fxnOffset, offset] = x[i] * 2
    matr[fxnOffset, offset + 1] = 1

    offset = 3 * (i - 1)
    matr[fxnOffset, offset] = x[i] * -2
    matr[fxnOffset, offset + 1] = -1

    fxnOffset = fxnOffset + 1
  }

  solution = GaussianElimination(list(
    variables=c(
      paste('a', 2:intervals, sep=''),
      paste('b', 1:intervals, sep=''),
      paste('c', 1:intervals, sep='')
    ),
    matrix=matr
  ))

  values = c(0, solution$solutionSet)
  result = list(functions=list(), coefficients=c('a0', solution$variables), data=data.frame(x=x, y=y))
  func_def = 'function (x) '

  for (i in 1:intervals) {
    fxn = paste(
      func_def,
      values[(i * 3) - 2], ' * x^2 + ',
      values[(i * 3) - 1], ' * x + ',
      values[i * 3],
      sep=''
    )

    result$functions[[i]] = eval(parse(text=fxn))
  }

  return(result)
}

# x = c(50, 50, 50, 70, 70, 70, 80, 80, 80, 90, 90, 90, 100, 100, 100)
# y = c(3.3, 2.8, 2.9, 2.3, 2.6, 2.1, 2.5, 2.9, 2.4, 3.0, 3.1, 2.8, 3.3, 3.5, 3.0)

# QuadraticSplineInterpolation(x, y)
