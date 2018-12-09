library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(rhandsontable)
library(shinyjs)

source('./utils/elimination.r');
source('./solvers/polynomial-regression.r')
source('./solvers/qspline.r')
source('./solvers/simplex.r')

qsplineEstimation <- function(results, x) {
  # determines interval function given x

  for (interval in 2:(length(results$functions) + 1)) {
    if (x >= results$data[[1]][interval - 1] && x <= results$data[[1]][interval]) {
      return(results$functions[[interval - 1]](x))
    }
  }

  return(NA)
}

neg <- function(val) val * -1

createTableu <- function(dt) {
  # creates tableu given input data table

  ROWS = 9
  COLS = 25

  return(matrix(c(
    -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, neg(dt[4,2]),
    0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, neg(dt[4,3]),
    0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, neg(dt[4,4]),
    0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, neg(dt[4,5]),
    0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1, 0, 0, 0, 0, neg(dt[4,6]),
    1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, dt[1,1],
    0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, dt[2,1],
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, dt[3,1],
    dt[1:3, 2:6], integer(8), 1, 0
  ), nrow=ROWS, ncol=COLS, byrow=TRUE))
}

rownames = c('Denver', 'Phoenix', 'Dallas', 'Demands')
colnames= c('Supply', 'Sacramento', 'Salt Lake', 'Chicago', 'Albeq', 'New York')

initialData = matrix(
  c(
    310, 260, 280, 0,
    10, 6, 3, 180,
    8, 5, 4, 80,
    6, 4, 5, 200,
    5, 3, 5, 160,
    4, 6, 9, 220
  ),
  nrow=length(rownames),
  dimnames=list(c(rownames), c(colnames))
)

server <- function(input, output) {
  polyreg <- reactiveValues()
  qspline <- reactiveValues()
  simplex <- reactiveValues()

  # Observers
  observeEvent(input$polyregInput, {
    polyreg$data = read.csv(input$polyregInput$datapath, header=input$polyregHasHeader)
    polyreg$data = polyreg$data[with(polyreg$data, order(polyreg$data[[1]])),]
  })

  observeEvent(input$qsplineInput, {
    qspline$data = read.csv(input$qsplineInput$datapath, header=input$qsplineHasHeader)
    qspline$data = qspline$data[with(qspline$data, order(qspline$data[[1]])),]
  })

  observeEvent(input$simplexInput, {
    data = hot_to_r(input$simplexInput)
    simplex$data = createTableu(data)

    simplex$result = Simplex(simplex$data)
    disable('previous')
  })

  observeEvent(input$'next', {
    simplex$index = simplex$index + 1

    enable('previous')
    if (simplex$index == length(simplex$result$tableus)) disable('next')
  })

  observeEvent(input$'previous', {
    simplex$index = simplex$index - 1

    if (simplex$index == 1) disable('previous')
    enable('next')
  })

  observe({
    polyreg$'polyreg_estimation' <- input$'polyreg_estimation'
    polyreg$degree <- input$degree

    qspline$'qspline_estimation' <- input$'qspline_estimation'
  })

  # Renders
  output$polyregData <- renderTable(
    { polyreg$data },
    bordered=TRUE,
    width='100%',
    align='c',
    hover=TRUE
  )

  output$qsplineData <- renderTable(
    { qspline$data },
    bordered=TRUE,
    width='100%',
    align='c',
    hover=TRUE
  )

  output$polyreg <- renderPlot({
    req(polyreg$data, polyreg$degree, polyreg$degree < length(polyreg$data[[1]]))

    x = polyreg$data[[1]]
    y = polyreg$data[[2]]

    polyreg_result = PolynomialRegression(x, y, polyreg$degree)
    polyreg$result <- polyreg_result

    series = data.frame(
      x=x,
      y=y,
      predicted_y=polyreg_result$'function'(x)
    )

    return (
      ggplot(
        data=series,
        aes(x=x, y=predicted_y)
      )  + geom_line() + geom_point() +
      geom_point(aes(x=x, y=y, color='red')) +
      theme_minimal() +
      theme(
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position='none'
      )
    )
  })

  output$polyregFunction <- renderText({
    req(polyreg$result, polyreg$degree < length(polyreg$data[[1]]))

    deparse(polyreg$result$'function')[2]
  })

  output$'qsplineFunctions' <- renderTable({
    req(qspline$data)

    x = qspline$data[[1]]
    y = qspline$data[[2]]

    qspline_result = QuadraticSplineInterpolation(x, y)
    qspline$result <- qspline_result

    intervals = c()
    fxns = c()

    for (interval in 2:(length(qspline_result$functions) + 1)) {
      range = paste(x[interval - 1], '<= x <=', x[interval])

      intervals[interval - 1] = range
      fxns[interval - 1] = deparse(qspline_result$functions[[interval - 1]])[2]
    }

    return(head(data.frame(Interval=intervals, Function=fxns)))
  }, bordered=TRUE, width='100%', align='c', hover=TRUE)

  output$'polyreg_estimate' <- renderValueBox({
    req(polyreg$data, polyreg$'polyreg_estimation')

    x = polyreg$data[[1]]
    if (polyreg$'polyreg_estimation' >= x[1] && polyreg$'polyreg_estimation' <= x[length(x)]) {
      estimation = polyreg$result$'function'(polyreg$'polyreg_estimation')
    } else {
      estimation = NA
    }

    valueBox(
      estimation,
      paste('Polynomial Regression Estimation for', polyreg$'polyreg_estimation'),
      icon=icon('stats', lib='glyphicon'),
      color='purple'
    )
  })

  output$'qspline_estimate' <- renderValueBox({
    req(qspline$data, qspline$'qspline_estimation')

    valueBox(
      qsplineEstimation(qspline$result, qspline$'qspline_estimation'),
      paste('Quadratic Spline Interpolation Estimation for', qspline$'qspline_estimation'),
      icon=icon('stats', lib='glyphicon'),
      color='blue'
    )
  })

  output$simplexInput <- renderRHandsontable(
    rhandsontable(initialData, width = '100%', rowHeaderWidth=150, stretchH = 'all')
      %>% hot_validate_numeric(cols = c(1:6), min = 0)
      %>% hot_cell(4, 'Supply', readOnly = TRUE)
  )

  output$simplexResults <- renderRHandsontable({
    req(simplex$result)

    result = simplex$result$result

    row = c('Denver', 'Phoenix', 'Dallas', 'Total', 'Shipping')
    col = c('Sacramento', 'Salt Lake', 'Chicago', 'Albeq', 'New York', 'Total')

    view = matrix(c(
      result[, 1], sum(result[, 1]), sum(result[, 1] * simplex$data[9, 1:3]),
      result[, 2], sum(result[, 2]), sum(result[, 2] * simplex$data[9, 4:6]),
      result[, 3], sum(result[, 3]), sum(result[, 3] * simplex$data[9, 7:9]),
      result[, 4], sum(result[, 4]), sum(result[, 4] * simplex$data[9, 10:12]),
      result[, 5], sum(result[, 5]), sum(result[, 5] * simplex$data[9, 13:15]),
      sum(result[1,]), sum(result[2,]), sum(result[3,]), sum(result), simplex$result$profit
    ), nrow=5, ncol=6, dimnames=list(c(row), c(col)))

    simplex$index = 1
    rhandsontable(view, width='100%', rowHeaderWidth=100, stretchH='all')
  })

  output$tableu <- renderRHandsontable({
    req(simplex$result, simplex$index)

    tableu = simplex$result$tableus[[ simplex$index ]]
    colnames = c(paste('X', 1:15, sep=''), paste('S', 1:8, sep=''), 'C', 'RHS')
    rownames = c(1:8, 'C')

    rhandsontable(
      matrix(tableu, nrow=9, dimnames=list(c(rownames), c(colnames))),
      width = '100%',
      stretchH = 'all'
    )
  })

  output$index <- renderText({
    req(simplex$result)

    return(paste('Iteration ', simplex$index, ' of ', length(simplex$result$tableus), sep=''))
  })

  output$'minCost' <- renderValueBox({
    req(simplex$result)

    valueBox(
      paste('$', simplex$result$profit, sep=''),
      'Minimum Shipping Cost',
      icon=icon('stats', lib='glyphicon'),
      color='blue'
    )
  })

  output$'totalShip' <- renderValueBox({
    req(simplex$result)

    valueBox(
      sum(simplex$result$result),
      'Total Number of Shipping',
      icon=icon('stats', lib='glyphicon'),
      color='blue'
    )
  })
}