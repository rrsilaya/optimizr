qsplineEstimation <- function(results, x) {
  for (interval in 2:(length(results$functions) + 1)) {
    if (x >= results$data[[1]][interval - 1] && x <= results$data[[1]][interval]) {
      return(results$functions[[interval - 1]](x))
    }
  }

  return(NA)
}

server <- function(input, output) {
  polyreg <- reactiveValues()
  qspline <- reactiveValues()

  # Observers
  observeEvent(input$polyregInput, {
    polyreg$data = read.csv(input$polyregInput$datapath, header=TRUE)
    polyreg$data = polyreg$data[with(polyreg$data, order(polyreg$data[[1]])),]
  })

  observeEvent(input$qsplineInput, {
    qspline$data = read.csv(input$qsplineInput$datapath, header=TRUE)
    qspline$data = qspline$data[with(qspline$data, order(qspline$data[[1]])),]
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
    req(polyreg$data, polyreg$degree, polyreg$degree < (length(polyreg$data[[1]]) - 1))

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
      geom_point(aes(x=x, y=y, color='red'))
    )
  })

  output$polyregFunction <- renderText({
    req(polyreg$result, polyreg$degree < (length(polyreg$data[[1]]) - 1))

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
}