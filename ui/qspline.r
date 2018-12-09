qsplineInput <- fluidRow(
  div(
    style='display: flex; margin: 15px; justify-content: center;',
    div(
      style='margin-right: 15px',
      fileInput('qsplineInput', 'Choose CSV File',
        accept = c(
          'text/csv',
          'text/comma-separated-values,text/plain',
          '.csv'
        )
      )
    ),
    numericInput('qspline_estimation', 'Get estimate of: ', 0),
    div(
      style='height: 84px; display: flex; align-items: center; margin-left: 15px',
      checkboxInput('qsplineHasHeader', 'File contains header', TRUE)
    )
  )
)

qsplineData <- fluidRow(
  box(
    title='Data',
    status='primary',
    collapsible=TRUE,
    div(
      style='min-height: 300px',
      tableOutput('qsplineData')
    )
  ),
  div(
    style="display: flex; flex-direction: column;",
    valueBoxOutput('qspline_estimate', width='12'),
    box(
      title='Interval Functions',
      status='primary',
      width='12',
      tableOutput('qsplineFunctions')
    )
  )
)

