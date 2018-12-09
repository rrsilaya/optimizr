polyregInput <- fluidRow(
  div(
    style='display: flex; margin: 15px; justify-content: center;',
    div(
      style='margin-right: 15px',
      fileInput('polyregInput', 'Choose CSV File',
        accept = c(
          'text/csv',
          'text/comma-separated-values,text/plain',
          '.csv'
        )
      )
    ),
    numericInput('polyreg_estimation', 'Get estimate of: ', 0),
    div(
      style='height: 84px; display: flex; align-items: center; margin-left: 15px',
      checkboxInput('polyregHasHeader', 'File contains header', TRUE)
    )
  )
)

polyregData <- fluidRow(
  box(
    title='Data',
    status='primary',
    collapsible=TRUE,
    numericInput('degree', 'Degree: ', 2, min=2),
    div(
      style='min-height: 300px',
      tableOutput('polyregData')
    )
  ),
  div(
    style="display: flex; flex-direction: column;",
    box(
      title='Graph',
      status='primary',
      width='12',
      plotOutput('polyreg', height='300px')
    ),
    valueBoxOutput('polyreg_estimate', width='12'),
    box(
      title='Function',
      status='primary',
      width='12',
      textOutput('polyregFunction')
    )
  )
)
