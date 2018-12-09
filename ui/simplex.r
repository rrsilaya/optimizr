simplexData <- fluidRow(
  box(
    title='Data Input',
    status='primary',
    width='12',
    rHandsontableOutput('simplexInput')
  )
)

simplexResults <- fluidRow(
  box(
    title='Optimized Results',
    status='primary',
    rHandsontableOutput('simplexResults')
  ),
  valueBoxOutput('minCost', width='3'),
  valueBoxOutput('totalShip', width='3')
)

tableu <- fluidRow(
  useShinyjs(),
  box(
    title='Tableu per Iteration',
    status='primary',
    width='12',
    collapsible=TRUE,
    class = 'collapsed-box',
    rHandsontableOutput('tableu'),
    div(
      style='text-align: center; margin-top: 20px;',
      actionButton('previous', 'Previous', width='100px'),
      div(
        style='display: inline-block; margin: 0 10px;',
        textOutput('index')
      ),
      actionButton('next', 'Next', width='100px')
    )
  )
)
