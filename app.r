library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)

source('./utils/elimination.r');
source('./solvers/polynomial-regression.r')
source('./solvers/qspline.r')

source('./server.r')

source('./ui/polyreg.r')
source('./ui/qspline.r')
source('./ui.r')

app <- shinyApp(client, server)
runApp(app, port=8000)
