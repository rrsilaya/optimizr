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

source('./server.r')

source('./ui/polyreg.r')
source('./ui/qspline.r')
source('./ui/simplex.r')
source('./ui.r')

app <- shinyApp(client, server)
runApp(app, port=8000)
