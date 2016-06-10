library(shiny)
library(dplyr)

shinyUI(fluidPage(
  fluidRow(
  	titlePanel("GA Data Cleaning"),
  	mainPanel(
    fileInput('file1','Choose GA File',accept=c('.tsv')),
    fileInput('file2','Choose Creative List',accept=c('.csv')),
	  h4("Live"),
    DT::dataTableOutput("table2"),
    h4("Off"),
    DT::dataTableOutput("table3"))
  )
))
