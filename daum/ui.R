library(shiny)
library(dplyr)

shinyUI(fluidPage(
  fluidRow(
  	titlePanel("GA Data Cleaning"),
  	sidebarPanel(
    fileInput('file1','Choose GA File',accept=c('.tsv')),
    fileInput('file3','Choose User File',accept=c('.tsv')),
    fileInput('file2','Choose Creative List',accept=c('.csv')), width = 3),
	  mainPanel( h4("Live"),
    DT::dataTableOutput("table2"),
    h4("Off"),
    DT::dataTableOutput("table3"))
  )
))
