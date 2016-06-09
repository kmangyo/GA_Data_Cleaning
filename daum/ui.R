library(shiny)
library(dplyr)

shinyUI(fluidPage(
  fluidRow(
    fileInput('file1','Choose GA File',accept=c('.tsv')),
    fileInput('file2','Choose Creative List',accept=c('.csv')),
#    selectInput('input', label=h5('Select options'),'', multiple=TRUE, selectize=TRUE, width = 300),
#    tableOutput("contents"),
#    verbatimTextOutput('out3'),
#    DT::dataTableOutput("table1"),
    DT::dataTableOutput("table2"),
    DT::dataTableOutput("table3")
  )
))
