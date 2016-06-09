library(shiny)
library(dplyr)

shinyUI(fluidPage(
  fluidRow(
    fileInput('file1','Choose File',accept=c('.tsv')),
    selectInput('input', label=h5('Select options'),'', multiple=TRUE, selectize=FALSE),
    tableOutput("contents"),
    verbatimTextOutput('out3'),
    DT::dataTableOutput("table")
  )
))
