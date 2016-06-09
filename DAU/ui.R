library(shiny)
library(dplyr)
library(stringi)
library(tidyr)

shinyUI(fluidPage(
  fluidRow(
  	titlePanel("GA Data Cleaning"),
  	sidebarPanel(
    fileInput('file1','Choose Conv. File',accept=c('.tsv')),
    fileInput('file2','Choose User File',accept=c('.tsv')),
    selectInput('input', label=h5('Select options'),'', multiple=TRUE, selectize=FALSE), width = 3),
    mainPanel(
    h4("Selected options"),
    verbatimTextOutput('out3'),
    h4("Conv."),
    DT::dataTableOutput("table"),
    h4("User"),
    DT::dataTableOutput("table1"),
    h4("Raw"),
    tableOutput("contents"))
  )
))
