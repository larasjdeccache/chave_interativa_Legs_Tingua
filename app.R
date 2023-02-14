library(shiny)
library(shinyjs)
library(shinyTree)
library(shinyWidgets)
library(shinythemes)
library(markdown)
library(htmltools)

# Run the application 
shinyApp(ui = "ui.R", server = "server.R")