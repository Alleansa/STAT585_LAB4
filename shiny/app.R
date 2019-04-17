library(shiny)
library(tidyverse)

ui <- navbarPage("Lab4",
                 tabPanel("Tab1"),
                 tabPanel("Tab2")
)

server <- function(input, output) {
  
}

# Run the application 
shinyApp(ui = ui, server = server)

