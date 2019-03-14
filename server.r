source("torin_and_daniel.R")
library(shiny)

shinyServer(function(input, output) {
  output$histogram <- renderPlot({
    return(histogram(three, input$city1, input$city2))
  })
})