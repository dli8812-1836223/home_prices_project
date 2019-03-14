library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
source("ui-td.R")

server_td <- shinyServer(function(input, output) {
  output$scatter <- renderPlotly({
    rents_year <- rents_year %>%
      filter(rents_year[[input$area]] > input$price_range[1],
             rents_year[[input$area]] < input$price_range[2])
    scatter_plot <- plot_ly(rents_year) %>%
      add_trace(x = rents_year$SizeRank,
                y = rents_year[[input$area]],
                type = "scatter",
                mode = "markers",
                hoverinfo = "text",
                color = rents_year[[input$area]],
                colors = c("blue", "red"),
                text = ~paste0(RegionName, "<br>",
                               "Average Rental Price: $",
                               round(rents_year[[input$area]], digits = 2),
                "<br>",
                "Size Rank: ", rents_year$SizeRank)) %>%
      layout(title = "Price vs Area",
             xaxis = list(title = "Size Rank"),
             yaxis = list(title = "Rental Price ($)",
                          showlegend = FALSE))
      if (input$box) {
        scatter_plot <-
        add_lines(scatter_plot, x = rents_year$SizeRank, y = ~fitted(
        loess(rents_year[[input$area]] ~ rents_year$SizeRank)),
        line = list(color = "black"),
        name = "Trendline",
        showlegend = FALSE)
        }
    colorbar(scatter_plot,
             title = "Rental Price ($)")
    scatter_plot
    })})
shinyApp(ui_td, server_td)
