library(dplyr)
library(plotly)
library(stringr)
library(rgdal)
library(tidyverse)
library(ggmap)
library(DT)
library(knitr)
library(leaflet)
library(tigris)
library(geojsonio)
library(leaflet)
library(maps)
library(sp)
library(shiny)

source("server-td.R")
source("server-nf.R")
source("server.r")


rents_df <- read.csv("State_MedianRentalPrice_2Bedroom.csv", stringsAsFactors = FALSE)
states <- states(cb=T)

rents_df <- rename_if(rents_df, is.numeric, funs(str_replace(., "X2010.", "2010/")))
rents_df <- rename_if(rents_df, is.numeric, funs(str_replace(., "X2011.", "2011/")))
rents_df <- rename_if(rents_df, is.numeric, funs(str_replace(., "X2012.", "2012/")))
rents_df <- rename_if(rents_df, is.numeric, funs(str_replace(., "X2013.", "2013/")))
rents_df <- rename_if(rents_df, is.numeric, funs(str_replace(., "X2014.", "2014/")))
rents_df <- rename_if(rents_df, is.numeric, funs(str_replace(., "X2015.", "2015/")))
rents_df <- rename_if(rents_df, is.numeric, funs(str_replace(., "X2016.", "2016/")))
rents_df <- rename_if(rents_df, is.numeric, funs(str_replace(., "X2017.", "2017/")))
rents_df <- rename_if(rents_df, is.numeric, funs(str_replace(., "X2018.", "2018/")))
rents_df <- rename_if(rents_df, is.numeric, funs(str_replace(., "X2019.", "2019/")))

geo <- geocode(location = rents_df$RegionName, output = "latlon", source = "google")
rents_df$lon <- geo$lon
rents_df$lat <- geo$lat
rents_df <- rents_df %>% rename(NAME = RegionName)

state_rents <- merge(states, rents_df, by = "NAME")

bins <- c(500, 800, 1100, 1400, 1700, 2000, 2300, 
          2600, 2900, Inf)
pal <- colorBin("Reds", domain = state_rents$`2019/01`, bins = bins) 

labels <- sprintf(
  "<strong>%s</strong><br/>
  Rent Price: $%g ",
  state_rents$NAME, state_rents$`2019/01`
) %>% lapply(htmltools::HTML)

rents_map <- leaflet(state_rents) %>% 
  setView(-96, 37.8, 2) %>% 
  addTiles() %>% 
  addPolygons(
    fillColor = ~pal(state_rents$`2019/01`),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = 2,
    fillOpacity = 6,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 6,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px", direction = "auto")) %>% 
  addLegend(pal = pal, values = ~state_rents$`2019/01`, opacity = 6, 
            title = NULL, position = "bottomright")


#Shiny Integration

server <- shinyServer(function(input, output) {
  
  output$rents_map <- renderLeaflet({
    
    bins <- c(500, 800, 1100, 1400, 1700, 2000, 2300, 
              2600, 2900, Inf)
    pal <- colorBin("Reds", domain = state_rents[[input$year_var]], bins = bins) 
    
    rents_map <- leaflet(state_rents) %>% 
      setView(-96, 37.8, 2) %>% 
      addTiles() %>% 
      addPolygons(
        fillColor = ~pal(state_rents[[input$year_var]]),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = 2,
        fillOpacity = 6,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 6,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px", direction = "auto")) %>% 
      addLegend(rents_map, pal = pal, values = ~state_rents[[input$year_var]], opacity = 6, 
                title = NULL, position = "bottomright")
    
    
    labels <- sprintf(
      "<strong>%s</strong><br/>
      Average Rent Price: $%s ",
      state_rents$NAME, input$year_var
    ) %>% lapply(htmltools::HTML)
    
    rents_map
  })
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
  })
  output$histogram <- renderPlot({
    return(histogram(three, input$city1, input$city2))
  })
})

shinyApp(ui, server)
