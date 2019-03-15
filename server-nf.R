library(dplyr)
library(rgdal)
library(ggmap)
library(DT)
library(leaflet)
library(tigris)
library(geojsonio)
library(leaflet)
library(maps)
library(sp)
library(shiny)
library(tidyr)


source("ui-nf.R")
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
})





