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

shinyUI(navbarPage(
  "Rent Prices Project",
  tabPanel(
    "US Map",
    titlePanel("Rent Prices By State"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = year_var,
          label = h3("Select Year"),
          choices = list(
            "2019 Rent Prices" = "2019/01",
            "2018 Rent Prices" = "2018/01",
            "2017 Rent Prices" = "2017/01",
            "2016 Rent prices" = "2016/01",
            "2015 Rent Prices" = "2015/01"
          ),
          selected = "2019 Rent Prices"
        ),
        mainPanel(
          leafletOutput("rents_map")
        )
      )
    )
  )
))  











