source("build-histogram.R")
library(shiny)
library(ggplot2)

shinyUI(navbarPage(
  "Rental Prices in the United States",
  # Create a tab for the histogram.
  tabPanel(
    "Histogram",
    titlePanel("Rental Prices on the West Coast in 2019"),
    sidebarPanel(
      selectInput(
        "city1",
        label = "Select a city",
        choices = list(
          "Seattle" = "Seattle, WA",
          "San Francisco" = "San Francisco, CA",
          "Fresno" = "Fresno, CA",
          "Portland" = "Portland, OR", 
          "Los Angeles" = "Los Angeles-Long Beach-Anaheim, CA",
          "Las Vegas" = "Las Vegas, NV", 
          "Olympia" = "Olympia, WA",
          "Vallejo" = "Vallejo, CA", 
          "Santa Cruz" = "Santa Cruz, CA", 
          "Bellingham" = "Bellingham, WA", 
          "Napa" = "Napa, CA",
          "San Jose" = "San Jose, CA",
          "San Luis Obispo" = "San Luis Obispo, CA"
        
          )
      ),
      selectInput(
        "city2",
        label = "Select another city",
        choices = list(
          "Seattle" = "Seattle, WA",
          "San Francisco" = "San Francisco, CA",
          "Fresno" = "Fresno, CA",
          "Portland" = "Portland, OR", 
          "Los Angeles" = "Los Angeles-Long Beach-Anaheim, CA",
          "Las Vegas" = "Las Vegas, NV", 
          "Olympia" = "Olympia, WA", 
          "Vallejo" = "Vallejo, CA", 
          "Santa Cruz" = "Santa Cruz, CA", 
          "Bellingham" = "Bellingham, WA", 
          "Napa" = "Napa, CA", 
          "San Jose" = "San Jose, CA",
          "San Luis Obispo" = "San Luis Obispo, CA"
        )
      )
    ),
    mainPanel(
      plotOutput("histogram")
    )
  )
)
)
