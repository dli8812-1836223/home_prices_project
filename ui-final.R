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
library(htmltools)

source("ui-td.R")
source("ui-nf.R")
source("ui.R")


rents_df <- read.csv("Metro_MedianRentalPrice_2Bedroom.csv",
                     stringsAsFactors = FALSE)
rents_df <- rename_if(rents_df, is.numeric, funs(str_replace(., "X2010.",
                                                             "2010/")))
rents_df <- rename_if(rents_df, is.numeric, funs(str_replace(., "X2011.",
                                                             "2011/")))
rents_df <- rename_if(rents_df, is.numeric, funs(str_replace(., "X2012.",
                                                             "2012/")))
rents_df <- rename_if(rents_df, is.numeric, funs(str_replace(., "X2013.",
                                                             "2013/")))
rents_df <- rename_if(rents_df, is.numeric, funs(str_replace(., "X2014.",
                                                             "2014/")))
rents_df <- rename_if(rents_df, is.numeric, funs(str_replace(., "X2015.",
                                                             "2015/")))
rents_df <- rename_if(rents_df, is.numeric, funs(str_replace(., "X2016.",
                                                             "2016/")))
rents_df <- rename_if(rents_df, is.numeric, funs(str_replace(., "X2017.",
                                                             "2017/")))
rents_df <- rename_if(rents_df, is.numeric, funs(str_replace(., "X2018.",
                                                             "2018/")))
rents_df <- rename_if(rents_df, is.numeric, funs(str_replace(., "X2019.",
                                                             "2019/")))

rents_test <- rents_df %>%
  mutate(mean_2010 = rowMeans(rents_df[3:14], na.rm = TRUE),
         mean_2011 = rowMeans(rents_df[15:26], na.rm = TRUE),
         mean_2012 = rowMeans(rents_df[27:38], na.rm = TRUE),
         mean_2013 = rowMeans(rents_df[39:50], na.rm = TRUE),
         mean_2014 = rowMeans(rents_df[51:62], na.rm = TRUE),
         mean_2015 = rowMeans(rents_df[63:74], na.rm = TRUE),
         mean_2016 = rowMeans(rents_df[75:86], na.rm = TRUE),
         mean_2017 = rowMeans(rents_df[87:98], na.rm = TRUE),
         mean_2018 = rowMeans(rents_df[99:110], na.rm = TRUE),
         mean_2019 = rowMeans(rents_df[111], na.rm = TRUE))

rents_year <- rents_test[, c(1:2, 112:121)]

min_list <- c(min(rents_year$mean_2010, na.rm = TRUE),
              min(rents_year$mean_2011, na.rm = TRUE),
              min(rents_year$mean_2012, na.rm = TRUE),
              min(rents_year$mean_2013, na.rm = TRUE),
              min(rents_year$mean_2014, na.rm = TRUE),
              min(rents_year$mean_2015, na.rm = TRUE),
              min(rents_year$mean_2016, na.rm = TRUE),
              min(rents_year$mean_2017, na.rm = TRUE),
              min(rents_year$mean_2018, na.rm = TRUE),
              min(rents_year$mean_2019, na.rm = TRUE)
)

max_list <- c(max(rents_year$mean_2010, na.rm = TRUE),
              max(rents_year$mean_2011, na.rm = TRUE),
              max(rents_year$mean_2012, na.rm = TRUE),
              max(rents_year$mean_2013, na.rm = TRUE),
              max(rents_year$mean_2014, na.rm = TRUE),
              max(rents_year$mean_2015, na.rm = TRUE),
              max(rents_year$mean_2016, na.rm = TRUE),
              max(rents_year$mean_2017, na.rm = TRUE),
              max(rents_year$mean_2018, na.rm = TRUE),
              max(rents_year$mean_2019, na.rm = TRUE)
)



ui <- shinyUI(navbarPage(
  "Rental Prices",
  tabPanel(
    "Project Introduction",
    p("Introduction
      The current housing shortage is affecting people nationwide, as prices have continued to skyrocket in the past few years. Potential buyers are forced to rent for longer periods of time, as the competitive market and rising rates will not allow for affordability to emerging independents. More and more people are forced to live with several others or move home with parents, as they are tired of paying for something they are never going to own. To provide context, rents in the third quarter of 2018 were up 2.9 percent compared with a year ago. As college students, we are always concerned about where we can find the best cost-efficient housing. Thus, we decided to explore a dataset with housing rental prices. 
      
      
      Our Audience
      Our primary target audience is college students who are planning to graduate and are unsure where they may live next year. During their searches for employment, that may mean they need to live outside their homes, and thus need to find a place to stay. Providing rental prices for locations nationally will give people an idea of where they might consider living. 
      
      
      Inspirations to look deeper
      With rising prices in the housing market, there are plenty of unanswered questions that can be answered through out data sets. 
      
      Some questions we believe could be answered include:
        
        - Is there a relationship between size of the city and rental prices?
        
        - What region has more expensive rental prices?
        
        - Where have housing prices been more static and changing?",
      a("zillow", href="https://www.zillow.com/research/data/")
    )
  ),

  tabPanel(
    "US Map",
    titlePanel("Rent Prices By State"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "year_var",
          label = h3("Select Year"),
          choices = list(
            "2019 Rent Prices" = "2019/01",
            "2018 Rent Prices" = "2018/01",
            "2017 Rent Prices" = "2017/01",
            "2016 Rent prices" = "2016/01",
            "2015 Rent Prices" = "2015/01"
          ),
          selected = "2019 Rent Prices"
        )
      ),
      mainPanel(
        leafletOutput("rents_map"),
        p("This is a choropleth map of the United States and the state average 
          rent prices. The data displayed currently is the rent prices collected
          from 2019. In the side panel bar, you (as the user) can choose to 
          examine data from the previous 5 years. The map is color coded into 9 
          different price ranges, which also correlates to the legend displayed
          in the bottom right corner.")
      )
    )
  ), #closes tab panel 1
  tabPanel("Price by Area",
           titlePanel("Rental Price by Size Rank, Per Year"),
           sidebarLayout(
             sidebarPanel(
               selectInput("area",
                           label = "Year to Analyze",
                           choices = list("2010" = "mean_2010",
                                          "2011" = "mean_2011",
                                          "2012" = "mean_2012",
                                          "2013" = "mean_2013",
                                          "2014" = "mean_2014",
                                          "2015" = "mean_2015",
                                          "2016" = "mean_2016",
                                          "2017" = "mean_2017",
                                          "2018" = "mean_2018",
                                          "2019" = "mean_2019"),
                           selected = "mean_2019"),
               sliderInput("price_range",
                           label = "Range of Rental Price",
                           min = min(min_list),
                           max = max(max_list),
                           value = c(min(min_list),
                                     max(max_list))),
               checkboxInput("box",
                             label = "Show Trendline",
                             value = TRUE)
             ),
             mainPanel(plotlyOutput("scatter"),
                       p("The chart above depicts the relationship between each city's
                         size rank, their size ranked in the top 350 biggest cities,
                         and the average rental price in the given year. The data
                         includes the average of the United States as a whole as the
                         marker with a size rank of 0. The relationship shows that the 
                         more sizable cities are more expensive on average, with a few
                         outliers. This is especially true in the top 50 cities, where
                         there is a sudden rise in average rental price.")))),
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
        ),
        selected = "Seattle, WA"
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
        ),
        selected = "San Francisco, CA"
      )
    ),
    mainPanel(
      plotOutput("histogram"),
      p("We created a histogram that allowed people to compare rental prices of 
        two cities on the West Coast in 2019. Because many students at the 
        University of Washington would like to stay on the West Coast, we 
        decided to filter out cities in the Midwest and East because those 
        regions are not as relevant to students here. From this histogram, 
        people can see if size and the notability of the city name impacts the 
        rental prices. We will be able to better assess where on the West Coast 
        would be the least costly.")
    )
  ) #closes tab panel
))




