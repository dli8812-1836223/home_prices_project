library(dplyr)
library(plotly)
library(stringr)
library(tidyverse)
library(ggmap)
library(knitr)
library(leaflet)
library(shiny)

rents_df <- read.csv("Metro_MedianRentalPrice_2Bedroom.csv", stringsAsFactors = FALSE)

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

#Making UI
ui <- shinyUI(navbarPage("Rental Prices",
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
      mainPanel(plotlyOutput("scatter"))))))
