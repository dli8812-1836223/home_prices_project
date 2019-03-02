# home_prices_project

By Daniel Li, Candice Lee, Nick Fang and Torin Dye 
=======
## Project Description

*Our project focuses on the rental prices for 2-bedrooms for metropolitan areas within the United States. Understanding the rental trends within the past year is extremely important to the economic vitality of any communities, as it attracts and retains workforces for big and small companies. Affordable homes and shorter commutes ensure huge opportunities for any individual and can grow a city’s community quickly, for it improves individual economic situations and lessens stress for all. We resourced our data from Zillow, which is the leading real estate marketplace that allows us to compare home values and search for listing in an online and convenient way. Within this website, we are able to retrieve data from their research database. Our target audience are college students and recent grads, for awareness about rental and home prices lack in these user groups. Some questions that we want to tackle are:*

- Where are the cheapest places to live?
- What is the trend of the rising rental prices within the past several years (2010-2019)?
- What is the trend on location compared to rental prices?

## Technical Description
_**How will you be reading in your data (i.e., are you using an API, or is it a static .csv/.json file)?**_  
  We have a static file which shows us rental prices within the past five years. We would read this as a .csv file.

_**What types of data-wrangling (reshaping, reformatting, etc.) will you need to do to your data?**_  
  There's a lot of empty cells in the data frame, so we will need to use dplyr to filter and select all the cells with actual numbers in them. We will probably need to mutate to create a new column to show the differences in rental prices to display how rental prices have changed within the past 5 years.

_**What (major/new) libraries will be using in this project (no need to list common libraries that are used in many projects such as dplyr)**_  
  We will be using shiny to put our report together. We will also need ggplot2 that will allow us to create scatter plots to show how rental prices have increased. Leaflet and plotly also may be useful because we will create maps that show how rental prices have changed in each different region and state.

_**What major challenges do you anticipate?**_  
  One major challenge will be to wrangle the data because there's many empty cells, and the process to create a different data frame that doesn't have any empty cells might be tedious. Using Shiny to create the user interface and push it to the server also may be complicated because we haven't really learned how to use Shiny, and what the package is able to do. We will also need to decide how to design our webpage and show our information in a way that makes sense to the general public. I anticipate that writing the reactive functions may be difficult.

