library(dplyr)
library(ggplot2)

home_prices <- read.csv("Metro_MedianRentalPrice_2Bedroom.csv", stringsAsFactors = FALSE)

home <-  as.data.frame(home_prices[,c(1:2,75:111)], drop = FALSE)

top_20 <- as.data.frame(home[2:20,], drop = FALSE)

three <- home %>% select(RegionName, X2016.01, X2017.01, X2018.01, X2019.01)

histogram <- function(data,
                      city_1,
                      city_2,
                      xvar = "RegionName",
                      yvar = "X2019.01") {
  data <- data %>%
    filter( (RegionName == city_1) | (RegionName == city_2))
  max_y <- mean(data$X2019.01, na.rm = TRUE)
  bars <- ggplot(data = data, aes(x = RegionName,
                                  y = X2019.01,
                                  fill = RegionName)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(name = "City", values=c("red1","blue1")) +
    labs(
      y = "Rental Prices",
      x = "Cities"
    )
  bars
}
histogram(three, "New York, NY", "Chicago, IL")