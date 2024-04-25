install.packages("tidyverse")
install.packages("plotly")
install.packages("dplyr")
install.packages("countrycode")
install.packages("scales")
install.packages("readr")
install.packages("ggplot2")
install.packages("viridis")

library(tidyverse)
library(plotly)
library(dplyr)
library(countrycode)
library(scales)
library(readr)
library(ggplot2)
library(viridis)

library(tidyverse)
library(ggplot2)
library(readr)
library(maps)
library(viridis)

# Read datasets
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")
map_world <- map_data("world")

# Convert years to integers
unicef_indicator_2$year <- as.integer(unicef_indicator_2$year)
unicef_metadata$year <- as.integer(unicef_metadata$year)

# Merge the datasets
data_join <- full_join(unicef_indicator_2, unicef_metadata, by = c("country", "alpha_2_code", "alpha_3_code", "numeric_code", "year"))

# Filter for the year 2015, which has life expectancy data
data_2015 <- data_join %>% filter(year == 2015)

# Check that 'Life expectancy at birth, total (years)' is numeric
data_2015$`Life expectancy at birth, total (years)` <- as.numeric(data_2015$`Life expectancy at birth, total (years)`)

# Join the data with the map
map_data_join <- full_join(data_2015, map_world, by = c("country" = "region"))

world_map_plotly_corrected <- plot_ly(map_data_join, 
                                      type = "choropleth", 
                                      locations = ~alpha_3_code, 
                                      locationmode = "ISO-3", 
                                      z = ~`Life expectancy at birth, total (years)`,
                                      colors = inferno(20), # You can use other viridis palettes such as inferno, plasma, magma, etc.
                                      hoverinfo = "text",
                                      text = ~paste("Country: ", country, "<br>Life Expectancy: ", `Life expectancy at birth, total (years)`, " years"),
                                      colorbar = list(title = "Life Expectancy at Birth (years)")) %>%
  layout(title = "Life Expectancy at Birth in 2015",
         geo = list(showframe = FALSE, 
                    projection = list(type = 'mercator')))

world_map_plotly_corrected



