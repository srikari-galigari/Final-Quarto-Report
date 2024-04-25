install.packages("tidyverse")
install.packages("plotly")
install.packages("dplyr")
install.packages("countrycode")
install.packages("scales")
install.packages("readr")
install.packages("maps")

library(tidyverse)
library(plotly)
library(dplyr)
library(countrycode)
library(scales)
library(readr)
library(maps)

'memory.limit(size = NA)'

# Read datasets
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")
map_world <- map_data("world")

unicef_indicator_2$year <- as.integer(unicef_indicator_2$year)
unicef_metadata$year <- as.integer(unicef_metadata$year)

data_join <- full_join(unicef_indicator_2, unicef_metadata, by = c("country", "alpha_2_code", "alpha_3_code", "numeric_code", "year"))

print(data_join)

data_join <- filter(data_join, year == 2015)

# Join the data with the map
map_data_join <- full_join(data_join, map_world, by = c("country" = "region"))

# Check for the presence of the required variables
head(map_data_join)

library(ggplot2)

ggplot(map_data_join) + aes(x = long, y = lat, group = group, fill = `Life expectancy at birth, total (years)`) +
  geom_polygon() +
  scale_fill_gradientn(colors = rainbow(7, start = 0, end = 0.8)) +
  labs(title = "Indicator Visualization", x = "Longitude", y = "Latitude")


# Print the plot
print(world_map_plot)

#timeseries1
timeseries_data <- data_join %>%
  select(country = country, year, Population_total = `Population, total`) %>%
  drop_na()

timeseries_ggplot <- ggplot(timeseries_data, aes(x = year, y = Population_total, group = country, color = country)) +
  geom_line() +
  scale_y_continuous(labels = scales::label_number(big.mark = ",", decimal.mark = ".", accuracy = 1)) +
  theme_minimal() +
  theme(legend.position = "none")

timeseries_plotly <- ggplotly(timeseries_ggplot)
print(timeseries_plotly)

library(readr)

data_join$'GNI (current US$)' <- as.numeric(as.character(data_join$'GNI (current US$)'))
data_join$'Life expectancy at birth, total (years)' <- as.numeric(as.character(data_join$'Life expectancy at birth, total (years)'))

data_join <- na.omit(data_join[, c('GNI (current US$)', 'Life expectancy at birth, total (years)')])


# create a Scatter Plot 
Scatter_plot <- ggplot(data_join, aes(x = `GNI (current US$)`, y = `Life expectancy at birth, total (years)`, text = country, group = country, color = country)) +
  geom_point() +
  labs(title = "Scatter Plot of GNI vs. Life Expectancy",
       x = "GNI (current US$)",
       y = "Life Expectancy at Birth (years)") +
  theme_minimal() +
theme(legend.position = "none") 

Scatter_plotly <- ggplotly(Scatter_plot)
print(Scatter_plotly)

#bar chart 1 - Countries showing their Military Expenditure
library(ggplot2)
library(dplyr)

# Print the names of the columns in the data_join dataframe
colnames(data_join)

data_summary <- data_join %>%
  filter(!is.na(`Military expenditure (% of GDP)`)) %>%
  group_by(`country`, year) %>%
  summarize(Military_expenditure = mean(`Military expenditure (% of GDP)`, na.rm = TRUE), .groups = "drop")

# Create the bar chart with the correct syntax
ggplot(data_summary, aes(x = reorder(`country`, Military_expenditure), y = Military_expenditure, fill = `country`)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")  # Rotate x-axis labels for readability and remove legend

#bar chat 3 showcasing 
library(dplyr)

# Calculate average military expenditure, grouped by country and year
avg_military_expenditure <- data_join %>%
  filter(!is.na(`Military expenditure (% of GDP)`)) %>%
  group_by(country, year) %>%
  summarize(avg_military_expenditure = mean(`Military expenditure (% of GDP)`, na.rm = TRUE), .groups = "drop")

# Determine the top 5 countries with the highest average military expenditure
top_5_countries <- avg_military_expenditure %>%
  group_by(country) %>%
  summarize(avg_military_expenditure = mean(avg_military_expenditure, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_military_expenditure)) %>%
  slice_max(avg_military_expenditure, n = 5)

# Filter the data for only the top 5 countries
top_5_data <- avg_military_expenditure %>%
  filter(country %in% top_5_countries$country)

# Plot the data
library(ggplot2)

ggplot(top_5_data, aes(x = year, y = avg_military_expenditure, fill = factor(country))) +
  geom_bar(stat = "identity") +
  labs(
    x = "Year",
    y = "Average Military Expenditure (% of GDP)",
    title = "Evolution of Average Military Expenditure per Top 5 Countries (1960-2022)"
  ) +
  scale_fill_manual(values = rainbow(length(unique(top_5_data$country))))  # Set colors manually
