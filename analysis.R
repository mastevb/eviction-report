# Analysis script: compute values and create graphics of interest
# install.packages("ggplot2")
# install.packages("ggmap")
# install.packages("tidyr")
# install.packages("lubridate")
library("dplyr")
library("ggplot2")
library("lubridate")
library("tidyr")
library("ggmap")

# Load in your data
evictions <- read.csv("data/Eviction_Notices.csv", stringsAsFactors = F)

# Compute some values of interest and store them in variables for the report

# How many evictions were there?
num_evictions <- nrow(evictions)

# Create a table (data frame) of evictions by zip code (sort descending)
by_zip <- evictions %>%
  group_by(Eviction.Notice.Source.Zipcode) %>%
  count(sort = T) %>%
  rename(zipcode = Eviction.Notice.Source.Zipcode, evictions = n) %>% 
  ungroup() %>% 
  top_n(10, wt = evictions)

# Create a plot of the number of evictions each month in the dataset
by_month <- evictions %>%
  group_by(
    month = floor_date(as.Date(File.Date, format = "%m/%d/%y"), unit = "month")
  ) %>%
  count()

# Store plot in a variable
by_month_plot <- ggplot(by_month) +
  geom_line(mapping = aes(x = month, y = n), size = .1, color = "blue") +
  labs(
    title = "Number of evictions each month in San Francisco", 
    y = "Number of evictions", 
    x = "Month"
  )

# Map evictions in 2017 

# Format the lat/long variables, filter to 2017
evictions_2017 <- evictions %>% 
  mutate(date = as.Date(File.Date, format="%m/%d/%y")) %>% 
  filter(format(date, "%Y") == "2017") %>%
  separate(Location, c("lat", "long"), ", ") %>% 
  mutate(
    lat = as.numeric(gsub("\\(", "", lat)), 
    long = as.numeric(gsub("\\)", "", long)) 
  ) 

# Create a maptile background
base_plot <- qmplot(
  data = evictions_2017,        
  x = long,                  
  y = lat,                      
  geom = "blank",              
  maptype = "toner-background", 
  darken = .7,                  
  legend = "topleft"            
)

# Add a layer of points on top of the map tiles
evictions_plot <- base_plot +
  geom_point(mapping = aes(x = long, y = lat), color = "red", alpha = .3) +
  labs(title = "Evictions in San Francisco, 2017") +
  theme(plot.margin = margin(.3, 0, 0, 0, "cm")) 