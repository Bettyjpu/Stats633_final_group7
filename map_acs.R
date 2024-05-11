# Map demographics

# Load packages
library(tidyverse)
library(tidycensus)
library(ggiraph)
library(scales)
library(ggplot2)
library(plotly)
# library(maps)

# Get data from ACS
us_value <- get_acs(
  geography = "state",
  variables = "B25077_001",
  year = 2022,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
)


library(ggiraph)
library(scales)
library(tigris)
options(tigris_use_cache = TRUE)

us_value_shifted <- us_value %>%
  shift_geometry(position = "outside") %>%
  mutate(tooltip = paste(NAME, estimate, sep = ": "))

gg <- ggplot(us_value_shifted, aes(fill = estimate)) + 
  geom_sf_interactive(aes(tooltip = tooltip, data_id = NAME), 
                      size = 0.1) + 
  scale_fill_viridis_c(option = "plasma", labels = label_dollar()) + 
  labs(title = "Median housing value by State, 2019",
       caption = "Data source: 2022 1-year ACS, US Census Bureau",
       fill = "ACS estimate") + 
  theme_void() 

girafe(ggobj = gg) %>%
  girafe_options(opts_hover(css = "fill:cyan;"), 
                 opts_zoom(max = 10))
