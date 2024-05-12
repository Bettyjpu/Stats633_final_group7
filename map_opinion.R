# Data wrangling for YCOM_2023

# Load packages
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(maps)

# Read Data
data_23 <- read_excel("YCOM_2023.xlsx")

# Seperate State-level data with county-level data

## State data
state_23 <- subset(data_23, geotype == "state") %>%
  rename (NAME = geoname)

## Select variables from state_23 that will be utilized for mapping
state_23 <- state_23 %>%
  select(citizens, congress, consensus, discuss, exp, happening, human, worried)

# Get ACS data for plotting
# medium income in the state
acs_data <- get_acs(
  geography = "state",
  variables = "B25077_001", 
  year = 2022,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
)

data_merged <- merge(acs_data, state_23, by = "NAME")
data_merged_shifted <- data_merged %>%
  shift_geometry(position = "outside") %>%
  mutate(tooltip_discuss = paste(NAME, ":", discuss, "% population that often discuss climate change")) %>%
  mutate(tooltip_ = paste(NAME, ":", citizens, "% population that discuss climate change"))


#Map
map_discuss <- ggplot(data_merged_shifted, aes(fill = discuss)) + 
  geom_sf_interactive(aes(tooltip = tooltip_discuss, data_id = NAME), 
                      size = 0.1) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Percentage of population that discuss climate change, 2023",
       caption = "Data source: 2023 Yale Program on Climate Change Communication",
       fill = "ACS estimate") + 
  theme_void() 

girafe(ggobj = map_discuss) %>%
  girafe_options(opts_hover(css = "fill:cyan;"), 
                 opts_zoom(max = 10))









