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
state_23 <- subset(data_23, geotype == "state")

## Congressional District
cong_23 <- subset(data_23, geotype == "cbsa")

## County data
county_23 <- subset(data_23, geotype == "county")

## County-level Choropleths

### Prepare map data
state_df <- map_data("state")

state_23$geoname <- tolower(state_23$geoname)

state_23 <- state_23 %>%
  rename(region = geoname)

### Join data
state_map <- inner_join(state_df, state_23, by = "region")
state_map <- state_map[!duplicated(state_map$order), ]

# colorramp
blue <- colorRampPalette(c("navy","royalblue","lightskyblue"))(200)                      
red <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)

state_map_unique <- state_map %>%
  group_by(region) %>%
  summarise(discuss = mean(discuss), long = first(long), lat = first(lat))

p <- ggplot(state_map, aes(long, lat, group = region)) +
     geom_polygon(aes(fill = discuss), size = 0.5)  +
     geom_polygon(data = state_df, colour = "white", fill = NA) +
     geom_text(data = state_map_unique, aes(label = paste("State: ", region, "Opinion on Global Warming: ", discuss)),
            size = 2, check_overlap = TRUE) + 
     ggtitle("2023 US opinion on Global Warming") +
     theme_void()

fig <- ggplotly(p)

fig


# Method 2: Plotly

# Get demographic data from ACS first

## Load `tidycensus` and `viridis`
library(tidycensus)
library(tidyverse)
library(viridis)
census_api_key("e576f7744caca50c2c06e77c300cb6152622426a", install = TRUE)
readRenviron("~/.Renviron")


us_value <- get_acs(
  geography = "state",
  variables = "B25077_001",
  year = 2023,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
)

# Plot the map with `ggiraph` and `scales`

library(ggiraph)
library(scales)

us_value_shifted <- us_value %>%
  shift_geometry(position = "outside") %>%
  mutate(tooltip = paste(NAME, estimate, sep = ": "))

gg <- ggplot(us_value_shifted, aes(fill = estimate)) + 
  geom_sf_interactive(aes(tooltip = tooltip, data_id = NAME), 
                      size = 0.1) + 
  scale_fill_viridis_c(option = "plasma", labels = label_dollar()) + 
  labs(title = "Median housing value by State, 2019",
       caption = "Data source: 2019 1-year ACS, US Census Bureau",
       fill = "ACS estimate") + 
  theme_void() 

girafe(ggobj = gg) %>%
  girafe_options(opts_hover(css = "fill:cyan;"), 
                 opts_zoom(max = 10))
















