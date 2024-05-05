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


#plot
p <- ggplot(state_map, aes(long, lat, group = group)) +
     geom_polygon(aes(fill = discuss),
               colour = alpha("white", 1/2), size = 0.05)  +
     geom_polygon(data = state_df, colour = "white", fill = NA) +
     ggtitle("2023 US opinion on Global Warming") +
     scale_fill_gradientn(colours=c(blue,"white", red))  +
     theme_void()

fig <- ggplotly(p)

fig


