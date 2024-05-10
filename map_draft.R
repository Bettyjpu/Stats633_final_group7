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



# Plotly

fig <- plot_ly() %>%
  add_trace(
    data = state_map,
    locations = ~group,
    type = "choropleth",
    lon = ~long,
    lat = ~lat,
    text = ~paste("State: ", group, "<br>Opinion on Global Warming: ", discuss),
    mode = "markers",
    colorscale = "Viridis",
    color = ~discuss,
    cmin = min(state_map$discuss),
    cmax = max(state_map$discuss)
  ) %>%
  add_trace(
    type = "scattergeo",
    lon = ~long,
    lat = ~lat,
    text = ~region,
    mode = "none",
    hoverinfo = "text"
  ) %>%
  layout(
    title = "2023 US opinion on Global Warming",
    geo = list(
      scope = "usa",
      showland = TRUE,
      showcountries = FALSE,
      showsubunits = TRUE,
      subunitcolor = "white",
      subunitwidth = 1
    )
  )

# Show the plot
fig



# Create plotly object directly
fig <- plot_ly() %>%
  add_trace(
    data = state_map,
    type = "scattergeo",
    # lon = ~long,
    # lat = ~lat,
    text = ~paste("State: ", group, "<br>Opinion on Global Warming: ", discuss),
    mode = "markers",
    marker = list(
      color = ~discuss,
      colorscale = "Viridis",
      cmin = min(state_map$discuss),
      cmax = max(state_map$discuss)
    )
  ) %>%
  add_trace(
    type = "scattergeo",
    lon = ~long,
    lat = ~lat,
    text = ~region,
    mode = "none",
    hoverinfo = "text"
  ) %>%
  layout(
    title = "2023 US opinion on Global Warming",
    geo = list(
      scope = "usa",
      showland = TRUE,
      showcountries = FALSE,
      showsubunits = TRUE,
      subunitcolor = "white",
      subunitwidth = 1
    )
  )


