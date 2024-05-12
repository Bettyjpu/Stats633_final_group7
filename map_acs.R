# Map demographics

# Load packages
library(tidyverse)
library(tidycensus)
library(ggiraph)
library(scales)
library(ggplot2)
library(plotly)

# Map
library(ggiraph)
library(scales)
library(tigris)
options(tigris_use_cache = TRUE)

# Get data from ACS

# Searching for Variables from ACS
##Getting variables from the Census or ACS requires knowing the variable ID 
## - and there are thousands of these IDs across the different Census files.
## Use the `load_variables` function

var_22 <- load_variables(2022, "acs1", cache = TRUE)
View(var_22)


## https://walker-data.com/tidycensus/reference/census_api_key.html

# population data
pop_22 <- get_acs(
  geography = "state",
  variables = "B01003_001", 
  year = 2022,
  survey = "acs1",
  geometry = FALSE,
  resolution = "20m"
) %>% rename(population = estimate) %>%
  select(NAME, population)


# medium income in the state
income_22 <- get_acs(
  geography = "state",
  variables = "B25077_001", 
  year = 2022,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
)

income_22_shifted <- income_22 %>%
  shift_geometry(position = "outside") %>%
  mutate(tooltip = paste(NAME, estimate, sep = ": "))


# people with a bachelor's degree
edu_22 <- get_acs(
  geography = "state",
  variables = "B07009_005", 
  year = 2022,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
)

edu_22_merge <- merge(edu_22, pop_22, by = "NAME")
edu_22_merge$percentage <- round((edu_22_merge$estimate / edu_22_merge$population)*100, 1)

edu_22_shifted <- edu_22_merge %>%
  shift_geometry(position = "outside") %>%
  mutate(tooltip = paste(NAME, ":", percentage, "% of population has a bachelor's degeree"))


# Foreign born population
foreign_22 <- get_acs(
  geography = "state",
  variables = "B05002_013",
  year = 2022,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
)

foreign_22_merge <- merge(foreign_22, pop_22, by = "NAME")
foreign_22_merge$percentage <- round((foreign_22_merge$estimate / foreign_22_merge$population)*100, 1)

foreign_22_shifted <- foreign_22_merge %>%
  shift_geometry(position = "outside") %>%
  mutate(tooltip = paste(NAME, ":", percentage, "% population are foreign born"))


# At or above 150% poverty level
abovepoverty_22 <- get_acs(
  geography = "state",
  variables = "B07012_004",
  year = 2022,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
)

abovepoverty_22_merge <- merge(abovepoverty_22, pop_22, by = "NAME")
abovepoverty_22_merge$percentage <- round((abovepoverty_22_merge$estimate / abovepoverty_22_merge$population)*100, 1)

abovepoverty_22_shifted <- abovepoverty_22_merge %>%
  shift_geometry(position = "outside") %>%
  mutate(tooltip = paste(NAME, ":", percentage, "% population at or above 150% poverty level"))


# plot medium income
map_income <- ggplot(income_22_shifted, aes(fill = estimate)) + 
  geom_sf_interactive(aes(tooltip = tooltip, data_id = NAME), 
                      size = 0.1) + 
  scale_fill_viridis_c(option = "plasma", labels = label_dollar()) + 
  labs(title = "Median housing value by State, 2022",
       caption = "Data source: 2022 1-year ACS, US Census Bureau",
       fill = "ACS estimate") + 
  theme_void() 

girafe(ggobj = map_income) %>%
  girafe_options(opts_hover(css = "fill:cyan;"), 
                 opts_zoom(max = 10))


# plot education level
map_edu <- ggplot(edu_22_shifted, aes(fill = percentage)) + 
  geom_sf_interactive(aes(tooltip = tooltip, data_id = NAME), 
                      size = 0.1) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Percentage of Population with Bachelor's Degree by State, 2022",
       caption = "Data source: 2022 1-year ACS, US Census Bureau",
       fill = "ACS estimate") + 
  theme_void() 

girafe(ggobj = map_edu) %>%
  girafe_options(opts_hover(css = "fill:cyan;"), 
                 opts_zoom(max = 10))


# plot foreign born popualtion
map_foreign <- ggplot(foreign_22_shifted, aes(fill = percentage)) + 
  geom_sf_interactive(aes(tooltip = tooltip, data_id = NAME), 
                      size = 0.1) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Percentage of Population that are Foreign Born, 2022",
       caption = "Data source: 2022 1-year ACS, US Census Bureau",
       fill = "ACS estimate") + 
  theme_void() 

girafe(ggobj = map_foreign) %>%
  girafe_options(opts_hover(css = "fill:cyan;"), 
                 opts_zoom(max = 10))

# plot above poverty level
map_abovepoverty <- ggplot(abovepoverty_22_shifted, aes(fill = percentage)) + 
  geom_sf_interactive(aes(tooltip = tooltip, data_id = NAME), 
                      size = 0.1) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Percentage of Population that are above poverty line, 2022",
       caption = "Data source: 2022 1-year ACS, US Census Bureau",
       fill = "ACS estimate") + 
  theme_void() 

girafe(ggobj = map_abovepoverty) %>%
  girafe_options(opts_hover(css = "fill:cyan;"), 
                 opts_zoom(max = 10))




