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
  select(NAME, citizens, congress, consensus, discuss, exp, happening, human, worried)

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
  mutate(tooltip_citizens = paste(NAME, ":", citizens, "% thinking that citizens should do more")) %>%
  mutate(tooltip_congress = paste(NAME, ":", congress, "% thinking that the congress should do more")) %>%
  mutate(tooltip_consensus = paste(NAME, ":", consensus, "% who believe that global warming is happening")) %>%
  mutate(tooltip_exp = paste(NAME, ":", exp, "% personally experienced global warming")) %>%
  mutate(tooltip_happening = paste(NAME, ":", happening, "% thinking global warming is happening")) %>%
  mutate(tooltip_human = paste(NAME, ":", human, "% agree that global warming is caused by human activities")) %>%
  mutate(tooltip_worried = paste(NAME, ":", worried, "% worried about global warming"))


#Mapping

# Discuss: Estimated percentage who discuss global warming occassionally or often with friends and family
map_discuss <- ggplot(data_merged_shifted, aes(fill = discuss)) + 
  geom_sf_interactive(aes(tooltip = tooltip_discuss, data_id = NAME), 
                      size = 0.1) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Percentage of population that often discuss climate change, 2023",
       caption = "Data source: 2023 Yale Program on Climate Change Communication",
       fill = "% state population") + 
  theme_void() 

girafe(ggobj = map_discuss) %>%
  girafe_options(opts_hover(css = "fill:cyan;"), 
                 opts_zoom(max = 10))

# Citizens: Estimated percentage who think citizens themselves should be doing more/much more to address global warming
map_citizens <- ggplot(data_merged_shifted, aes(fill = citizens)) + 
  geom_sf_interactive(aes(tooltip = tooltip_citizens, data_id = NAME), 
                      size = 0.1) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Percentage of population thinking that citizens should do more, 2023",
       caption = "Data source: 2023 Yale Program on Climate Change Communication",
       fill = "% state population") + 
  theme_void() 

girafe(ggobj = map_citizens) %>%
  girafe_options(opts_hover(css = "fill:cyan;"), 
                 opts_zoom(max = 10))


# Congress: Estimated percentage who think Congress should be doing more/much more to address global warming
map_congress <- ggplot(data_merged_shifted, aes(fill = congress)) + 
  geom_sf_interactive(aes(tooltip = tooltip_congress, data_id = NAME), 
                      size = 0.1) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Percentage of population thinking that the congress should do more, 2023",
       caption = "Data source: 2023 Yale Program on Climate Change Communication",
       fill = "% state population") + 
  theme_void() 

girafe(ggobj = map_congress) %>%
  girafe_options(opts_hover(css = "fill:cyan;"), 
                 opts_zoom(max = 10))

# Consensus: Estimated percentage who believe that most scientists think global warming is happening
map_consensus <- ggplot(data_merged_shifted, aes(fill = consensus)) + 
  geom_sf_interactive(aes(tooltip = tooltip_consensus, data_id = NAME), 
                      size = 0.1) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Percentage of population who believe that global warming is happening, 2023",
       caption = "Data source: 2023 Yale Program on Climate Change Communication",
       fill = "% state population") + 
  theme_void() 

girafe(ggobj = map_consensus) %>%
  girafe_options(opts_hover(css = "fill:cyan;"), 
                 opts_zoom(max = 10))


# Exp: Estimated percentage who somewhat/strongly agree that they have personally experienced the effects of global warming
map_exp <- ggplot(data_merged_shifted, aes(fill = exp)) + 
  geom_sf_interactive(aes(tooltip = tooltip_exp, data_id = NAME), 
                      size = 0.1) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Percentage of population who believe that personally experienced global warming, 2023",
       caption = "Data source: 2023 Yale Program on Climate Change Communication",
       fill = "% state population") + 
  theme_void() 

girafe(ggobj = map_exp) %>%
  girafe_options(opts_hover(css = "fill:cyan;"), 
                 opts_zoom(max = 10))

# Happening: Estimated percentage who think that global warming is happening
map_happening <- ggplot(data_merged_shifted, aes(fill = happening)) + 
  geom_sf_interactive(aes(tooltip = tooltip_happening, data_id = NAME), 
                      size = 0.1) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Percentage of population who think that global warming is happening, 2023",
       caption = "Data source: 2023 Yale Program on Climate Change Communication",
       fill = "% state population") + 
  theme_void() 

girafe(ggobj = map_happening) %>%
  girafe_options(opts_hover(css = "fill:cyan;"), 
                 opts_zoom(max = 10))

# Human: Estimated percentage who think that global warming is caused mostly by human activities
map_human <- ggplot(data_merged_shifted, aes(fill = human)) + 
  geom_sf_interactive(aes(tooltip = tooltip_human, data_id = NAME), 
                      size = 0.1) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Percentage who think that global warming is caused mostly by human activities, 2023",
       caption = "Data source: 2023 Yale Program on Climate Change Communication",
       fill = "% state population") + 
  theme_void() 

girafe(ggobj = map_human) %>%
  girafe_options(opts_hover(css = "fill:cyan;"), 
                 opts_zoom(max = 10))

# Worried: Estimated percentage who are somewhat/very worried about global warming
map_worried <- ggplot(data_merged_shifted, aes(fill = worried)) + 
  geom_sf_interactive(aes(tooltip = tooltip_worried, data_id = NAME), 
                      size = 0.1) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Percentage who are somewhat/very worried about global warming, 2023",
       caption = "Data source: 2023 Yale Program on Climate Change Communication",
       fill = "% state population") + 
  theme_void() 

girafe(ggobj = map_worried) %>%
  girafe_options(opts_hover(css = "fill:cyan;"), 
                 opts_zoom(max = 10))

