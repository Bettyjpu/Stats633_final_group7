# Import religion and politics data
library(dplyr)
library(ggplot2)
library(plotly)
library(maps)

# Import religion data
# religion <- read.csv("religion by state.csv")

# Import senate party data
senate_party <- read_csv("blue-states-2024.csv", show_col_types = FALSE)%>%
  select(state, BlueStatesSenateParty2023)%>%
  rename(NAME = state)%>%
  rename(Party = BlueStatesSenateParty2023)

# merge senate party data with religion data
data_merged_party <- merge(data_merged, senate_party, by = "NAME")

# Shift data shape
data_merged_party_shifted <- data_merged_party %>%
  shift_geometry(position = "outside")

# Visualize by political party
data_merged_party_shifted <- data_merged_party_shifted %>%
  mutate(color = ifelse(Party == "Democratic", "blue",
                        ifelse(Party == "Republican", "red", "purple")))

# Prepare the dataset for plotting by creating hover info
party_plot <- data_merged_party_shifted %>%
  mutate(tooltip = paste(NAME, ":",
                         "\n", discuss, "% often discuss climate change",
                         "\n", citizens, "% thinking that citizens should do more",
                         "\n", congress, "% thinking that the congress should do more"))


# Assuming you have an sf object named 'map_sf' containing the spatial data

map_party <- ggplot(party_plot, aes(fill = color)) + 
  geom_sf_interactive(aes(tooltip = tooltip, data_id = NAME), size = 0.1) + 
  scale_fill_identity() + 
  labs(title = "Party Affiliation by State",
       caption = "Data source: Your source",
       fill = "Party") + 
  theme_void()

girafe(ggobj = map_party) %>%
  girafe_options(opts_hover(css = "fill:cyan;"), 
                 opts_zoom(max = 10))
