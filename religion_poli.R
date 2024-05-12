# Import religion and politics data
library(dplyr)
library(ggplot2)
library(plotly)
library(maps)

# Import religion data
# religion <- read.csv("religion by state.csv")

# Import senate party data
senate_party <- read_csv("blue-states-2024.csv", show_col_types = FALSE)%>%
  select(state, BlueStatesSenateParty2023)

# merge senate party data with religion data