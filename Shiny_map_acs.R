# Load packages
library(shiny)
library(tidyverse)
library(tidycensus)
library(ggiraph)
library(scales)
library(ggplot2)
library(plotly)
library(tigris)
options(tigris_use_cache = TRUE)

# Get data from ACS
var_22 <- load_variables(2022, "acs1", cache = TRUE)

# Population data
pop_22 <- get_acs(
  geography = "state",
  variables = "B01003_001",
  year = 2022,
  survey = "acs1",
  geometry = FALSE,
  resolution = "20m"
) %>% rename(population = estimate) %>%
  select(NAME, population)

# Median income in the state
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
  mutate(fill_value = estimate) %>%
  mutate(tooltip = paste(NAME, fill_value, sep = ": "))

# People with a bachelor's degree
edu_22 <- get_acs(
  geography = "state",
  variables = "B07009_005",
  year = 2022,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
)

edu_22_merge <- merge(edu_22, pop_22, by = "NAME")
edu_22_merge$percentage <- round((edu_22_merge$estimate / edu_22_merge$population) * 100, 1)

edu_22_shifted <- edu_22_merge %>%
  shift_geometry(position = "outside") %>%
  mutate(fill_value = percentage) %>%
  mutate(tooltip = paste(NAME, ":", fill_value, "% of population has a bachelor's degree"))

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
foreign_22_merge$percentage <- round((foreign_22_merge$estimate / foreign_22_merge$population) * 100, 1)

foreign_22_shifted <- foreign_22_merge %>%
  shift_geometry(position = "outside") %>%
  mutate(fill_value = percentage) %>%
  mutate(tooltip = paste(NAME, ":", fill_value, "% population are foreign born"))

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
abovepoverty_22_merge$percentage <- round((abovepoverty_22_merge$estimate / abovepoverty_22_merge$population) * 100, 1)

abovepoverty_22_shifted <- abovepoverty_22_merge %>%
  shift_geometry(position = "outside") %>%
  mutate(fill_value = percentage) %>%
  mutate(tooltip = paste(NAME, ":", fill_value, "% population at or above 150% poverty level"))

# UI layout
ui <- fluidPage(
  titlePanel("State-Level Demographic Analysis (2022)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select Demographic Variable:", 
                  choices = list("Median Housing Value" = "income",
                                 "Education Level" = "edu",
                                 "Foreign Born Population" = "foreign",
                                 "Above Poverty Level" = "abovepoverty"))
    ),
    mainPanel(
      girafeOutput("map")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  reactive_data <- reactive({
    switch(input$variable,
           "income" = income_22_shifted,
           "edu" = edu_22_shifted,
           "foreign" = foreign_22_shifted,
           "abovepoverty" = abovepoverty_22_shifted)
  })
  
  reactive_title <- reactive({
    switch(input$variable,
           "income" = "Median Housing Value by State, 2022",
           "edu" = "Percentage of Population with Bachelor's Degree by State, 2022",
           "foreign" = "Percentage of Population that are Foreign Born, 2022",
           "abovepoverty" = "Percentage of Population that are Above Poverty Level, 2022")
  })
  
  reactive_fill <- reactive({
    if (input$variable == "income") {
      scale_fill_viridis_c(option = "plasma", labels = label_dollar())
    } else {
      scale_fill_viridis_c(option = "plasma")
    }
  })
  
  output$map <- renderGirafe({
    data <- reactive_data()
    print(unique(data$fill_value)) # Print unique values for debugging
    map <- ggplot(data, aes(fill = fill_value)) +
      geom_sf_interactive(aes(tooltip = tooltip, data_id = NAME), size = 0.1) +
      reactive_fill() +
      labs(title = reactive_title(),
           caption = "Data source: 2022 1-year ACS, US Census Bureau",
           fill = "ACS estimate") +
      theme_void()
    
    girafe(ggobj = map) %>%
      girafe_options(opts_hover(css = "fill:cyan;"), 
                     opts_zoom(max = 10))
  })
}

shinyApp(ui = ui, server = server)
