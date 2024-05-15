# Load packages
library(shiny)
library(tidyverse)
library(tidycensus)
library(ggiraph)
library(scales)
library(ggplot2)
library(plotly)
library(tigris)
library(readxl)
options(tigris_use_cache = TRUE)

# ACS
var_22 <- load_variables(2022, "acs1", cache = TRUE)

# population data
pop_22 <- get_acs(
  geography = "state",
  variables = "B01003_001",
  year = 2022,
  survey = "acs1",
  geometry = FALSE,
  resolution = "20m"
) |> rename(population = estimate) |>
  select(NAME, population)

# median income in the state
income_22 <- get_acs(
  geography = "state",
  variables = "B25077_001",
  year = 2022,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
)

income_22_shifted <- income_22 |>
  shift_geometry(position = "outside") |>
  mutate(fill_value = estimate) |>
  mutate(tooltip = paste(NAME, fill_value, sep = ": "))

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
edu_22_merge$percentage <- round((edu_22_merge$estimate / edu_22_merge$population) * 100, 1)

edu_22_shifted <- edu_22_merge |>
  shift_geometry(position = "outside") |>
  mutate(fill_value = percentage) |>
  mutate(tooltip = paste(NAME, ":", fill_value, "% of population has a bachelor's degree"))

# foreign born population
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

foreign_22_shifted <- foreign_22_merge |>
  shift_geometry(position = "outside") |>
  mutate(fill_value = percentage) |>
  mutate(tooltip = paste(NAME, ":", fill_value, "% population are foreign born"))

# at or above 150% poverty level
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

abovepoverty_22_shifted <- abovepoverty_22_merge |>
  shift_geometry(position = "outside") |>
  mutate(fill_value = percentage) |>
  mutate(tooltip = paste(NAME, ":", fill_value, "% population at or above 150% poverty level"))

data_23 <- read_excel("YCOM_2023.xlsx")

# separate State-level data from county-level data
state_23 <- subset(data_23, geotype == "state") |>
  rename(NAME = geoname) |>
  select(NAME, citizens, congress, consensus, discuss, exp, happening, human, worried)

# add a row for Puerto Rico with all climate data set to null
state_23 <- state_23 %>%
  add_row(NAME = "Puerto Rico", citizens = NA, congress = NA, consensus = NA, discuss = NA, exp = NA, happening = NA, human = NA, worried = NA)


# merge ACS data with climate change opinion data
data_merged <- merge(income_22, state_23, by = "NAME")

data_merged_shifted <- data_merged |>
  shift_geometry(position = "outside") |>
  mutate(tooltip_discuss = paste(NAME, ":", discuss, "% population that often discuss climate change")) |>
  mutate(tooltip_citizens = paste(NAME, ":", citizens, "% thinking that citizens should do more")) |>
  mutate(tooltip_congress = paste(NAME, ":", congress, "% thinking that the congress should do more")) |>
  mutate(tooltip_consensus = paste(NAME, ":", consensus, "% who believe that global warming is happening")) |>
  mutate(tooltip_exp = paste(NAME, ":", exp, "% personally experienced global warming")) |>
  mutate(tooltip_happening = paste(NAME, ":", happening, "% thinking global warming is happening")) |>
  mutate(tooltip_human = paste(NAME, ":", human, "% agree that global warming is caused by human activities")) |>
  mutate(tooltip_worried = paste(NAME, ":", worried, "% worried about global warming"))

# UI
ui <- fluidPage(
  titlePanel("State-Level Analysis of Demographics (2022) and Climate Change Opinions (2023)"),
  fluidRow(
    column(6,
           selectInput("variable1", "Select Climate Change Opinion Variable:",
                       choices = list(
                         "Discuss Climate Change" = "discuss",
                         "Citizens Should Do More" = "citizens",
                         "Congress Should Do More" = "congress",
                         "Global Warming Consensus" = "consensus",
                         "Experienced Global Warming" = "exp",
                         "Global Warming Happening" = "happening",
                         "Human-Caused Warming" = "human",
                         "Worried About Warming" = "worried"
                       )),
           girafeOutput("map1", height = "600px")
    ),
    column(6,
           selectInput("variable2", "Select Demographic Variable:", 
                       choices = list("Median Housing Value" = "income",
                                      "Education Level" = "edu",
                                      "Foreign Born Population" = "foreign",
                                      "Above Poverty Level" = "abovepoverty")),
           girafeOutput("map2", height = "600px")
    )
  )
)

# Server
server <- function(input, output) {
  
  reactive_data1 <- reactive({
    data_merged_shifted
  })
  
  reactive_title1 <- reactive({
    switch(input$variable1,
           "discuss" = "Percentage of population that often discuss climate change, 2023",
           "citizens" = "Percentage of population thinking that citizens should do more, 2023",
           "congress" = "Percentage of population thinking that the congress should do more, 2023",
           "consensus" = "Percentage of population who believe that global warming is happening, 2023",
           "exp" = "Percentage of population who believe that personally experienced global warming, 2023",
           "happening" = "Percentage of population who think that global warming is happening, 2023",
           "human" = "Percentage who think that global warming is caused mostly by human activities, 2023",
           "worried" = "Percentage who are somewhat/very worried about global warming, 2023")
  })
  
  output$map1 <- renderGirafe({
    data1 <- reactive_data1()
    var1 <- input$variable1
    map1 <- ggplot(data1, aes_string(fill = var1)) +
      geom_sf_interactive(aes_string(tooltip = paste0("tooltip_", var1), data_id = "NAME"), size = 0.1) +
      scale_fill_viridis_c(option = "plasma") +
      labs(title = reactive_title1(),
           caption = "Data source: 2023 Yale Program on Climate Change Communication",
           fill = "% state population") +
      theme_void() +
      theme(plot.title = element_text(size = 10))
    
    girafe(ggobj = map1) |>
      girafe_options(opts_hover(css = "fill:cyan;"), 
                     opts_zoom(max = 10))
  })
  
  reactive_data2 <- reactive({
    switch(input$variable2,
           "income" = income_22_shifted,
           "edu" = edu_22_shifted,
           "foreign" = foreign_22_shifted,
           "abovepoverty" = abovepoverty_22_shifted)
  })
  
  reactive_title2 <- reactive({
    switch(input$variable2,
           "income" = "Median Housing Value by State, 2022",
           "edu" = "Percentage of Population with Bachelor's Degree by State, 2022",
           "foreign" = "Percentage of Population that are Foreign Born, 2022",
           "abovepoverty" = "Percentage of Population that are Above Poverty Level, 2022")
  })
  
  reactive_fill2 <- reactive({
    if (input$variable2 == "income") {
      scale_fill_viridis_c(option = "plasma", labels = label_dollar())
    } else {
      scale_fill_viridis_c(option = "plasma")
    }
  })
  
  output$map2 <- renderGirafe({
    data2 <- reactive_data2()
    map2 <- ggplot(data2, aes(fill = fill_value)) +
      geom_sf_interactive(aes(tooltip = tooltip, data_id = NAME), size = 0.1) +
      reactive_fill2() +
      labs(title = reactive_title2(),
           caption = "Data source: 2022 1-year ACS, US Census Bureau",
           fill = "ACS estimate") +
      theme_void() +
      theme(plot.title = element_text(size = 10))
    
    girafe(ggobj = map2) |>
      girafe_options(opts_hover(css = "fill:cyan;"), 
                     opts_zoom(max = 10))
  })
}

shinyApp(ui = ui, server = server)
