# Load packages
library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(tigris)
library(tidycensus)
library(ggiraph)
library(scales)

options(tigris_use_cache = TRUE)

# Read Data
data_23 <- read_excel("YCOM_2023.xlsx")

# Seperate State-level data from county-level data
state_23 <- subset(data_23, geotype == "state") %>%
  rename(NAME = geoname) %>%
  select(NAME, citizens, congress, consensus, discuss, exp, happening, human, worried)

# Get ACS data for plotting
# Median income in the state
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

# UI layout
ui <- fluidPage(
  titlePanel("State-Level Climate Change Opinions Analysis (2023)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select Variable to Map:",
                  choices = list(
                    "Discuss Climate Change" = "discuss",
                    "Citizens Should Do More" = "citizens",
                    "Congress Should Do More" = "congress",
                    "Global Warming Consensus" = "consensus",
                    "Personally Experienced Global Warming" = "exp",
                    "Global Warming is Happening" = "happening",
                    "Global Warming Caused by Humans" = "human",
                    "Worried About Global Warming" = "worried"
                  ))
    ),
    mainPanel(
      girafeOutput("map")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  reactive_map <- reactive({
    var <- input$variable
    
    fill_var <- data_merged_shifted[[var]]
    tooltip_var <- data_merged_shifted[[paste0("tooltip_", var)]]
    
    gg <- ggplot(data_merged_shifted, aes_string(fill = var)) + 
      geom_sf_interactive(aes_string(tooltip = paste0("tooltip_", var), data_id = "NAME"), size = 0.1) + 
      scale_fill_viridis_c(option = "plasma") + 
      labs(
        title = switch(var,
                       discuss = "Percentage of population that often discuss climate change, 2023",
                       citizens = "Percentage of population thinking that citizens should do more, 2023",
                       congress = "Percentage of population thinking that the congress should do more, 2023",
                       consensus = "Percentage of population who believe that global warming is happening, 2023",
                       exp = "Percentage of population who believe that personally experienced global warming, 2023",
                       happening = "Percentage of population who think that global warming is happening, 2023",
                       human = "Percentage who think that global warming is caused mostly by human activities, 2023",
                       worried = "Percentage who are somewhat/very worried about global warming, 2023"
        ),
        caption = "Data source: 2023 Yale Program on Climate Change Communication",
        fill = "% state population"
      ) + 
      theme_void()+
      theme(
        plot.title = element_text(size = 10)  # Adjust title size
      )
    
    girafe(ggobj = gg) %>%
      girafe_options(opts_hover(css = "fill:cyan;"), opts_zoom(max = 10))
  })
  
  output$map <- renderGirafe({
    reactive_map()
  })
}

shinyApp(ui = ui, server = server)
