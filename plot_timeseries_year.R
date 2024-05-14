# Plot average % by year

# Load packages
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(reshape2)

# Read Data and pivot to long format
df_ts <- read_excel("YCOM_ts.xlsx")%>%
  select(-c("State code"))

# Pivot the dataset to long format
df_ts_long <- pivot_longer(df_ts, 
                           cols = -c(State, Question), 
                           names_to = "year", 
                           values_to = "value")


library(shiny)
library(dplyr)
library(plotly)

# Assuming df_plot is available in your shiny app

ui <- fluidPage(
  titlePanel("Climate Change Opinion Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Select Year:", min = 2010, max = 2023, value = 2010)
    ),
    mainPanel(
      plotlyOutput("opinion_plot")
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    df_ts_long %>%
      filter(year == input$year) %>%
      group_by(Question) %>%
      summarise(min= min(value), max = max(value))
  })
  
  output$opinion_plot <- renderPlotly({
    plot_ly(filtered_data(), type = 'scatter', mode = 'markers+lines') %>%
      add_segments(
        x = ~min, y = ~Question,
        xend = ~max, yend = ~Question,
        color = I("gray"), showlegend = TRUE
      ) %>%
      add_markers(
        x = ~min, y = ~Question,
        color = I("blue"),
        name = "minimum"
      ) %>%
      add_markers(
        x = ~max, y = ~Question, 
        color = I("red"),
        name  = "maximum"
      ) %>%
      layout(
        xaxis = list(title = "Proportion of people answering 'yes' (%)"),
        yaxis = list(title="Climate Change Questions")
      )
  })
}

shinyApp(ui = ui, server = server)
