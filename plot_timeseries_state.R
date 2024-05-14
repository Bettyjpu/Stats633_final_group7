library(shiny)
library(dplyr)
library(plotly)
library(readxl)
library(tidyr)

# Read Data and pivot to long format
df_ts <- read_excel("YCOM_ts.xlsx") %>%
  select(-c("State code"))

# Pivot the dataset to long format
df_ts_long <- pivot_longer(df_ts, 
                           cols = -c(State, Question), 
                           names_to = "year", 
                           values_to = "value")

ui <- fluidPage(
  titlePanel("Climate Change Opinion Analysis by State"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select State:", choices = unique(df_ts_long$State))
    ),
    mainPanel(
      plotlyOutput("state_opinion_plot")
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    df_ts_long %>%
      filter(State == input$state) %>%
      group_by(Question) %>%
      summarise(min_value = min(value),
                max_value = max(value),
                min_year = year[which.min(value)],
                max_year = year[which.max(value)])
  })
  
  output$state_opinion_plot <- renderPlotly({
    plot_ly(filtered_data(), type = 'scatter', mode = 'markers+lines') %>%
      add_segments(
        x = ~min_value, y = ~Question,
        xend = ~max_value, yend = ~Question,
        color = I("gray"), showlegend = TRUE
      ) %>%
      add_markers(
        x = ~min_value, y = ~Question,
        color = I("blue"),
        name = "minimum",
        hoverinfo = "text",
        text = ~paste("Year:", min_year, "<br>Value:", min_value)
      ) %>%
      add_markers(
        x = ~max_value, y = ~Question, 
        color = I("red"),
        name  = "maximum",
        hoverinfo = "text",
        text = ~paste("Year:", max_year, "<br>Value:", max_value)
      ) %>%
      layout(
        xaxis = list(title = "Proportion of people answering 'yes' (%)"),
        yaxis = list(title = "Climate Change Questions")
      )
  })
}

shinyApp(ui = ui, server = server)

