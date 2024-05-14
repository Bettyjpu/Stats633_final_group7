# Data wrangling for data from 2010 - 23

# Load packages
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(reshape2)

# Read Data and pivot to long format
df_exp <- read_excel("Exp.xlsx")
df_exp_long <- pivot_longer(df_exp, cols = -State, names_to = "year", values_to = "exp")

df_fundrenew <- read_excel("Fundrenewables.xlsx") 
df_fundrenew_long <- pivot_longer(df_fundrenew, cols = -State, names_to = "year", values_to = "fundrenew")

df_future <- read_excel("Futuregen.xlsx")
df_future_long <- pivot_longer(df_future, cols = -State, names_to = "year", values_to = "future")

df_happening <- read_excel("Happening.xlsx")
df_happeing_long <- pivot_longer(df_happening, cols = -State, names_to = "year", values_to = "happening")

df_governer <- read_excel("Governer.xlsx")
df_governer_long <- pivot_longer(df_governer, cols = -State, names_to = "year", values_to = "governer")
  
df_harmus <- read_excel("harmus.xlsx")
df_harmus_long <- pivot_longer(df_harmus, cols = -State, names_to = "year", values_to = "harmus")
  
df_human <- read_excel("human.xlsx")
df_human_long <- pivot_longer(df_human, cols = -State, names_to = "year", values_to = "human")

  
df_timing <- read_excel("timing.xlsx")
df_timing_long <- pivot_longer(df_timing, cols = -State, names_to = "year", values_to = "timing")


# Merge the datasets by the variables "State" and "Year"
df_merged_ts <- Reduce(function(x, y) merge(x, y, by = c("State", "year"), all = TRUE), list(df_exp_long, df_fundrenew_long, df_future_long, df_happeing_long,
                                                                                             df_governer_long, df_harmus_long, df_human_long,
                                                                                             df_timing_long))

df_plot_ts <- melt(df_merged_ts, id.vars=c("State", "year"), variable.name="question")


df_plot_ts %>%
  group_by(question, year) %>%
  summarise(min = min(value), max = max(value), .groups = 'drop') %>%
  ungroup() %>%
  mutate(question_year = interaction(question, year)) %>%
  mutate(question_year = forcats::fct_reorder(question_year, min), year = forcats::fct_reorder(year, min))%>%
  plot_ly() %>%
  add_segments(
    x = ~min, y = ~question_year,
    xend = ~max, yend = ~question_year,
    color = ~year, showlegend = TRUE
  ) %>%
  add_markers(
    x = ~min, y = ~question_year,
    symbol = I("square"),
    color = ~year,
    name = "minimum %",
    legendgroup = ~year,
    showlegend = FALSE,
    text = ~paste0(~State, ": ", min, "% of state population"),
    hoverinfo = "text"
  ) %>%
  add_markers(
    x = ~max, y = ~question_year, 
    color = ~year,
    name  = "maximum %",
    legendgroup = ~year,
    showlegend = FALSE,
    text = ~paste0(~State, ": ", max, "% of state population"),
    hoverinfo = "text"
  ) %>%
  layout(yaxis = list(title = "Questions and Years"),
         xaxis = list(title = "Americans' opinion on climate change in %"))








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
    df_plot_ts %>%
      filter(year == input$year) %>%
      group_by(question) %>%
      summarise(min = min(value), max = max(value))
  })
  
  output$opinion_plot <- renderPlotly({
    plot_ly(filtered_data(), type = 'scatter', mode = 'markers+lines') %>%
      add_segments(
        x = ~min, y = ~question,
        xend = ~max, yend = ~question,
        color = I("gray"), showlegend = TRUE
      ) %>%
      add_markers(
        x = ~min, y = ~question,
        color = I("blue"),
        name = "minimum"
      ) %>%
      add_markers(
        x = ~max, y = ~question, 
        color = I("red"),
        name  = "maximum"
      ) %>%
      layout(
        xaxis = list(title = "Americans' opinion on climate change in %")
      )
  })
}

shinyApp(ui = ui, server = server)






