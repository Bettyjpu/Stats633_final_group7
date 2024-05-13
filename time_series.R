# Data wrangling for data from 2010 - 23

# Load packages
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)

# Read Data and pivot to long format
df_exp <- read_excel("Exp.xlsx")
df_exp_long <- pivot_longer(df_exp, cols = -State, names_to = "year", values_to = "exp")

df_fundrenew <- read_excel("Fundrenewables.xlsx") 
df_fundrenew_long <- pivot_longer(df_fundrenew, cols = -State, names_to = "year", values_to = "fundrenew")

df_future <- read_excel("Futuregen.xlsx")
df_future_long <- pivot_longer(df_future, cols = -State, names_to = "year", values_to = "future")

df_happening <- read_excel("Happening.xlsx")
df_happeing_long <- pivot_longer(df_happening, cols = -State, names_to = "year", values_to = "happening")

# Merge the datasets by the variables "State" and "Year"
df_merged_ts <- Reduce(function(x, y) merge(x, y, by = c("State", "year"), all = TRUE), list(df_exp_long, df_fundrenew_long, df_future_long, df_happeing_long))

