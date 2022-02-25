# Author: Brendan Keane
# Date: February 24th, 2022
# Purpose: Analyze incarceration statistics and generate insightful graphics

# Packages
library(dplyr)
library(plotly)

# Importing data
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Key statistics

# Total number of people in jail in 2018
total_jail_2018 <- incarceration %>%
  filter(year == 2018) %>%
  select(total_jail_pop) %>%
  sum(na.rm = TRUE) %>%
  round()

# Total number of people in jail in 1985
total_jail_1985 <- incarceration %>%
  filter(year == 1985) %>%
  select(total_jail_pop) %>%
  sum(na.rm = TRUE) %>%
  round()

# Percentage growth of jail from 1985 to 2018
percent_change_jail <- round(100 * total_jail_2018 / total_jail_1985, digits = 2)

# Total US population 2018
total_pop_2018 <- incarceration %>%
  filter(year == 2018) %>%
  select(total_pop) %>%
  sum(na.rm = TRUE) %>%
  round()

# Total US population 1985
total_pop_1985 <- incarceration %>%
  filter(year == 1985) %>%
  select(total_pop) %>%
  sum(na.rm = TRUE) %>%
  round()

# Percentage growth of US population from 1985 to 2018
percent_change_pop <- round(100 * total_pop_2018 / total_pop_1985, digits = 2)

# Jailed growth rate versus US population growth rate
prison_growth_rate <- round(percent_change_jail / percent_change_pop, digits = 2)

# States in data set
num_states <- incarceration %>%
  select(state) %>%
  unique() %>%
  pull() %>%
  length()

# Counties in data set
num_counties <- incarceration %>%
  select(fips) %>%
  unique() %>%
  pull() %>%
  length()

# Source files (See files for additional details)
source("time-chart.R")      # Trends over time chart
source("variable-chart.R")  # Variable comparison chart
source("map-chart.R")       # Map chart

