# Author: Brendan Keane
# Date: February 24th, 2022
# Purpose: Jail population over time with race coding

# Packages
library(tidyr)
library(dplyr)
library(plotly)
library(ggplot2)

# Wrangling data to create a population over time graph coded by race
inc_over_time <- incarceration %>%
  group_by(year) %>%
  filter(year >= 1985) %>%
  mutate(pop_white_jail = sum(white_jail_pop, na.rm = TRUE)) %>%
  mutate(pop_black_jail = sum(black_jail_pop, na.rm = TRUE)) %>%
  mutate(pop_latinx_jail = sum(latinx_jail_pop, na.rm = TRUE)) %>%
  mutate(pop_native_jail = sum(native_jail_pop, na.rm = TRUE)) %>%
  mutate(pop_aapi_jail = sum(aapi_jail_pop, na.rm = TRUE)) %>%
  select(year, pop_white_jail, pop_black_jail, pop_latinx_jail, pop_native_jail, pop_aapi_jail) %>%
  unique()

inc_over_time <- gather(inc_over_time, key = race, value = population, -year)

jail_over_time_race <- ggplot(inc_over_time) +
  geom_line(mapping = aes(x = year, y = population, color = race)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title="Population In Jail by Race", x="Year", y="Population", fill="Race") +
  # Adjusting colors of aapi/black/latinx/native/white in order
  scale_color_manual(labels = c("AAPI", "Black", "Latinx", "Native American", "White"),
    values = c("#0000CC", "#5CE6B5", "#c4c400", "#C70000", "#FF8FEC"))

jail_over_time_race
