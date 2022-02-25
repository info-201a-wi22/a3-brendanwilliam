# Author: Brendan Keane
# Date: February 24th, 2022
# Purpose: Compare portion of race in jail by region (2018)

# Wrangling data to create a population over time graph coded by race
inc_by_region <- incarceration %>%
  group_by(division) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  mutate(pop_white_jail = sum(white_jail_pop, na.rm = TRUE)) %>%
  mutate(pop_black_jail = sum(black_jail_pop, na.rm = TRUE)) %>%
  mutate(pop_latinx_jail = sum(latinx_jail_pop, na.rm = TRUE)) %>%
  mutate(pop_native_jail = sum(native_jail_pop, na.rm = TRUE)) %>%
  mutate(pop_aapi_jail = sum(aapi_jail_pop, na.rm = TRUE)) %>%
  mutate(pop_total_jail = pop_white_jail + pop_black_jail + pop_native_jail + pop_latinx_jail + pop_aapi_jail) %>%
  select(division, pop_total_jail, pop_white_jail, pop_black_jail, pop_latinx_jail, pop_native_jail, pop_aapi_jail) %>%
  unique()

# Normalizing by overall population
inc_by_region_normal <- inc_by_region %>%
  mutate(pop_white_jail = pop_white_jail * 100 / pop_total_jail) %>%
  mutate(pop_black_jail = pop_black_jail * 100/ pop_total_jail) %>%
  mutate(pop_latinx_jail = pop_latinx_jail * 100/ pop_total_jail) %>%
  mutate(pop_native_jail = pop_native_jail * 100/ pop_total_jail) %>%
  mutate(pop_aapi_jail = pop_aapi_jail * 100/ pop_total_jail) %>%
  select(-pop_total_jail) %>%
  rename("White" = "pop_white_jail", "Black" = "pop_black_jail", 
         "Latinx" = "pop_latinx_jail", "AAPI" = "pop_aapi_jail",
         "Native American" = "pop_native_jail") %>%
  gather(key = race, value = population, -division)



jail_by_raceandregion <- plot_ly(
  data = inc_by_region_normal,
  x = ~division,
  y = ~population,
  color = ~race,
  colors = c("#0000CC", "#5CE6B5", "#FFFF00", "#C70000", "#FF8FEC"),
  marker = list(colorscale = "Accent"),
  type = "bar"
) %>% layout(
  title = "Composition of Jailed Population by Region",
  legend = list(title = list(text = "Race")),
  yaxis = list(title = "% of Jailed Population"), barmode = "stack",
  xaxis = list(title = "Geographic Region"),
  plot_bgcolor = "#f5f5f5"
  )
  
jail_by_raceandregion
