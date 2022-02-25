# Author: Brendan Keane
# Date: February 24th, 2022
# Purpose: Visualizing incarceration rate by geographic region

# Packages
library(leaflet)
library(dplyr)

# Additional source files
fips_location <- read.csv("https://raw.githubusercontent.com/btskinner/spatial/master/data/county_centers.csv") %>%
  select(fips, pclon10, pclat10)

# Adding coordinates to incarceration data
inc_by_coord <- incarceration %>% 
  filter(year == max(year)) %>% # Filter for latest year
  left_join(fips_location) %>%  # Adding coordinates
  mutate(state_and_county = paste(county_name, ", ", state, ": ", total_jail_pop, " people in jail", sep = "")) %>%  # Creating a "State, County" column
  mutate(private_prison = private_jail_flag != 0) %>%
  select(pclon10, pclat10, state_and_county, total_jail_pop, private_prison) 

pal <- colorFactor(c("#A700A7", "#00FFC7"), inc_by_coord$private_prison, na.color = NA)

# Creating national map of prison and jail population
jail_by_geography <- leaflet(data = inc_by_coord) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -97, lat = 37.5, zoom = 4) %>% # Center on continental US
  addCircles(
    lat = inc_by_coord$pclat10,   # county lat
    lng = inc_by_coord$pclon10,   # county long
    popup = inc_by_coord$state_and_county,       # "State, County" location
    radius = sqrt(inc_by_coord$total_jail_pop) * 1200, # Incarcerated-population based radius
    stroke = FALSE,
    color = ~pal(inc_by_coord$private_prison),
    fillOpacity = 0.4
  ) %>%
  addLegend("bottomleft", pal = pal, 
            values = inc_by_coord$private_prison, title = "Private Jail in County")

jail_by_geography
