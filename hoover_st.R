library(tidycensus)
library(dplyr)
library(tidyr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(geofacet)
library(dotenv)
library(sf)
library(urbnthemes)

#test

options(scipen=999)
set_urbn_defaults(style="print")

####Load and Clean####
file_path <- file.path("C:/Users/GSamuels/Box/LA Transit project/Social Climate Analysis")


###load in shapefiles###

##census tracts##
CA_tracts <- st_read(file.path(file_path, "Hoover/Mapping/CA_census_Tracts.shp"))
la_tracts <- CA_tracts %>% filter(COUNTYFP == "037")

##hoover tracts##
hoover_tracts <- st_read(file.path(file_path, "Hoover/Mapping/hoover_buffer_census_tracts.shp"))
st_crs(hoover_tracts)
hoover_tracts <- st_set_crs(hoover_tracts, 2229)

#load in census api key
Sys.getenv("CENSUS_API_KEY")

####Analysis####

##Race##

#pull from ACS
race_data <- get_acs(
  geography = "tract",
  variables = c(
    total = "B02001_001",
    white = "B02001_002",
    black = "B02001_003",
    native = "B02001_004",
    asian = "B02001_005",
    pacific = "B02001_006",
    other = "B02001_007",
    two_or_more = "B02001_008"
  ),
  state = "CA",
  county = "Los Angeles",
  geometry = TRUE
)

#check and update crs to match what we have for hoover st 
st_crs(race_data)
race_data <- st_transform(race_data, 2229)


#pivot so that each row is a tract and that each percentage is displayed
race_wide <- race_data %>%
  select(GEOID, variable, estimate, geometry) %>%
  pivot_wider(names_from = variable, values_from = estimate)%>%
  mutate(
    pct_white = (white / total) * 100,
    pct_black = (black / total) * 100,
    pct_native = (native / total) * 100,
    pct_asian = (asian / total) * 100,
    pct_pacific = (pacific / total) * 100,
    pct_other = (other / total) * 100,
    pct_two_or_more = (two_or_more / total) * 100
  )

#bind percentage data with hoover st tracts
hoover_race <- race_wide %>%
  filter(GEOID %in% hoover_tracts$GEOID)


#create graphs
#Black population
hoover_black_map <- hoover_race %>%
  ggplot() +
  geom_sf(aes(
    # Color in states by the pct_black variable
    fill = pct_black
  ))+
  theme(
    axis.text = element_blank(),  # Remove lat/lon text
    axis.ticks = element_blank(),  # Remove tick marks
    panel.grid = element_blank()  # Remove grid lines
  ) +
  labs(
    title = "Black Population Percentage in Hoover Tracts",
    fill = "Percentage (%)",
    caption = "Data Source: tidycensus (ACS 5-Year)"
  )

#white population
hoover_white_map <- hoover_race %>%
  ggplot() +
  geom_sf(aes(
    # Color in states by the pct_white variable
    fill = pct_white
  ))+
  theme(
    axis.text = element_blank(),  # Remove lat/lon text
    axis.ticks = element_blank(),  # Remove tick marks
    panel.grid = element_blank()  # Remove grid lines
  ) +
  labs(
    title = "White Population Percentage in Hoover Tracts",
    fill = "Percentage (%)",
    caption = "Data Source: tidycensus (ACS 5-Year)"
  )

#asian population
hoover_asian_map <- hoover_race %>%
  ggplot() +
  geom_sf(aes(
    # Color in states by the pct_asian variable
    fill = pct_asian
  ))+
  theme(
    axis.text = element_blank(),  # Remove lat/lon text
    axis.ticks = element_blank(),  # Remove tick marks
    panel.grid = element_blank()  # Remove grid lines
  ) +
  labs(
    title = "Asian Population Percentage in Hoover Tracts",
    fill = "Percentage (%)",
    caption = "Data Source: tidycensus (ACS 5-Year)"
  )


#other race
hoover_other_map <- hoover_race %>%
  ggplot() +
  geom_sf(aes(
    # Color in states by the pct_other variable
    fill = pct_other
  ))+
  theme(
    axis.text = element_blank(),  # Remove lat/lon text
    axis.ticks = element_blank(),  # Remove tick marks
    panel.grid = element_blank()  # Remove grid lines
  ) +
  labs(
    title = "Other Race Population Percentage in Hoover Tracts",
    fill = "Percentage (%)",
    caption = "Data Source: tidycensus (ACS 5-Year)"
  )

##Ethnicity##

#pull hispanic data from acs
hispanic_data <- get_acs(
  geography = "tract",
  variables = c(total = "B03002_001", hispanic = "B03002_003"),
  state = "CA",
  county = "Los Angeles",
  geometry = TRUE
)

#check and update crs to match what we have for hoover st 
st_crs(hispanic_data)
hispanic_data <- st_transform(hispanic_data, 2229)

#pivot so that each row is a tract and that each percentage is displayed
hispanic_wide <- hispanic_data %>%
  select(GEOID, variable, estimate, geometry) %>%
  pivot_wider(names_from = variable, values_from = estimate)%>%
  mutate(
    pct_hispanic = (hispanic / total) * 100,
  )

#bind percentage data with hoover st tracts
hoover_hispanic <- hispanic_wide %>%
  filter(GEOID %in% hoover_tracts$GEOID)

##didn't make a map because the % is very low, but we could#

##Language##


#etc#




