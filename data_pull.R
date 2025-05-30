library(tidycensus)
library(tmap)
library(cols4all)
library(tidyverse)
library(readxl)
library(openxlsx)
library(geofacet)
library(sf)
library(scales)


#setting wd
username = getwd() %>% str_split("\\/") %>% unlist %>% .[3]
file_path <- file.path("C:", "Users", username, "Box", "LA Transit project/Social Climate Analysis")

year <- 2023
geo  <- "tract"  
state_fips <- "06"
county_fips <- "037"

tables <- c("B01001", "B03002", "B05006", "B18101", "B19013", "B17001", "B23001", "B11003","B18102", "B18103", "B18105", "B23022", "B28002", "B28001", "B25003", "B16004", "C16001", "B25070", "B25092", "B25091", "B25044", "B08301", "B08303")

acs_data <- map_df(
  tables,
  ~ get_acs(
    geography = geo,
    state = state_fips,
    county = county_fips,
    table = .x,
    year = year,
    output = "wide"
  ),
  .id = "table_id"
)

la_acs <- acs_data %>%
  select(GEOID, NAME, ends_with("E")) %>%  # Keep only estimates
  rename_with(~ str_remove(., "E$"))

la_acs_sf <- la_acs %>%
  left_join(tigris::tracts(state = state_fips, county = county_fips, year = year, class = "sf"), by = "GEOID")
