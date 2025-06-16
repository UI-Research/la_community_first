
library(tidyverse)
library(tidycensus)
library(readxl)
library(openxlsx)
library(sf)

options(tigris_use_cache = TRUE)

#' Access American Community Survey (ACS) data for Los Angeles and for specific boulevards
#'
#' @param file_path The file path to the directory containing the shapefiles and other data files.
#' @param area A character vector indicating the area of interest. One of c("LA", "Pico", "Hoover", "MLK")
#'
#' @return A `sf` object containing the ACS data for the specified area, at the tract level.
get_acs_data <- function(file_path, area = c("LA", "Pico", "Hoover", "MLK")) {
  
  ## we'll use conditional logic to trigger specific actions if we're working with
  ## a boulevard vs. the entire city
  if (area %in% c("Pico", "Hoover", "MLK")) { area_type = "boulevard" } else area_type = "city"
  
  # Set ACS parameters
  year <- 2023
  geo <- "tract"
  state_fips <- "06"
  county_fips <- "037"
  
  # List of ACS tables to pull
  tables <- c("B01001", "B03002", "B05006", "B18101", "B19013", "B17001", "B23001", 
              "B11003", "B18102", "B18103", "B18105", "B23001", "B23022", "B28002", "B28001", 
              "B25003", "B16004", "C16001", "B25070", "B25092", "B25091", "B25044", 
              "B08301", "B08303", "B25071")
  
  # Define a function to retrieve and clean each ACS table
  get_table <- function(tbl) {
    get_acs(
      geography = geo,
      state = state_fips,
      county = county_fips,
      table = tbl,
      year = year,
      output = "wide") }
  
  # Retrieve and merge ACS data
  la_acs_df <- map(tables, get_table) %>%
    reduce(full_join, by = "GEOID") %>%
    select(GEOID, ends_with("E")) %>%
    rename_with(~ str_remove(., "E$"))
  
  la_acs_sf = la_acs_df %>%
    left_join(
      tigris::tracts(state = state_fips, county = county_fips, year = year, class = "sf") %>%
      select(GEOID), 
      by = "GEOID")
  
  la_city_tracts <- st_read(file.path(file_path, "Data", "LA tracts", "LA_City_2020_Census_Tracts_.shp")) %>%
    transmute(GEOID = paste0("06037", CT20))
  
  if (area_type == "boulevard") {
    boulevard_tracts <- st_read(
      file.path(file_path, "Data", "Mapping", "Boulevards", area, str_c(area, "_buffer_tracts.shp")), 
      quiet = TRUE) 
    
    result = la_acs_sf %>%
      filter(GEOID %in% boulevard_tracts$GEOID) %>%
      st_as_sf() }
  
  if (area_type == "city") {
    result = la_acs_sf %>%
      filter(GEOID %in% la_city_tracts$GEOID) %>%
      st_as_sf() }
  
  return(result)
}
