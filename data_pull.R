### Data gathering file for the Pico Boulevard SCA ###

clean_and_prepare_data <- function(file_path) {
  # Load necessary libraries
  library(tidycensus)
  library(tmap)
  library(cols4all)
  library(tidyverse)
  library(readxl)
  library(openxlsx)
  library(geofacet)
  library(sf)
  library(scales)
  library(osmdata)
  library(writexl)

  
  # Set ACS parameters
  year <- 2023
  geo <- "tract"
  state_fips <- "06"
  county_fips <- "037"
  
  # List of ACS tables to pull
  tables <- c("B01001", "B03002", "B05006", "B18101", "B19013", "B17001", "B23001", 
              "B11003", "B18102", "B18103", "B18105", "B23022", "B28002", "B28001", 
              "B25003", "B16004", "C16001", "B25070", "B25092", "B25091", "B25044", 
              "B08301", "B08303")
  
  # Define a function to retrieve and clean each ACS table
  get_table <- function(tbl) {
    get_acs(
      geography = geo,
      state = state_fips,
      county = county_fips,
      table = tbl,
      year = year,
      output = "wide",
      cache_table = T
    )
  }
  
  # Retrieve and merge ACS data
  acs_data <- map(tables, get_table) %>%
    reduce(full_join, by = "GEOID")
  
  la_acs <- acs_data %>%
    select(GEOID, NAME, ends_with("E")) %>%
    rename_with(~ str_remove(., "E$"))
  
  # Merge with shapefile
  la_acs_sf <- la_acs %>%
    left_join(tigris::tracts(state = state_fips, county = county_fips, year = year, class = "sf"), by = "GEOID")
  
  # Load external shapefiles
  CA_tracts <- st_read(file.path(file_path, "Hoover/Mapping/CA_census_Tracts.shp"), quiet = TRUE)
  la_shp <- st_read(file.path(file_path, "Data/tl_2023_06_tract/tl_2023_06_tract.shp"), quiet = TRUE)
  pico_buffer_tracts <- st_read(file.path(file_path, "Pico/Maps/pico_buffer_tracts.shp"), quiet = TRUE)
  
  # Extract relevant Pico tracts
  pico_tracts <- la_shp %>%
    filter(GEOID %in% pico_buffer_tracts$GEOID)
  
  # Get major streets within Pico tract bounding box
  bbox <- st_bbox(pico_tracts)
  streets <- opq(bbox = bbox) %>%
    add_osm_feature(key = "highway") %>%
    osmdata_sf()
  
  major_streets <- streets$osm_lines %>%
    filter(highway %in% c("primary", "secondary", "tertiary")) %>%
    st_transform(st_crs(pico_tracts)) %>%
    st_intersection(st_union(st_geometry(pico_tracts)))
  
  # Filter ACS data to Pico tracts
  pico_master_data <- la_acs_sf %>%
    filter(GEOID %in% pico_tracts$GEOID) %>%
    st_as_sf()
  
  return(pico_master_data)
}




















