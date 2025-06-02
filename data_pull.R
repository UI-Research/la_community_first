### Data gathering file for the Pico Boulevard SCA ###

#libraries
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


#setting wd
username = getwd() %>% str_split("\\/") %>% unlist %>% .[3]
file_path <- file.path("C:", "Users", username, "Box", "LA Transit project/Social Climate Analysis")



#### Step 1: Pulling in ACS Data for LA County ####

#setting acs pull parameters
year <- 2023
geo  <- "tract"  
state_fips <- "06"
county_fips <- "037"

tables <- c("B01001", "B03002", "B05006", "B18101", "B19013", "B17001", "B23001", 
            "B11003","B18102", "B18103", "B18105", "B23022", "B28002", "B28001", 
            "B25003", "B16004", "C16001", "B25070", "B25092", "B25091", "B25044", 
            "B08301", "B08303")


#defining function to retrieve each table and clean for joining
get_table <- function(tbl) {
  get_acs(
    geography = geo,
    state = state_fips,
    county = county_fips,
    table = tbl,
    year = year,
    output = "wide"
  )
}

#obtaining all tables and joining by GEOID
acs_data <- map(tables, get_table) %>%
  reduce(full_join, by = "GEOID")


#only keeping estimates
la_acs <- acs_data %>%
  select(GEOID, NAME, ends_with("E")) %>% 
  rename_with(~ str_remove(., "E$"))

#merging with shapefile
la_acs_sf <- la_acs %>%
  left_join(tigris::tracts(state = state_fips, county = county_fips, year = year, class = "sf"), by = "GEOID")



#### Step 2: Loading in shapefiles to obtain Pico census tracts ####

#los angeles tracts
CA_tracts <- st_read(file.path(file_path, "Hoover/Mapping/CA_census_Tracts.shp"))
la_tracts <- CA_tracts %>% filter(COUNTYFP == "037")
la_shp <- st_read(file.path(file_path, "Data/tl_2023_06_tract/tl_2023_06_tract.shp"))

#pico tracts
pico_buffer_tracts <- st_read(file.path(file_path, "Pico/Maps/pico_buffer_tracts.shp"))
pico_tracts <- la_shp%>%
  filter(GEOID %in% pico_buffer_tracts$GEOID)

#streets
bbox <- st_bbox(pico_tracts)
streets <- opq(bbox = bbox) %>%
  add_osm_feature(key = "highway") %>%  # 'highway' includes roads, streets, etc.
  osmdata_sf()
#extract just streets
streets_lines <- streets$osm_lines
#filter to main streets
major_streets <- streets_lines[streets_lines$highway %in% c("primary", "secondary", "tertiary"), ]
major_streets <- st_transform(major_streets, st_crs(pico_tracts)) #transform CRS
major_streets_clipped <- st_intersection(major_streets, st_union(st_geometry(pico_tracts))) #restricting streets to census tract boundaries


#### Step 3: Filter data to Pico tracts ####
pico_master_data <- la_acs_sf %>%
  filter(GEOID %in% pico_tracts$GEOID) %>%
  st_as_sf()

class(pico_master_data) #checking to make sure it is a sf

#save pico master data file as csv
write.csv(pico_master_data, file = file.path(file_path, "/Pico/pico_master_data.csv"), row.names = FALSE)




#### Step 4: Making a separate df of Los Angeles city census data ####

#loading in LA city tracts
la_city_tracts <- st_read(file.path(file_path, "/Data/LA tracts/LA_City_2020_Census_Tracts_.shp"))%>%
  mutate(GEOID = paste0("06037", CT20))

#filter census data to LA city tracts
la_city_master_data <- la_acs_sf %>%
  filter(GEOID %in% la_city_tracts$GEOID) %>%
  st_as_sf()

#save la city master data file as csv
write.csv(la_city_master_data, file = file.path(file_path, "/Data/la_city_master_data.csv"), row.names = FALSE)










