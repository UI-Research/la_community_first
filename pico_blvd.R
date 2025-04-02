#Install all the below packages if not already installed, using install.packages("package")

#used for pulling census data
library(tidycensus)
#contains analysis packages
library(dplyr)
#contains analysis packages
library(tidyr)
#used for loading/saving excel files
library(readxl)
#contains analysis packages
library(tidyverse)
#used for maps and charts
library(ggplot2)
#used for maps
library(geofacet)
#used for shapefile functions
library(sf)

#used to pull urban theme template
library(urbnthemes)
options(scipen=999)
set_urbn_defaults(style="print")

####Load and Clean####
#set personal file path to make it easier to pull data

#Gabe
file_path <- file.path("C:/Users/GSamuels/Box/LA Transit project/Social Climate Analysis")

#Teddy
#file_path <- file.path("C:/Users/TMaginn/Box/LA Transit project/Social Climate Analysis")

###load in shapefiles###

##census tracts##
CA_tracts <- st_read(file.path(file_path, "Hoover/Mapping/CA_census_Tracts.shp"))
la_tracts <- CA_tracts %>% filter(COUNTYFP == "037")

#pico tracts
pico_tracts <- st_read(file.path(file_path, "Pico/Maps/pico_buffer_tracts.shp"))

#load in census api key
#note - you will need to get a census api key to access this#
Sys.getenv("CENSUS_API_KEY")

####Data Analysis####
#set fips codes
state_fips <- "06"
county_fips <- "037"

###Demographic Profile###

##Population##
sex_by_age <- get_acs(
  geography = "tract",
  table = "B01001",
  state = state_fips,
  county = county_fips,
  year = 2023,
  survey = "acs5",
  geometry = TRUE,
  cache_table = TRUE
)

##Age##

##Race and Ethnicity##
race <- get_acs(
  geography = "tract",
  table = "B02001",
  state = state_fips,
  county = county_fips,
  year = 2023,
  survey = "acs5",
  geometry = TRUE
)

ethnicity <- 
  get_acs(
    geography = "tract",
    table = "B03002",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = TRUE
  )

##Median Income/Poverty##
median_income <- 
  get_acs(
    geography = "tract",
    table = "B19013",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = TRUE
  )

poverty_level <-
  get_acs(
    geography = "tract",
    table = "B17001",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = TRUE,
    )
  
##Employment Status##

##Gender##

##Family structure##

###Language Access and Cultural Considerations###

##Limited English##

##Other languages spoken in the area##

##Country of origin##

###Other Accessibility Considerations###

##Disability Status##

##Irregular work hours##

##Internet/Computer access##

###Housing & Displacement###

##Homeowners/Renters##

##Housing Cost Burden##

###Transportation and Commuting###

##Car free and one car##

##Method for commuting to work##

##H+T Index metrics##





