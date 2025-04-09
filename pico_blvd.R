#Install all the below packages if not already installed, using install.packages("package")

#used for pulling census data
library(tidycensus)
#contains analysis packages
library(dplyr)
#contains analysis packages
library(tidyr)
#used for loading/saving excel files
library(readxl)
#used for loading/saving excel files
library(openxlsx)
#contains analysis packages
library(tidyverse)
#used for maps and charts
library(ggplot2)
#used for maps
library(geofacet)
#used for shapefile functions
library(sf)

#used for street data
library(osmdata)

options(tigris_use_cache = TRUE)


#used to pull urban theme template
library(urbnthemes)
options(scipen=999)
set_urbn_defaults(style="print")

#load in data dictionary for acs
v23 <- load_variables(2023, "acs5", cache = TRUE)

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


#load in census api key
#note - you will need to get a census api key to access this#
Sys.getenv("CENSUS_API_KEY")

####Data Analysis####
#set fips codes
state_fips <- "06"
county_fips <- "037"

###Demographic Profile### - Gabe

##Population##
#pull population table from ACS
sex_by_age <- get_acs(
  geography = "tract",
  table = "B01001",
  state = state_fips,
  county = county_fips,
  year = 2023,
  survey = "acs5",
  geometry = FALSE
)

#restructure so that each row is a census tract
##ADD geometry##
age_wide_pico <- sex_by_age %>%
  select(GEOID, variable, estimate, moe) %>%
  filter(variable == "B01001_001", GEOID %in% pico_tracts$GEOID)%>%
  pivot_wider(names_from = variable, values_from = estimate)%>%
  rename(population = B01001_001)

#add census tracts
age_wide_pico <- left_join(pico_tracts, age_wide_pico, by = "GEOID")

#confirm that this is a shapefile
sf::st_geometry(age_wide_pico)

#data - get the total population
totalpop <- sum(age_wide_pico$population)

#create a histogram
population_histogram_pico <- ggplot(age_wide_pico, aes(x = population)) +
  geom_histogram(binwidth = 500) +
  labs(
    title = NULL,
    x = "Population",
    y = "Census Tracts"
  )

ggsave(file.path(file_path, "Pico/Outputs/population_histogram_pico.png"), width = 8, height = 6, dpi = 300)


#add geometry from census tracts to create a map

#create map of population
population_map_pico <- ggplot(age_wide_pico) +
  geom_sf(aes(fill = population), show.legend = TRUE) +
  #add the buffer overlay
  #geom_sf(data = pico_buffer_tracts, color = "black", alpha = 0.2, lwd = 1) +
  #add the street overlay
  geom_sf(data = major_streets, color = "gray20", size = 0.3) +
  scale_fill_gradientn(
    colors = palette_urbn_cyan[c(2, 4, 6, 7)],  # Selecting a subset of colors
    name = "Population by Census Tract",
    labels = scales::comma
  ) +
  theme_urbn_map()  


ggsave(file.path(file_path, "Pico/Outputs/population_map_pico.png"), width = 14, height = 6, dpi = 300)


##Age##
#see sex_by_age

##Race and Ethnicity##
#pull race  from ACS
race <- get_acs(
  geography = "tract",
  table = "B02001",
  state = state_fips,
  county = county_fips,
  year = 2023,
  survey = "acs5",
  geometry = TRUE
)

#pull ethnicity  from ACS
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
#pull income from ACS
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

#pull poverty level from ACS
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
#pull employment status from acs
employment_status <-
  get_acs(
    geography = "tract",
    table = "B23025",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = TRUE,
  )

##Gender##
#see sex_by_age


##Family structure##
#pull family structure
family_structure <-
  get_acs(
    geography = "tract",
    table = "B11003",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = TRUE,
  )

###Language Access and Cultural Considerations### - Gabe

##Limited English##
#pull from acs
language <-
  get_acs(
    geography = "tract",
    table = "B16004",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = TRUE,
  )

##Other languages spoken in the area##
#see language

##Country of origin##
#pull from acs
country_origin <- 
  get_acs(
    geography = "tract",
    table = "B05006",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = TRUE,
  )

###Other Accessibility Considerations###

##Disability Status##
#pull disability status
disability <-
  get_acs(
    geography = "tract",
    table = "B18101",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = TRUE,
  )


#pull hearing difficulty status
hearing_diff <-
  get_acs(
    geography = "tract",
    table = "B18102",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = TRUE,
  )

#pull vision difficulty status
vision_diff <-
  get_acs(
    geography = "tract",
    table = "B18103",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = TRUE,
  )

##Irregular work hours##
#pull hours worked data
hrs_worked <-
  get_acs(
    geography = "tract",
    table = "B23022",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = TRUE,
  )

##Internet/Computer access##
#pull internet data
internet_subs <-
  get_acs(
    geography = "tract",
    table = "B28002",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = TRUE,
  )

#pull computer access data
tech_access <-
  get_acs(
    geography = "tract",
    table = "B28001",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = TRUE,
  )

###Housing & Displacement### - Teddy

##Homeowners/Renters##
#pull housing tenure data
tenure <-   
  get_acs(
  geography = "tract",
  table = "B25003",
  state = state_fips,
  county = county_fips,
  year = 2023,
  survey = "acs5",
  geometry = TRUE,
)


##Housing Cost Burden##
#pull cost burden data
#for renters
renter_burden <-   
  get_acs(
    geography = "tract",
    table = "B25070",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = TRUE,
  )

#for owners
owner_burden <-   
  get_acs(
    geography = "tract",
    table = "B25091",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = TRUE,
  )

##Disadvantaged communities##

#load in CES disadvantaged community data
ces_disadvantaged <- read.xlsx(file.path(file_path, "Data/Disadvantaged/sb535_tract_all_data.xlsx"))%>%
  rename(GEOID = Census.Tract)

#load in displacement risk
displacement_risk <- read.csv(file.path(file_path, "Data/la_displacement_index.csv"))

###Transportation and Commuting###

##Car free and one car##
#pull car data
vehicles <-   
  get_acs(
    geography = "tract",
    table = "B25044",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = TRUE,
  )

##Method for commuting to work##
#pull means of transportation data
transportation_means <-   
  get_acs(
    geography = "tract",
    table = "B08301",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = TRUE,
  )

#pull travel time data
travel_time <-   
  get_acs(
    geography = "tract",
    table = "B08303",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = TRUE,
  )

##H+T Index metrics##
#pull H+T data
ht_index <- read.csv(file.path(file_path, "Data/htaindex2022_data_tracts_06.csv"))%>%
  rename(GEOID = tract)%>%
  mutate(GEOID = gsub('"', '', GEOID))%>%
  mutate(GEOID = as.numeric(GEOID))

#housing and transportation costs as percent of income

#all transit performance score

##Bike score

##Collisions - not sure about best data here




