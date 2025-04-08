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

#used to pull urban theme template
library(urbnthemes)
options(scipen=999)
set_urbn_defaults(style="print")

####Load and Clean####
#set personal file path to make it easier to pull data

#Gabe
#file_path <- file.path("C:/Users/GSamuels/Box/LA Transit project/Social Climate Analysis")

#Teddy
file_path <- file.path("C:/Users/TMaginn/Box/LA Transit project/Social Climate Analysis")

###load in shapefiles###

##census tracts##
CA_tracts <- st_read(file.path(file_path, "Hoover/Mapping/CA_census_Tracts.shp"))
la_tracts <- CA_tracts %>% filter(COUNTYFP == "037")
la_shp <- st_read(file.path(file_path, "Data/tl_2023_06_tract/tl_2023_06_tract.shp"))

#pico tracts
pico_buffer_tracts <- st_read(file.path(file_path, "Pico/Maps/pico_buffer_tracts.shp"))
pico_tracts <- la_shp%>%
  filter(GEOID %in% pico_buffer_tracts$GEOID)

#load in census api key
#note - you will need to get a census api key to access this#
Sys.getenv("CENSUS_API_KEY")

####Data Analysis####
#set fips codes
state_fips <- "06"
county_fips <- "037"

###load list of 2023 acs variables
v23 <- load_variables(2023, "acs5", cache = TRUE)

########################Demographic Profile#########################

##Population##
#pull population table from ACS
sex_by_age <- get_acs(
  geography = "tract",
  table = "B01001",
  state = state_fips,
  county = county_fips,
  year = 2023,
  survey = "acs5",
  geometry = FALSE, #change to TRUE for maps
  cache_table = TRUE
)


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
  geometry = FALSE #change to TRUE for maps
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
    geometry = FALSE #change to TRUE for maps
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
    geometry = FALSE #change to TRUE for maps
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
    geometry = FALSE, #change to TRUE for maps
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
    geometry = FALSE, #change to TRUE for maps
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
    geometry = FALSE, #change to TRUE for maps
  )

##################Language Access and Cultural Considerations#######################

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
    geometry = FALSE, ##change to TRUE for map of other languages spoken
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
    geometry = FALSE,
  )

########################Other Accessibility Considerations########################

##Disability Status##
#pull disability status 
disability <- 
  get_acs(
    geography = "tract",
    variables = c(
      total_pop = "B18101_001", 
      v1="B18101_004", 
      v2="B18101_007", 
      v3="B18101_010", 
      v4="B18101_013", 
      v5="B18101_016", 
      v6="B18101_019",
      v7="B18101_023", 
      v8="B18101_026", 
      v9="B18101_029", 
      v10="B18101_032", 
      v11="B18101_035", 
      v12="B18101_038"
    ),
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE
  ) %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)%>% #pivoting so each tract is one row and each variable is one column
  mutate(
    total_disability =v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12, #summing diability by sex by age to get total population with disability by tract
    share_disability = total_disability/total_pop)%>% #generating share of tract population with disability
  select(GEOID,total_pop,total_disability,share_disability) #reducing df to necessary variables

#binding with pico tracts to limit our data to the study area
pico_disability <- disability %>%
  filter(GEOID %in% pico_tracts$GEOID)

#add geometry from census tracts to create a map
st_geometry(pico_disability) <- st_geometry(pico_tracts[match(pico_disability$GEOID, pico_tracts$GEOID), ])
class(pico_disability)


#creating custom bins for a map of %pop w/ disability
map_disability_pico <- pico_disability%>% 
  mutate(bin_disability = cut(total_disability, breaks=c(0,200, 300, 400, 500,800),
                              labels  = c("Less than 200", "200-300", "300-400", "400-500", "500 or more"),
                              include.lowest = TRUE))
#next, make each of the bins a factor
map_disability_pico$bin_disability <- factor(map_disability_pico$bin_disability, 
                                       levels = c("Less than 200", "200-300", "300-400", "400-500", "500 or more"))

#making the map
plot <-ggplot()+
  geom_sf(map_disability_pico, mapping = aes(fill = bin_disability), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "black", alpha = 0, lwd = 1) + # Adding the buffer zone as a transparent overlay
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = palette_urbn_cyan[c(1,3,5,6,8)], #can adjust the palette or color scheme as necessary
    name = "Number of individuals with a disability",
    breaks = c("Less than 200", "200-300", "300-400", "400-500", "500 or more")
  )+
  theme_urbn_map()

print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/disability_map_pico.png"), width = 14, height = 6, dpi = 300)


###pull hearing difficulty status
hearing_diff <- 
  get_acs(
    geography = "tract",
    variables = c(
      total_pop = "B18102_001", 
      v1="B18102_004", 
      v2="B18102_007", 
      v3="B18102_010", 
      v4="B18102_013", 
      v5="B18102_016", 
      v6="B18102_019",
      v7="B18102_023", 
      v8="B18102_026", 
      v9="B18102_029", 
      v10="B18102_032", 
      v11="B18102_035", 
      v12="B18102_038"
    ),
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE
  ) %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)%>% #pivoting
  mutate(
    total_hearing_diff =v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12, #summing hearing diff by sex by age to get total population with hearing diff
    share_hearing_diff = total_hearing_diff/total_pop)%>% #generating share of tract population with hearing diff
  select(GEOID,total_pop,total_hearing_diff,share_hearing_diff)%>% #reducing df to necessary variables
  filter(GEOID %in% pico_tracts$GEOID) #limiting to pico tracts

#add geometry for map
st_geometry(hearing_diff) <- st_geometry(pico_tracts[match(hearing_diff$GEOID, pico_tracts$GEOID), ])
class(hearing_diff)

#creating custom bins for a map of %pop w/ hearing diff
map_hearing_diff <- hearing_diff%>% 
  mutate(bin_hearing_diff = cut(total_hearing_diff, breaks=c(0,50,100,150,99999),
                              labels  = c("Less than 50", "50-100", "100-150", "150 or more"),
                              include.lowest = TRUE))
#making each bin a factor
map_hearing_diff$bin_hearing_diff <- factor(map_hearing_diff$bin_hearing_diff, 
                                             levels = c("Less than 50", "50-100", "100-150", "150 or more"))

#plot map
plot <-ggplot()+
  geom_sf(map_hearing_diff, mapping = aes(fill = bin_hearing_diff), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "black", alpha = 0, lwd = 1) + # Adding the buffer zone as a transparent overlay
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = palette_urbn_cyan[c(1,3,6,8)], #can adjust the palette or color scheme as necessary
    name = "Number of individuals with hearing difficulty",
    breaks = c("Less than 50", "50-100", "100-150", "150 or more")
  )+
  theme_urbn_map()

print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/hearing_diff_map_pico.png"), width = 14, height = 6, dpi = 300)


####pull vision difficulty status####
vision_diff <- 
  get_acs(
    geography = "tract",
    variables = c(
      total_pop = "B18103_001", 
      v1="B18103_004", 
      v2="B18103_007", 
      v3="B18103_010", 
      v4="B18103_013", 
      v5="B18103_016", 
      v6="B18103_019",
      v7="B18103_023", 
      v8="B18103_026", 
      v9="B18103_029", 
      v10="B18103_032", 
      v11="B18103_035", 
      v12="B18103_038"
    ),
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE
  ) %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)%>% #pivoting
  mutate(
    total_vision_diff =v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12, #summing hearing diff by sex by age to get total population with vision diff
    share_vision_diff = total_vision_diff/total_pop)%>% #generating share of tract population with vision diff
  select(GEOID,total_pop,total_vision_diff,share_vision_diff)%>% #reducing df to necessary variables
  filter(GEOID %in% pico_tracts$GEOID) #limiting to pico tracts

#add geometry for map
st_geometry(vision_diff) <- st_geometry(pico_tracts[match(vision_diff$GEOID, pico_tracts$GEOID), ])
class(vision_diff)

#creating custom bins for a map of %pop w/ vision diff
map_vision_diff <- vision_diff%>% 
  mutate(bin_vision_diff = cut(total_vision_diff, breaks=c(0,50,100,150,99999),
                                labels  = c("Less than 50", "50-100", "100-150", "150 or more"),
                                include.lowest = TRUE))
#making each bin a factor
map_vision_diff$bin_vision_diff <- factor(map_vision_diff$bin_vision_diff, 
                                            levels = c("Less than 50", "50-100", "100-150", "150 or more"))

#plot map
plot <-ggplot()+
  geom_sf(map_vision_diff, mapping = aes(fill = bin_vision_diff), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "black", alpha = 0, lwd = 1) + # Adding the buffer zone as a transparent overlay
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = palette_urbn_cyan[c(1,3,6,8)], #can adjust the palette or color scheme as necessary
    name = "Number of individuals with a vision difficulty",
    breaks = c("Less than 50", "50-100", "100-150", "150 or more")
  )+
  theme_urbn_map()

print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/vision_diff_map_pico.png"), width = 14, height = 6, dpi = 300)




#### Irregular work hours #### - skipping for now
#pull hours worked data
hrs_worked <-
  get_acs(
    geography = "tract",
    table = "B23022",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE,
  )

#### Internet/Computer access ####
#pull internet data
internet_subs <-
  get_acs(
    geography = "tract",
    variables = c(
      total_pop = "B28002_001",
      no_int_access = "B28002_013"),
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE, #change back to TRUE to make a map
  )%>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)%>% #pivoting
  mutate(
    share_no_int = no_int_access/total_pop)%>% #generating share of tract population without internet access
  select(GEOID,total_pop,no_int_access,share_no_int)%>% #reducing df to necessary variables
  filter(GEOID %in% pico_tracts$GEOID) #limiting to pico tracts

#adding geometry for map
st_geometry(internet_subs) <- st_geometry(pico_tracts[match(internet_subs$GEOID, pico_tracts$GEOID), ])
class(internet_subs)

#creating custom bins for a map of share w/out int access
map_no_int <- internet_subs%>% 
  mutate(bin_no_int = cut(share_no_int, breaks=c(0,0.05, 0.1, 0.15,0.2,1),
                               labels  = c("Less than 5%", "5-10%", "10-15%", "15-20%", "20% or more"),
                               include.lowest = TRUE))
#making each bin a factor
map_no_int$bin_no_int <- factor(map_no_int$bin_no_int, 
                                          levels = c("Less than 5%", "5-10%", "10-15%", "15-20%", "20% or more"))

#plot map
plot <-ggplot()+
  geom_sf(map_no_int, mapping = aes(fill = bin_no_int), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "black", alpha = 0, lwd = 1) + # Adding the buffer zone as a transparent overlay
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = palette_urbn_cyan[c(1,2,4,6,8)], #can adjust the palette or color scheme as necessary
    name = "Share of households without internet access",
    breaks = c("Less than 5%", "5-10%", "10-15%", "15-20%", "20% or more")
  )+
  theme_urbn_map()

print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/internet_access_map_pico.png"), width = 14, height = 6, dpi = 300)






### pull computer access data
tech_access <-
  get_acs(
    geography = "tract",
    variables = c(
      total_pop = "B28001_001",
      no_computer = "B28001_011"
      ),
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE, #change back to TRUE to make a map
  )%>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)%>% #pivoting
  mutate(
    share_no_computer = no_computer/total_pop)%>% #generating share of tract population with different types of computer access
  select(GEOID,total_pop,no_computer,share_no_computer)%>% #reducing df to necessary variables
  filter(GEOID %in% pico_tracts$GEOID) #limiting to pico tracts

#add geometry for map
st_geometry(tech_access) <- st_geometry(pico_tracts[match(tech_access$GEOID, pico_tracts$GEOID), ])
class(tech_access)

#creating custom bins for a map of %pop w/ vision diff
map_tech_access <- tech_access%>% 
  mutate(bin_no_computer = cut(share_no_computer, breaks=c(0,0.01,0.05,0.1,1),
                               labels  = c("Less than 1%", "1-5%", "5-10%", "10% or higher"),
                               include.lowest = TRUE))
#making each bin a factor
map_tech_access$bin_no_computer <- factor(map_tech_access$bin_no_computer, 
                                          levels = c("Less than 1%", "1-5%", "5-10%", "10% or higher"))

#plot map
plot <-ggplot()+
  geom_sf(map_tech_access, mapping = aes(fill = bin_no_computer), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "black", alpha = 0, lwd = 1) + # Adding the buffer zone as a transparent overlay
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = palette_urbn_cyan[c(1,3,6,8)], #can adjust the palette or color scheme as necessary
    name = "Share of households without access to a computer",
    breaks = c("Less than 1%", "1-5%", "5-10%", "10% or higher")
  )+
  theme_urbn_map()

print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/tech_access_map_pico.png"), width = 14, height = 6, dpi = 300)

###bar chart of share of total population by device access type
tech_by_type <-
  get_acs(
    geography = "tract",
    variables = c(
      total_pop = "B28001_001",
      has_computer = "B28001_003",
      computer_only = "B28001_004",
      has_smartphone = "B28001_005",
      smartphone_only = "B28001_006",
      has_tablet = "B28001_007",
      tablet_only = "B28001_008",
      no_computer = "B28001_011"
    ),
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE
  )%>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)%>% #pivoting
  mutate(
    share_computer = has_computer/total_pop,
    share_computer_only = computer_only/total_pop,
    share_smartphone = has_smartphone/total_pop,
    share_smartphone_only = smartphone_only/total_pop,
    share_tablet = has_tablet/total_pop,
    share_tablet_only = tablet_only/total_pop,
    share_no_computer = no_computer/total_pop)%>% #generating share of tract population with different types of computer access
  select(GEOID,total_pop,no_computer, has_computer, computer_only,
         has_smartphone, smartphone_only, has_tablet, tablet_only,)%>% #reducing df to necessary variables
  filter(GEOID %in% pico_tracts$GEOID) #limiting to pico tracts

#making separate df for bar chart  
tech_sums <- as.data.frame(t(colSums(tech_by_type[, 2:9]))) %>%
  mutate(
    share_computer = has_computer/total_pop,
    share_computer_only = computer_only/total_pop,
    share_smartphone = has_smartphone/total_pop,
    share_smartphone_only = smartphone_only/total_pop,
    share_tablet = has_tablet/total_pop,
    share_tablet_only = tablet_only/total_pop,
    share_no_computer = no_computer/total_pop)
tech_sums_long <- tech_sums %>% #selecting columns we want
  select(share_computer, share_smartphone, share_tablet, share_no_computer) %>%
  pivot_longer(cols = everything(), names_to = "device_type", values_to = "share") 

# Create the bar chart
ggplot(tech_sums_long, aes(x = device_type, y = share)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Share of Total Population by Device Access Type",
       x = "Device Access Type",
       y = "Share of Total Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###Housing & Displacement###

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
  geometry = FALSE,
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
    geometry = FALSE,
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
    geometry = FALSE,
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
    geometry = FALSE,
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
    geometry = FALSE,
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
    geometry = FALSE,
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




