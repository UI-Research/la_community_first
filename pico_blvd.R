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
#used for charts
library(stringr)
#used for maps
library(geofacet)
#used for shapefile functions
library(sf)

#used for street data
library(osmdata)

# options(tigris_use_cache = TRUE)


#used to pull urban theme template
library(urbnthemes)
options(scipen=999)
set_urbn_defaults(style="print")


####Load and Clean####
#set personal file path to make it easier to pull data

#Gabe
file_path <- file.path("C:/Users/GSamuels/Box/LA Transit project/Social Climate Analysis")

#load in data dictionary for acs
v23 <- load_variables(2023, "acs5", cache = TRUE)
write.csv(v23, file.path(file_path, "Fileshare", "census_variables.csv"), row.names = FALSE)

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
#create specific set of streets we want to see
bbox <- st_bbox(pico_tracts)
#load in
streets <- opq(bbox = bbox) %>%
  add_osm_feature(key = "highway") %>%  # 'highway' includes roads, streets, etc.
  osmdata_sf()

#extract just streets
streets_lines <- streets$osm_lines

#filter to main streets
major_streets <- streets_lines %>%
  filter(highway %in% c("primary", "secondary", "tertiary")) %>%
  st_transform(st_crs(pico_tracts))

#create a version that clips the streets to the focus area
major_streets_clipped <- st_intersection(major_streets, st_union(st_geometry(pico_tracts)))


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

write.csv(sex_by_age, file.path(file_path, "Fileshare", "sex_by_age.csv"), row.names = FALSE)

#restructure so that each row is a census tract
##ADD geometry##
pop_wide_pico <- sex_by_age %>%
  select(GEOID, variable, estimate, moe) %>%
  filter(variable == "B01001_001", GEOID %in% pico_tracts$GEOID)%>%
  pivot_wider(names_from = variable, values_from = estimate)%>%
  rename(population = B01001_001)


#add census tracts
pop_wide_pico <- left_join(pico_tracts, pop_wide_pico, by = "GEOID")

#confirm that this is a shapefile
sf::st_geometry(age_wide_pico)

#data - get the total population
totalpop <- sum(age_wide_pico$population)

#create a histogram
population_histogram_pico <- ggplot(pop_wide_pico, aes(x = population)) +
  geom_histogram(binwidth = 500) +
  labs(
    title = NULL,
    x = "Population",
    y = "Census Tracts"
  )

ggsave(file.path(file_path, "Pico/Outputs/population_histogram_pico.png"), width = 8, height = 6, dpi = 300)

#add geometry from census tracts to create a map

#create map of population
population_map_pico <- ggplot(pop_wide_pico) +
  geom_sf(aes(fill = population), show.legend = TRUE) +
  #add the buffer overlay
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0.2, lwd = 1) +
  #add the street overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.3) +
  scale_fill_gradientn(
    colors = palette_urbn_cyan[c(2, 4, 6, 7)],  # Selecting a subset of colors
    name = "Population by Census Tract",
    labels = scales::comma
  ) +
  theme_urbn_map()+  
  theme(
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.key.height = unit(1.2, "cm"),
    legend.key.width = unit(0.6, "cm")
  )  


ggsave(file.path(file_path, "Pico/Outputs/population_map_pico.png"), width = 14, height = 6, dpi = 300)


##Age##
#see sex_by_age

#create bins of different age groups, based on census data dictionary (v23)
age_bins <- list(
  age_0_17  = c("B01001_003", "B01001_004", "B01001_005", "B01001_006",
                "B01001_027", "B01001_028", "B01001_029", "B01001_030"),
  age_18_29 = c("B01001_007", "B01001_008", "B01001_009", "B01001_010", "B01001_011",  
                "B01001_031", "B01001_032", "B01001_033","B01001_034", "B01001_035"),
  age_30_44 = c("B01001_012","B01001_013", "B01001_014", 
                "B01001_036", "B01001_037", "B01001_038"),
  age_45_59 = c("B01001_015", "B01001_016", "B01001_017", 
                "B01001_039", "B01001_040", "B01001_041"),
  age_60_up = c("B01001_018", "B01001_019","B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
 "B01001_041", "B01001_042", "B01001_043", "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049")
)


#summarise total by age, for each tract
age_wide_pico <- sex_by_age %>%
  filter(variable %in% unlist(age_bins), GEOID %in% pico_tracts$GEOID)%>%
  group_by(GEOID)%>%
  mutate(age_group = case_when(
    variable %in% age_bins$age_0_17 ~ "age_0_17",
    variable %in% age_bins$age_18_29 ~ "age_18_29",
    variable %in% age_bins$age_30_44 ~ "age_30_44",
    variable %in% age_bins$age_45_59 ~ "age_45_59",
    variable %in% age_bins$age_60_up ~ "age_60_up",
  )) %>%
  group_by(GEOID, age_group)%>%
  summarise(population = sum(estimate, na.rm = TRUE), .groups = "drop")%>%
  pivot_wider(names_from = age_group, values_from = population)

#calculate the total age 
age_totals <- age_wide_pico %>%
  summarise(across(starts_with("age_"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "age_group",
    values_to = "population"
  )

#create a bar chart
pop_by_age_group <- ggplot(age_totals, aes(x = age_group, y = population, fill = age_group)) +
  geom_col() +
  labs(
    x = "Age Group",
    y = "Population"
  ) +
  scale_x_discrete(
    labels = c(
      "age_0_17" = "0-17", 
      "age_18_29" = "18-29", 
      "age_30_44" = "30-44",
      "age_45_59" = "45-59",
      "age_60_up" = "60+"
    ))+
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "none")+
  theme(
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text.x = element_text(size = 16),
  axis.text.y = element_text(size = 16))
  

ggsave(file.path(file_path, "Pico/Outputs/age_chart_pico.png"), width = 14, height = 6, dpi = 300)


##Race and Ethnicity##

#decided to remove race for now, since ethnicity captures everything we are looking for

# #pull race  from ACS
# race <- get_acs(
#   geography = "tract",
#   table = "B02001",
#   state = state_fips,
#   county = county_fips,
#   year = 2023,
#   survey = "acs5",
#   geometry = FALSE
# )
# 
# write.csv(race, file.path(file_path, "Fileshare", "race.csv"), row.names = FALSE)
# 
# race_wide_pico <- race %>%
#   filter(GEOID %in% pico_tracts$GEOID,
#          variable %in% c("B02001_001",  # Total pop
#                          "B02001_002",  # White
#                          "B02001_003",  # Black
#                          "B02001_004",  # Native American
#                          "B02001_005",  # Asian
#                          "B02001_009"   # Other
#          )) %>%
#   select(GEOID, variable, estimate) %>%
#   pivot_wider(
#     names_from = variable,
#     values_from = estimate,
#     names_prefix = "race_"
#   ) %>%
#   rename(
#     total = race_B02001_001,
#     white = race_B02001_002,
#     black = race_B02001_003,
#     native_am = race_B02001_004,
#     asian = race_B02001_005,
#     other = race_B02001_009
#   ) %>%
#   mutate(
#     pct_white = white / total * 100,
#     pct_black = black / total * 100,
#     pct_native_am = native_am / total * 100,
#     pct_asian = asian / total * 100,
#     pct_other = other / total * 100
#   ) %>%
#   bind_rows(
#     summarise(., 
#               GEOID = "Total",
#               total = sum(total, na.rm = TRUE),
#               white = sum(white, na.rm = TRUE),
#               black = sum(black, na.rm = TRUE),
#               native_am = sum(native_am, na.rm = TRUE),
#               asian = sum(asian, na.rm = TRUE),
#               other = sum(other, na.rm = TRUE),
#               pct_white = white / total * 100,
#               pct_black = black / total * 100,
#               pct_native_am = native_am / total * 100,
#               pct_asian = asian / total * 100,
#               pct_other = other / total * 100)
#   )


#pull ethnicity  from ACS
ethnicity <- 
  get_acs(
    geography = "tract",
    table = "B03002",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE
  )

write.csv(ethnicity, file.path(file_path, "Fileshare", "ethnicity.csv"), row.names = FALSE)

#reformat and calculate percentages and totals
ethnicity_wide_pico <- ethnicity %>%
  filter(GEOID %in% pico_tracts$GEOID,
         variable %in% c("B03002_001",  # Total pop
                         "B03002_003",  # White
                         "B03002_004",  # Black
                         "B03002_005",  # American Indian
                         "B03002_006",  # Asian
                         "B03002_007",  # Hawaiian and Pacific Islander
                         "B03002_010",  # Other
                         "B03002_012"   # Hispanic
         )) %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(
    names_from = variable,
    values_from = estimate,
    names_prefix = "eth_"
  ) %>%
  rename(
    total = eth_B03002_001,
    white = eth_B03002_003,
    black = eth_B03002_004,
    native_am = eth_B03002_005,
    asian = eth_B03002_006,
    hawaiian_pac = eth_B03002_007,
    other = eth_B03002_010,
    hispanic = eth_B03002_012
  ) %>%
  mutate(
    native_hawaiian = native_am + hawaiian_pac,
    pct_white = white / total * 100,
    pct_black = black / total * 100,
    pct_native_hawaiian = native_hawaiian / total * 100,
    pct_asian = asian / total * 100,
    pct_other = other / total * 100,
    pct_hispanic = hispanic / total * 100
  ) %>%
  bind_rows(
    summarise(.,
              GEOID = "Total",
              total = sum(total, na.rm = TRUE),
              white = sum(white, na.rm = TRUE),
              black = sum(black, na.rm = TRUE),
              native_am = sum(native_am, na.rm = TRUE),
              hawaiian_pac = sum(hawaiian_pac, na.rm = TRUE),
              native_hawaiian = native_am + hawaiian_pac,
              asian = sum(asian, na.rm = TRUE),
              other = sum(other, na.rm = TRUE),
              hispanic = sum(hispanic, na.rm = TRUE),
              pct_white = white / total * 100,
              pct_black = black / total * 100,
              pct_native_hawaiian = native_hawaiian / total * 100,
              pct_asian = asian / total * 100,
              pct_other = other / total * 100,
              pct_hispanic = hispanic / total * 100)
  )

#reformat for ggplot
ethnicity_long <- ethnicity_wide_pico %>%
  filter(GEOID == "Total") %>%
  select(starts_with("pct_")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "group",
    values_to = "percent"
  ) %>%
  mutate(
    group = recode(group,
                   pct_white = "White",
                   pct_black = "Black",
                   pct_native_hawaiian = "Native American, Alaska Native, Native Hawaiiwan, and other Pacific Islander",
                   pct_asian = "Asian",
                   pct_other = "Other",
                   pct_hispanic = "Hispanic")
  )

ethnicity_chart_pico <- ggplot(ethnicity_long, aes(x = reorder(group, -percent), y = percent, fill = group)) +
  geom_bar(stat = "identity") +
  labs(
    title = NULL,
    x = NULL,
    y = "Percent of Corridor"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 16)) + 
  theme(legend.position = "none")+
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16))

ggsave(file.path(file_path, "Pico/Outputs/ethnicity_chart_pico.png"), width = 14, height = 6, dpi = 300)


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

write.csv(median_income, file.path(file_path, "Fileshare", "median_income.csv"), row.names = FALSE)

##Map of median incomes by Census Tract

#edit the table so that each row is a census tract
income_wide_pico <- median_income %>%
  filter(variable == "B19013_001", GEOID %in% pico_tracts$GEOID) %>%
  mutate(income = estimate) %>%
  select(GEOID, income, NAME, moe, geometry)

income_map_pico <- ggplot(income_wide_pico) +
  geom_sf(aes(fill = income), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0.2, lwd = 1) +
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.3) +
  scale_fill_gradientn(
    colors = palette_urbn_green[c(1,2,4,6,8)],
    name = "Median Income by Census Tract",
    labels = scales::label_dollar(scale_cut = scales::cut_short_scale())
  ) +
  theme_urbn_map() +
  theme(
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.key.height = unit(1.2, "cm"),
    legend.key.width = unit(0.6, "cm")
  )

ggsave(file.path(file_path, "Pico/Outputs/income_map_pico.png"), width = 14, height = 6, dpi = 300)


#pull poverty level from ACS
poverty_level <-
  get_acs(
    geography = "tract",
    table = "B17001",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = TRUE
    )


write.csv(poverty_level, file.path(file_path, "Fileshare", "poverty_level.csv"), row.names = FALSE)

#edit table so that each row is a census tract and we can calculate the share
poverty_wide_pico <- poverty_level %>%
  filter(variable %in% c("B17001_001", "B17001_002"),
         GEOID %in% pico_tracts$GEOID) %>%
  select(GEOID, variable, estimate, geometry) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(
    pct_below_pov = 100 * B17001_002 / B17001_001,
    pov_bin = case_when(
      pct_below_pov < 10 ~ "Less than 10%",
      pct_below_pov >= 10 & pct_below_pov < 20 ~ "10–20%",
      pct_below_pov >= 20 & pct_below_pov < 30 ~ "20–30%",
      pct_below_pov >= 30 ~ "30% or more",
      TRUE ~ NA_character_
    ),
    pov_bin = factor(pov_bin, levels = c("Less than 10%", "10–20%", "20–30%", "30% or more"))
  )%>%
  rename(
    total_pop = B17001_001,
    below_pov = B17001_002
  ) %>%
  select(GEOID, total_pop, below_pov, pct_below_pov, pov_bin, geometry)

sum(poverty_wide_pico$below_pov)

total_share_below_pov <- sum(poverty_wide_pico$below_pov, na.rm = TRUE) / sum(poverty_wide_pico$total_pop, na.rm = TRUE)

#Map of tracts by poverty level status
poverty_map_pico <- ggplot(poverty_wide_pico) +
  geom_sf(aes(fill = pov_bin), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0.2, lwd = 1) +
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.3) +
  scale_fill_manual(
    values = c(
      "Less than 10%" = "#d0f0c0",
      "10–20%" = "#a1d99b",
      "20–30%" = "#74c476",
      "30% or more" = "#238b45"
    ),
    name = "Share Below Poverty",
    drop = FALSE
  ) +
  theme_urbn_map() +
  theme(
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.key.height = unit(1.2, "cm"),
    legend.key.width = unit(0.6, "cm")
  )

ggsave(file.path(file_path, "Pico/Outputs/poverty_map_pico.png"), width = 14, height = 6, dpi = 300)

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

write.csv(employment_status, file.path(file_path, "Fileshare", "employment_status.csv"), row.names = FALSE)

#create a new table that lists the employment status (total and share) for each tract
employment_wide_pico <- employment_status %>%
  filter(GEOID %in% pico_tracts$GEOID) %>%
  mutate(employment_stat = case_when(
    variable == "B23025_001" ~ "total",
    variable == "B23025_002" ~ "in_labor_force",
    variable == "B23025_007" ~ "out_labor_force",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(employment_stat)) %>%
  select(GEOID, employment_stat, estimate, NAME) %>%
  pivot_wider(names_from = employment_stat, values_from = estimate) %>%
  mutate(
    share_in_labor_force = in_labor_force / total,
    share_out_labor_force = out_labor_force / total
  )

#calculate the totals separately
employment_total_row <- employment_wide_pico %>%
  summarise(
    GEOID = "Total",
    total = sum(total, na.rm = TRUE),
    in_labor_force = sum(in_labor_force, na.rm = TRUE),
    out_labor_force = sum(out_labor_force, na.rm = TRUE)
  ) %>%
  mutate(
    share_in_labor_force = in_labor_force / total,
    share_out_labor_force = out_labor_force / total
  )

#bind to the original table
employment_wide_pico <- bind_rows(employment_wide_pico, employment_total_row)

#create a bar chart
employment_bar_chart <- ggplot(
  tibble::tibble(
    status = c("Employed", "Unemployed"),
    share = c(
      employment_wide_pico$share_in_labor_force[employment_wide_pico$GEOID == "Total"],
      employment_wide_pico$share_out_labor_force[employment_wide_pico$GEOID == "Total"]
    )
  ),
  aes(x = status, y = share, fill = status)
) +
  geom_col() +
  geom_text(aes(label = scales::percent(share, accuracy = 1)), vjust = -0.5, size = 6) +
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  scale_y_continuous(labels = NULL, breaks = NULL, expand = expansion(mult = c(0, 0.1))) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 16),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

ggsave(file.path(file_path, "Pico/Outputs/employment_bar_chart.png"), width = 14, height = 6, dpi = 300)


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
    geometry = FALSE,
  )

write.csv(family_structure, file.path(file_path, "Fileshare", "family_structure.csv"), row.names = FALSE)

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
    geometry = FALSE,
  )

write.csv(language, file.path(file_path, "Fileshare", "language.csv"), row.names = FALSE)


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

write.csv(country_origin, file.path(file_path, "Fileshare", "country_origin.csv"), row.names = FALSE)


###Other Accessibility Considerations### - Teddy

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
    geometry = FALSE,
  )

write.csv(disability, file.path(file_path, "Fileshare", "disability.csv"), row.names = FALSE)


#pull hearing difficulty status
hearing_diff <-
  get_acs(
    geography = "tract",
    table = "B18102",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE,
  )

write.csv(hearing_diff, file.path(file_path, "Fileshare", "hearing_diff.csv"), row.names = FALSE)


#pull vision difficulty status
vision_diff <-
  get_acs(
    geography = "tract",
    table = "B18103",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE,
  )

write.csv(vision_diff, file.path(file_path, "Fileshare", "vision_diff.csv"), row.names = FALSE)


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
    geometry = FALSE,
  )

write.csv(hrs_worked, file.path(file_path, "Fileshare", "hrs_worked.csv"), row.names = FALSE)


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
    geometry = FALSE,
  )

write.csv(hrs_worked, file.path(file_path, "Fileshare", "internet_subs.csv"), row.names = FALSE)


#pull computer access data
tech_access <-
  get_acs(
    geography = "tract",
    table = "B28001",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE,
  )

write.csv(tech_access, file.path(file_path, "Fileshare", "tech_access.csv"), row.names = FALSE)


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
  geometry = FALSE,
)

write.csv(tenure, file.path(file_path, "Fileshare", "tenure.csv"), row.names = FALSE)


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

write.csv(renter_burden, file.path(file_path, "Fileshare", "renter_burden.csv"), row.names = FALSE)


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

write.csv(owner_burden, file.path(file_path, "Fileshare", "owner_burden.csv"), row.names = FALSE)


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

write.csv(vehicles, file.path(file_path, "Fileshare", "vehicles.csv"), row.names = FALSE)

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

write.csv(transportation_means, file.path(file_path, "Fileshare", "transportation_means.csv"), row.names = FALSE)


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

write.csv(travel_time, file.path(file_path, "Fileshare", "travel_time.csv"), row.names = FALSE)


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




