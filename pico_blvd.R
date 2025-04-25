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

##census tracts and shapefiles##
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
  geometry = TRUE
)

#save
write.csv(sex_by_age, file.path(file_path, "Fileshare", "sex_by_age.csv"), row.names = FALSE)

#restructure so that each row is a census tract
pop_wide_pico <- sex_by_age %>%
  select(GEOID, variable, estimate, geometry) %>%
  filter(variable == "B01001_001", GEOID %in% pico_tracts$GEOID) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  rename_with(~ "population", .cols = "B01001_001")

#add census tracts
pop_wide_pico <- left_join(pico_tracts, st_drop_geometry(pop_wide_pico), by = "GEOID")

#for data - get the total population
totalpop <- sum(pop_wide_pico$population)

#create a histogram
population_histogram_pico <- ggplot(pop_wide_pico, aes(x = population)) +
  geom_histogram(binwidth = 500) +
  labs(
    title = NULL,
    x = "Population",
    y = "Census Tracts"
  )

#save
ggsave(file.path(file_path, "Pico/Outputs/population_histogram_pico.png"), width = 8, height = 6, dpi = 300)

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
  st_drop_geometry() %>%
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
  
#save
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

#create a barchart
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

#save
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

#save
write.csv(median_income, file.path(file_path, "Fileshare", "median_income.csv"), row.names = FALSE)

##Map of median incomes by Census Tract

#edit the table so that each row is a census tract
income_wide_pico <- median_income %>%
  filter(variable == "B19013_001", GEOID %in% pico_tracts$GEOID) %>%
  mutate(income = estimate) %>%
  select(GEOID, income, NAME, moe, geometry)

#create a map based on incomes
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

#save
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

#save
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
    geometry = FALSE,
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

#save
ggsave(file.path(file_path, "Pico/Outputs/employment_bar_chart.png"), width = 14, height = 6, dpi = 300)


##Gender##
#see sex_by_age

#pivot to take Total Male and Total Female population
gender__wide_pico <- sex_by_age %>%
  filter(
    variable %in% c("B01001_001", "B01001_002", "B01001_026"),
    GEOID %in% pico_tracts$GEOID
  ) %>%
  select(GEOID, variable, estimate) %>%
  group_by(GEOID) %>%
  summarise(
    Total = sum(estimate[variable == "B01001_001"], na.rm = TRUE),
    Male = sum(estimate[variable == "B01001_002"], na.rm = TRUE),
    Female = sum(estimate[variable == "B01001_026"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    sex_by_age %>% select(GEOID, NAME) %>% distinct(),
    by = "GEOID"
  ) %>%
  mutate(
    share_male = Male / Total,
    share_female = Female / Total
  ) %>%
  #Add total row
  bind_rows(
    summarise(
      .,
      GEOID = "Total",
      NAME = "Total",
      Total = sum(Total, na.rm = TRUE),
      Male = sum(Male, na.rm = TRUE),
      Female = sum(Female, na.rm = TRUE),
      share_male = sum(Male, na.rm = TRUE) / sum(Total, na.rm = TRUE),
      share_female = sum(Female, na.rm = TRUE) / sum(Total, na.rm = TRUE)
    )
  )

#create figure
gender_bar_chart_pico <- ggplot(
  tibble(
    status = c("Male", "Female"),
    share = c(
      gender__wide_pico$share_male[gender__wide_pico$GEOID == "Total"],
      gender__wide_pico$share_female[gender__wide_pico$GEOID == "Total"]
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
  scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.1))) +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(size = 16),  
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(),  
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank()  
  )

#save
ggsave(file.path(file_path, "Pico/Outputs/gender_bar_chart_pico.png"), width = 14, height = 6, dpi = 300)


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

#save
write.csv(family_structure, file.path(file_path, "Fileshare", "family_structure.csv"), row.names = FALSE)

#pivot to get descriptive stats
family_bins <- list(
  married_children = "B11003_003",
  married_no_children = "B11003_007",
  single_children = c("B11003_010", "B11003_016"),
  single_no_children = c("B11003_014", "B11003_020")
)


#summarise total by family structure, for each tract
# Pivot family_structure data wider
family_wide_pico <- family_structure %>%
  filter(variable %in% unlist(family_bins), GEOID %in% pico_tracts$GEOID) %>%
  mutate(family_structure = case_when(
    variable %in% family_bins$married_children ~ "Married with children under 18",
    variable %in% family_bins$married_no_children ~ "Married without children under 18",
    variable %in% family_bins$single_children ~ "Single parent with children under 18",
    variable %in% family_bins$single_no_children ~ "Single parent without children under 18",
    TRUE ~ NA_character_ 
  )) %>%
    filter(!is.na(family_structure)) %>%
  group_by(GEOID, family_structure) %>%
  summarise(population = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
  # Pivot the data wider to have family types as columns
  pivot_wider(names_from = family_structure, values_from = population, values_fill = list(population = 0)) %>%
    left_join(select(family_structure, GEOID, NAME) %>% distinct(), by = "GEOID")


#calculate the totals 
family_totals <- family_wide_pico %>%
  summarise(across(starts_with("Married"), ~sum(.x, na.rm = TRUE)), 
            across(starts_with("Single"), ~sum(.x, na.rm = TRUE))) %>%
  # Reshape the data to a long format for easier analysis
  pivot_longer(
    cols = everything(),
    names_to = "family_structure",
    values_to = "population"
  )


#create a bar chart
pop_by_family_type_pico <- ggplot(family_totals, aes(x = family_structure, y = population, fill = family_structure)) +
  geom_col() +
  geom_text(aes(label = scales::comma(population)), vjust = -0.5, size = 5) +
  labs(
    x = NULL,
    y = "Population"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 16)) + 
  theme(legend.position = "none") +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

#save
ggsave(file.path(file_path, "Pico/Outputs/pop_by_family_type_pico.png"), width = 14, height = 6, dpi = 300)

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

#save
write.csv(language, file.path(file_path, "Fileshare", "language.csv"), row.names = FALSE)

#create bins based on the table
language_bins <- list(
  very_well = c("B16004_005", "B16004_010", "B16004_015", "B16004_020", "B16004_027", "B16004_032", "B16004_037", "B16004_042", "B16004_049", "B16004_054", "B16004_059", "B16004_064"),
  well = c("B16004_006", "B16004_011", "B16004_016", "B16004_021", "B16004_028", "B16004_033", "B16004_038", "B16004_043", "B16004_050", "B16004_055", "B16004_060", "B16004_065"),
  not_well = c("B16004_007", "B16004_012", "B16004_017", "B16004_022", "B16004_029", "B16004_034", "B16004_039", "B16004_044", "B16004_051", "B16004_056", "B16004_061", "B16004_066"),
  none =c("B16004_008", "B16004_013", "B16004_018", "B16004_023", "B16004_030", "B16004_035", "B16004_040", "B16004_045", "B16004_052", "B16004_057", "B16004_062", "B16004_067")
)

#summarise total by language, for each tract
# Pivot  data wider
language_wide_pico <- language %>%
  filter(variable %in% unlist(language_bins), GEOID %in% pico_tracts$GEOID) %>%
  mutate(language_skill = case_when(
    variable %in% language_bins$very_well ~ "Very well",
    variable %in% language_bins$well ~ "Well",
    variable %in% language_bins$not_well ~ "Not well",
    variable %in% language_bins$none ~ "Not at all",
    TRUE ~ NA_character_  
  )) %>%
  filter(!is.na(language_skill)) %>%
  group_by(GEOID, language_skill) %>%
  summarise(population = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
  # Pivot the data wider to have language skills as columns
  pivot_wider(names_from = language_skill, values_from = population, values_fill = list(population = 0)) %>%
  left_join(select(language, GEOID, NAME) %>% distinct(), by = "GEOID")

#calculate totals for bar chart
language_totals <- language_wide_pico %>%
  select(-GEOID, -NAME) %>%
  summarise(across(everything(), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "language_skill",
    values_to = "population"
  )

#reorder so that the bars show up in the order we want
language_totals$language_skill <- factor(
  language_totals$language_skill,
  levels = c("Very well", "Well", "Not well", "Not at all")
)

#create a bar chart
pop_by_language_skill_pico <- ggplot(language_totals, aes(x = language_skill, y = population, fill = language_skill)) +
  geom_col() +
  geom_text(aes(label = scales::comma(population)), vjust = -0.5, size = 5) +
  labs(
    x = NULL,
    y = "Population"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 16)) + 
  theme(legend.position = "none") +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank()
  )

#save
ggsave(file.path(file_path, "Pico/Outputs/pop_by_language_skill_pico.png"), width = 14, height = 6, dpi = 300)

#repeat the above but calculating percentages
# Calculate percentages for each language skill category
language_totals_pct <- language_wide_pico %>%
  select(-GEOID, -NAME) %>%
  summarise(across(everything(), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "language_skill",
    values_to = "population"
  ) %>%
  mutate(
    percent = population / sum(population)
  )

# Reorder the factor levels
language_totals_pct$language_skill <- factor(
  language_totals_pct$language_skill,
  levels = c("Very well", "Well", "Not well", "Not at all")
)

# Plot as a percentage bar chart
pop_by_language_skill_pct_pico <- ggplot(language_totals_pct, aes(x = language_skill, y = percent, fill = language_skill)) +
  geom_col() +
  geom_text(aes(label = scales::percent(percent, accuracy = 1)), vjust = -0.5, size = 5) +  # Percent labels
  labs(
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(labels = NULL, breaks = NULL, expand = expansion(mult = c(0, 0.1))) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 16)) + 
  theme(legend.position = "none") +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

#save
ggsave(file.path(file_path, "Pico/Outputs/pop_by_language_skill_pct_pico.png"), width = 14, height = 6, dpi = 300)


##Other languages spoken in the area##
#see language

#create bins for each language spoken
language_bins_1 <- list(
  total = "B16004_001",
  only_english = c("B16004_003", "B16004_025", "B16004_047"),
  spanish = c("B16004_004", "B16004_026", "B16004_048"),
  other_indo_euro = c("B16004_009", "B16004_031", "B16004_053"),
  asian_pac_island =c("B16004_014", "B16004_036", "B16004_058"),
  other_language = c("B16004_19", "B16004_041", "B16004_063")
)

#summarise total by language, for each tract
# Pivot  data wider
spoken_language_wide_pico <- language %>%
  filter(variable %in% unlist(language_bins_1), GEOID %in% pico_tracts$GEOID) %>%
  mutate(language_spoken = case_when(
    variable %in% language_bins_1$total ~ "Total",
    variable %in% language_bins_1$only_english ~ "English Only",
    variable %in% language_bins_1$spanish ~ "Spanish",
    variable %in% language_bins_1$other_indo_euro ~ "Other Indo-European Languages",
    variable %in% language_bins_1$asian_pac_island ~ "Asian and Pacific Island Languages",
    variable %in% language_bins_1$other_language ~ "Other Languages",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(language_spoken)) %>%
  group_by(GEOID, language_spoken) %>%
  summarise(population = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
  # Pivot wider to get one column per language
  pivot_wider(names_from = language_spoken, values_from = population, values_fill = list(population = 0)) %>%
  mutate(
    share_english_only = `English Only` / Total,
    share_spanish = Spanish / Total,
    share_other_indo_euro = `Other Indo-European Languages` / Total,
    share_asian_pac_island = `Asian and Pacific Island Languages` / Total,
    share_other_language = `Other Languages` / Total
  ) %>%
  left_join(select(language, GEOID, NAME) %>% distinct(), by = "GEOID")

#calculate totals for bar chart
language_spoken_totals <- spoken_language_wide_pico %>%
  select(-GEOID, -NAME) %>%
  summarise(across(everything(), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "language_spoken",
    values_to = "population"
  )

#reorder so that the bars show up in the order we want
language_spoken_totals$language_spoken <- factor(
  language_spoken_totals$language_spoken,
  levels = c("English Only", "Spanish", "Other Indo-European Languages", "Asian and Pacific Island Languages", "Other Languages")
)

#create a bar chart
pop_by_language_spoken_pico <- ggplot(language_spoken_totals, aes(x = language_spoken, y = population, fill = language_spoken)) +
  geom_col() +
  geom_text(aes(label = scales::comma(population)), vjust = -0.5, size = 5) +
  labs(
    x = NULL,
    y = "Population"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 16)) + 
  theme(legend.position = "none") +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

#save
ggsave(file.path(file_path, "Pico/Outputs/pop_by_language_spoken_pico.png"), width = 14, height = 6, dpi = 300)

#replicate in case we want percentages
# Calculate percentages for each language spoken category
language_spoken_totals_pct <- spoken_language_wide_pico %>%
  select(-GEOID, -NAME) %>%
  summarise(across(everything(), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "language_spoken",
    values_to = "population"
  ) %>%
  mutate(
    percent = population / sum(population)
  )

# Reorder the factor levels
language_spoken_totals_pct$language_spoken <- factor(
  language_spoken_totals_pct$language_spoken,
  levels = c("English Only", "Spanish", "Other Indo-European Languages", "Asian and Pacific Island Languages", "Other Languages")
)

# Plot as a percentage bar chart
pop_by_language_spoken_pct_pico <- ggplot(language_spoken_totals_pct, aes(x = language_spoken, y = percent, fill = language_spoken)) +
  geom_col() +
  geom_text(aes(label = scales::percent(percent, accuracy = 1)), vjust = -0.5, size = 5) +
  labs(
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(labels = NULL, breaks = NULL, expand = expansion(mult = c(0, 0.1))) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 16)) + 
  theme(legend.position = "none") +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

#save
ggsave(file.path(file_path, "Pico/Outputs/pop_by_language_spoken_pct_pico.png"), width = 14, height = 6, dpi = 300)

#create a map
spoken_language_wide_pico_sf <- spoken_language_wide_pico %>%
  left_join(select(pico_tracts, GEOID, geometry), by = "GEOID") %>%
  st_as_sf()

spanish_map_pico <- ggplot(spoken_language_wide_pico_sf) +
  geom_sf(aes(fill = share_spanish), show.legend = TRUE) +
  #add the buffer overlay
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0.2, lwd = 1) +
  #add the street overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.3) +
  scale_fill_gradientn(
    colors = palette_urbn_magenta[c(1, 3, 5, 7)], 
    name = NULL,
    labels = scales::percent_format()
  ) +
  theme_urbn_map()+  
  theme(
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.key.height = unit(1.2, "cm"),
    legend.key.width = unit(0.6, "cm")
  )  

#save
ggsave(file.path(file_path, "Pico/Outputs/spanish_map_pico.png"), width = 14, height = 6, dpi = 300)


api_map_pico <- ggplot(spoken_language_wide_pico_sf) +
  geom_sf(aes(fill = share_asian_pac_island), show.legend = TRUE) +
  #add the buffer overlay
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0.2, lwd = 1) +
  #add the street overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.3) +
  scale_fill_gradientn(
    colors = palette_urbn_magenta[c(2, 4, 6, 8)],
    name = NULL,
    labels = scales::percent_format()
  ) +
  theme_urbn_map()+  
  theme(
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.key.height = unit(1.2, "cm"),
    legend.key.width = unit(0.6, "cm")
  )  

#save
ggsave(file.path(file_path, "Pico/Outputs/api_map_pico.png"), width = 14, height = 6, dpi = 300)


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

#create a table that generates the totals for each country of origin variable to see manually which countries have the highest populations


# country_of_origin_totals <- country_origin %>%
#   filter(GEOID %in% pico_tracts$GEOID) %>%
#   group_by(variable) %>%
#   summarise(total = sum(estimate, na.rm = TRUE), .groups = "drop")

#after reviewing the table above, edit to manually pull out the highest (in this case I chose the top countries with more than 1,000). This is done manually because some of the variables include 'Total', 'Asia', 'Central America', etc.
country_origin_filtered_pico <- country_origin %>%
  filter(
    GEOID %in% pico_tracts$GEOID
  ) %>%
  mutate(country = case_when(
    variable == "B05006_158" ~ "Guatemala",
    variable == "B05006_157" ~ "El Salvador",
    variable == "B05006_160" ~ "Mexico",
    variable == "B05006_054" ~ "Korea",
    variable == "B05006_049" ~ "China",
    variable == "B05006_074" ~ "Phillipnes",
    variable == "B05006_159" ~ "Honduras",
  )) %>%
  group_by(country) %>%
  summarise(total = sum(estimate, na.rm = TRUE), .groups = "drop")

write.csv(country_origin_filtered_pico, file.path(file_path, "Fileshare", "country_origin_filtered_pico.csv"), row.names = FALSE)


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

##Collisions

#load in data from TIMS
crashes_pico <- read.csv(file.path(file_path, "Pico/Outputs/crash_data_pico_blvd.csv"))

#convert to an SF
crashes_pico_sf <- st_as_sf(
  crashes_pico,
  coords = c("POINT_X", "POINT_Y"),
  crs = 4326,  # EPSG:4326 = WGS 84 (standard lat/lon)
  remove = FALSE  # keeps Point_X and Point_Y columns too
)

#create map with points overlaid
crash_map_pico <- ggplot() +
  geom_sf(data = pico_buffer_tracts, fill = "lightgray", color = "gray40", size = 0.3) +
  geom_sf(data = major_streets_clipped, color = "orange", size = 0.6) +
  geom_sf(data = crashes_pico_sf, color = "red", size = 1, alpha = 0.7) +
  # labs(title = "Crash Locations in Pico Area",
  #      subtitle = "Overlaid on Buffer Tracts and Major Streets",
  #      caption = "Source: TIMS") +
  theme_urbn_map()+
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12)
  )

#save
ggsave(file.path(file_path, "Pico/Outputs/crash_map_pico.png"), width = 14, height = 6, dpi = 300)

