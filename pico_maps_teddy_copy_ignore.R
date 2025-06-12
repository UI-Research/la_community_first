#teddy's copy of Gabe's tmap code

## WCG: IMO, it's good to load tidyverse first--it's your workhorse for everyday tasks
## then load other packages thereafter because some will intentionally mask tidyverse functions
## note that I've removed things like tidyr, ggplot, etc.--these are contained within the tidyverse
## I'd also vote against commenting packages unless they're lesser-used ones, just for readability
library(tidyverse)
library(tmap)
library(cols4all)
library(tidycensus)
library(readxl)
library(openxlsx)
library(geofacet)
library(sf)
library(scales)
library(osmdata)

username = getwd() %>% str_split("\\/") %>% unlist %>% .[3]
file_path <- file.path("C:", "Users", username, "Box", "LA Transit project/Social Climate Analysis")

#load in data dictionary for acs
v23 <- load_variables(2023, "acs5", cache = TRUE)
# write.csv(v23, file.path(file_path, "Fileshare", "census_variables.csv"), row.names = FALSE)

####----Geographic Data----####

#pico tracts
pico_buffer_tracts <- st_read(file.path(file_path, "Pico/Maps/pico_buffer_tracts.shp"))

pico_buffer_outline <- st_union(pico_buffer_tracts) |> st_as_sf()

# Load external shapefiles
CA_tracts <- st_read(file.path(file_path, "Hoover/Mapping/CA_census_Tracts.shp"), quiet = TRUE)
la_shp <- st_read(file.path(file_path, "Data/tl_2023_06_tract/tl_2023_06_tract.shp"), quiet = TRUE)
pico_buffer_tracts <- st_read(file.path(file_path, "Pico/Maps/pico_buffer_tracts.shp"), quiet = TRUE)

# Extract relevant Pico tracts
pico_tracts <- la_shp %>%
  filter(GEOID %in% pico_buffer_tracts$GEOID)

#pico blvd
pico_blvd <- st_read(file.path(file_path, "Pico/Maps/pico_blvd.shp"))

# Get major streets within Pico tract bounding box
bbox <- st_bbox(pico_tracts)
streets <- opq(bbox = bbox) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

major_streets <- streets$osm_lines %>%
  filter(highway %in% c("primary", "secondary", "tertiary")) %>%
  st_transform(st_crs(pico_tracts)) %>%
  st_intersection(st_union(st_geometry(pico_tracts)))

#create a version that clips the streets to the focus area
major_streets_clipped <- st_intersection(major_streets, st_union(st_geometry(pico_tracts)))

#streets for Pico maps
pico_streets <- major_streets_clipped %>%
  filter(name %in% c("West Pico Boulevard", "South Hoover Street", "Crenshaw Boulevard", "West Washington Boulevard", "West Olympic Boulevard"))

pico_streets_single <- pico_streets %>%
  filter(!is.na(name) & name != "") %>%
  group_by(name) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_centroid()

#key locations
pico_locations_df <- data.frame(
  name = c("HAROLD A HENRY PARK", "MACARTHUR PARK", "LAFAYETTE RECREATION CENTER", "LOS ANGELES ELEMENTARY SCHOOL", "LOS ANGELES HIGH SCHOOL", "NORMANDIE RECREATION CENTER"),
  lon = c(-118.326204, -118.277825, -118.283041, -118.332695, -118.332512, -118.299765),  # longitude from Google Maps
  lat = c(34.05771, 34.059539, 34.061962, 34.047654, 34.055755, 34.044543)         # latitude from Google Maps
)

pico_locations_sf <- st_as_sf(pico_locations_df, coords = c("lon", "lat"), crs = 4326)

st_write(pico_locations_sf, "pico_locations.shp", append = F)

#standard formatting for tmaps
###I have not had a chance to add these (working too fast) but we should plan to clean up maps on the next round of code 
standard_map_formatting = tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),  # moved up
    legend.outside = TRUE,
    legend.bg.color = "white",
    legend.bg.alpha = 0.8,
    legend.frame = TRUE,
    legend.title.size = .8,
    legend.text.size = .9,
    legend.title.color = "black",
    legend.text.color = "black",
    legend.title.fontface = "bold")

####----Population----####
#pull population table from ACS
sex_by_age <- pico_master_data %>%
  select(GEOID, NAME, contains("B01001"))

#restructure so that each row is a census tract
pop_wide_pico <- sex_by_age %>%
  select(GEOID, NAME, B01001_001, geometry)%>%
  rename(population = B01001_001)

# Create the map object
pico_population_map_1 <- tm_shape(
  shp = pop_wide_pico,
  bbox = st_bbox(st_buffer(pico_tracts, 1250))
)+
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_polygons(
    col = "population",
    palette = c("#87A0B4", "#1A4378"),
    style = "cont",
    n = 3,
    border.col = "white",
    border.lwd = 0.3,
    title = "Legend\n─────────",
    popup.format = list(fun = function(x) scales::comma(round(x))),
    legend.format = list(fun = function(x) scales::comma_format()(x)),
    na.show = FALSE
  )+
  tm_shape(pico_locations_sf) +
  tm_dots(
    col = "#ffffff",
    border.col = "#28381e",
    border.lwd = 2,
    size = 0.4
  ) +
  tm_text(
    "name",
    size = .9,
    just = "center",        
    ymod = -0.5,          
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text(
    "name",
    size = 1,
    col = "black",
    shadow = TRUE,
    auto.placement = TRUE,
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.outside = TRUE,  
    legend.bg.color = "white",      
    legend.bg.alpha = 0.8,          
    legend.frame = TRUE,           
    legend.title.size = .8,
    legend.text.size = .9,  
    legend.title.color = "black",  
    legend.text.color = "black",
    legend.title.fontface = "bold"
  ) 

tmap_mode("plot")



# Save as SVG
tmap_save(
  tm = pico_population_map_1,
  filename = file.path(file_path, "Pico/Outputs/pico_population_map_1.svg"),
  width = 12,
  height = 9,
  units = "in"
)

tmap_save(pico_population_map_1, filename = file.path(file_path, "Pico/Outputs/pico_population_map_1.png"), width = 12, height = 9, dpi = 300)



####----Country of Origin----####
##Country of origin##
##this one can be better streamlined in the future, but couldn't quite figure out the best way given the time crunch##
#pull from acs

#restructure so that each row is a census tract
country_origin <- 
  get_acs(
    geography = "tract",
    table = "B05006",
    state = "06",
    county = "037",
    year = 2023,
    survey = "acs5",
    geometry = FALSE)

write.csv(country_origin, file.path(file_path, "Fileshare", "country_origin.csv"), row.names = FALSE)

#filter for pico tracts 
country_origin_filtered_pico <- country_origin %>%
  filter(GEOID %in% pico_tracts$GEOID)

country_of_origin_totals <- country_origin_filtered_pico %>%
  group_by(variable) %>%
  summarise(total = sum(estimate, na.rm = TRUE), .groups = "drop")

##calculate total # share of foreign born

#divide by the total population 
foreign_value <- country_origin_filtered_pico %>%
  filter(variable == "B05006_001") %>%
  summarise(total_estimate = sum(estimate, na.rm = TRUE)) %>%
  mutate(result = total_estimate / variable) %>%
  pull(result)

#after reviewing the table above, edit to manually pull out the highest (in this case I chose the top countries with more than 1,000). This is done manually because some of the variables include 'Total', 'Asia', 'Central America', etc.
country_origin_top_10_pico <- country_origin_filtered_pico %>%
  filter(GEOID %in% pico_tracts$GEOID) %>%
  mutate(country = case_when(
    variable == "B05006_158" ~ "Guatemala",
    variable == "B05006_157" ~ "El Salvador",
    variable == "B05006_160" ~ "Mexico",
    variable == "B05006_054" ~ "Korea",
    variable == "B05006_049" ~ "China",
    variable == "B05006_074" ~ "Phillipnes",
    variable == "B05006_159" ~ "Honduras"
  )) %>%
  group_by(country) %>%
  summarise(total = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
  mutate(share_pop = total / totalpop)

write.csv(country_origin_filtered_pico, file.path(file_path, "Fileshare", "country_origin_filtered_pico.csv"), row.names = FALSE)

####----Disability----####
#pulling disability status data
disability <- pico_master_data%>%
  select(GEOID, NAME, contains("B18101"))

disability_wide_pico <- disability %>%
  mutate(
    total_disability = B18101_004 + B18101_007 + B18101_010 + B18101_013 +
      B18101_016 + B18101_019 + B18101_023 + B18101_026 +
      B18101_029 + B18101_032 + B18101_035 + B18101_038,
    share_disability = total_disability / B18101_001
  ) %>%
  select(GEOID, total_pop = B18101_001, total_disability, share_disability)


#creating custom bins for a map of %pop w/ disability
map_disability_pico <- disability_wide_pico %>% 
  mutate(bin_disability = cut(total_disability, breaks=c(0,200, 300, 400, 500,800),
                              labels  = c("Less than 200", "200-300", "300-400", "400-500", "500 or more"),
                              include.lowest = TRUE))

## WCG: prefer to consolidate this into the mutate call above rather than use the $ operator
#next, make each of the bins a factor
map_disability_pico$bin_disability <- factor(map_disability_pico$bin_disability, 
                                             levels = c("Less than 200", "200-300", "300-400", "400-500", "500 or more"))


pico_disability_map <- tm_shape(
  shp = map_disability_pico,
  bbox = st_bbox(st_buffer(pico_tracts, 1250))
)+
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_polygons(
    col = "bin_disability",
    palette = c("#87A0B4", "#1A4378"),
    style = "cat",
    n = 3,
    border.col = "white",
    border.lwd = 0.3,
    title = "Legend\n─────────",
    # popup.format = list(fun = function(x) scales::comma(round(x))),
    # legend.format = list(fun = function(x) scales::comma_format()(x)),
    na.show = FALSE
  )+
  tm_shape(pico_locations_sf) +
  tm_dots(
    col = "#ffffff",
    border.col = "#28381e",
    border.lwd = 2,
    size = 0.4
  ) +
  tm_text(
    "name",
    size = .9,
    just = "center",        
    ymod = -0.5,          
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text(
    "name",
    size = 1,
    col = "black",
    shadow = TRUE,
    auto.placement = TRUE,
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.outside = TRUE,  
    legend.bg.color = "white",      
    legend.bg.alpha = 0.8,          
    legend.frame = TRUE,           
    legend.title.size = .8,
    legend.text.size = .9,  
    legend.title.color = "black",  
    legend.text.color = "black",
    legend.title.fontface = "bold"
  ) 

# Save as SVG
tmap_save(
  tm = pico_disability_map,
  filename = file.path(file_path, "Pico/Outputs/pico_disability_map.svg"),
  width = 12,
  height = 9,
  units = "in"
)

tmap_save(pico_disability_map, filename = file.path(file_path, "Pico/Outputs/pico_disability_map.png"), width = 12, height = 9, dpi = 300)

####----Median Income / Poverty----####
#pull income from ACS
median_income <- pico_master_data%>%
  select(GEOID, NAME, contains("B19013"))

income_wide_pico <- median_income %>%
  select(GEOID, NAME, B19013_001, geometry)%>%
  rename(med_income = B19013_001)


##Map of median incomes by Census Tract

#create a map based on incomes
income_map_pico_1 <- tm_shape(
  shp = income_wide_pico,
  bbox = st_bbox(st_buffer(pico_tracts, 1250))
)+
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_polygons(
    col = "med_income",
    palette = c("#87A0B4", "#1A4378"),
    style = "cont",
    n = 3,
    border.col = "white",
    border.lwd = 0.3,
    title = "Legend\n─────────",
    popup.format = list(fun = function(x) scales::dollar(round(x))),
    legend.format = list(fun = function(x) scales::dollar_format()(x)),
    na.show = FALSE
  )+
  tm_shape(pico_locations_sf) +
  tm_dots(
    col = "#ffffff",
    border.col = "#28381e",
    border.lwd = 2,
    size = 0.4
  ) +
  tm_text(
    "name",
    size = .9,
    just = "center",        
    ymod = -0.5,          
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text(
    "name",
    size = 1,
    col = "black",
    shadow = TRUE,
    auto.placement = TRUE,
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.outside = TRUE,  
    legend.bg.color = "white",      
    legend.bg.alpha = 0.8,          
    legend.frame = TRUE,           
    legend.title.size = .8,
    legend.text.size = .9,  
    legend.title.color = "black",  
    legend.text.color = "black",
    legend.title.fontface = "bold"
  ) 

tmap_mode("plot")

# Save as SVG
tmap_save(
  tm = income_map_pico_1,
  filename = file.path(file_path, "Pico/Outputs/income_map_pico_1.svg"),
  width = 12,
  height = 9,
  units = "in"
)

tmap_save(income_map_pico_1, filename = file.path(file_path, "Pico/Outputs/income_map_pico_1.png"), width = 12, height = 9, dpi = 300) 

#pull poverty level
poverty_level <- pico_master_data%>%
  select(GEOID, NAME, contains("B17001"))

poverty_wide_pico <- poverty_level%>%
  select(GEOID, NAME, B17001_001, B17001_002)%>%
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

#find total sum and share
sum(poverty_wide_pico$below_pov)

total_share_below_pov <- sum(poverty_wide_pico$below_pov, na.rm = TRUE) / sum(poverty_wide_pico$total_pop, na.rm = TRUE)

#Map of tracts by poverty level status
poverty_map_pico_1 <- tm_shape(
  shp = poverty_wide_pico,
  bbox = st_bbox(st_buffer(pico_tracts, 1250))
)+
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_polygons(
    col = "pov_bin",
    palette = c("#87A0B4", "#1A4378"),
    style = "cat",
    n = 4,
    border.col = "white",
    border.lwd = 0.3,
    title = "Legend\n─────────",
    popup.format = list(fun = function(x) scales::comma(round(x))),
    legend.format = list(fun = function(x) scales::comma_format()(x)),
    na.show = FALSE
  )+
  tm_shape(pico_locations_sf) +
  tm_dots(
    col = "#ffffff",
    border.col = "#28381e",
    border.lwd = 2,
    size = 0.4
  ) +
  tm_text(
    "name",
    size = .9,
    just = "center",        
    ymod = -0.5,          
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text(
    "name",
    size = 1,
    col = "black",
    shadow = TRUE,
    auto.placement = TRUE,
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.outside = TRUE,  
    legend.bg.color = "white",      
    legend.bg.alpha = 0.8,          
    legend.frame = TRUE,           
    legend.title.size = .8,
    legend.text.size = .9,  
    legend.title.color = "black",  
    legend.text.color = "black",
    legend.title.fontface = "bold"
  )

# Save as SVG
tmap_save(
  tm = poverty_map_pico_1,
  filename = file.path(file_path, "Pico/Outputs/poverty_map_pico_1.svg"),
  width = 12,
  height = 9,
  units = "in"
)

tmap_save(poverty_map_pico_1, filename = file.path(file_path, "Pico/Outputs/poverty_map_pico_1.png"), width = 12, height = 9, dpi = 300)

####----Language Access / Culture----####

###pull hearing difficulty status
hearing_diff <- pico_master_data%>%
  select(GEOID, NAME, contains("B18102"))

hearing_wide_pico <- hearing_diff %>%
  mutate(
    total_hearing_diff = B18102_004 + B18102_007 + B18102_010 + B18102_013 +
      B18102_016 + B18102_019 + B18102_023 + B18102_026 +
      B18102_029 + B18102_032 + B18102_035 + B18102_038,
    share_hearing_diff = total_hearing_diff / B18102_001
  ) %>%
  select(GEOID, total_pop = B18102_001, total_hearing_diff, share_hearing_diff)

#creating custom bins for a map of %pop w/ hearing diff
map_hearing_diff <- hearing_wide_pico%>% 
  mutate(bin_hearing_diff = cut(total_hearing_diff, breaks=c(0,50,100,150,99999),
                                labels  = c("Less than 50", "50-100", "100-150", "150 or more"),
                                include.lowest = TRUE))

#making each bin a factor
map_hearing_diff$bin_hearing_diff <- factor(map_hearing_diff$bin_hearing_diff, 
                                            levels = c("Less than 50", "50-100", "100-150", "150 or more"))

##WCG: if I were to guess, there are no statistically significant differences in counts
##of individuals with hearing difficulty across the tracts you're looking at because
##the margions of error are so high. I'd consider dropping all of the subsets of disability here--
##they're interesting, but probably not appropriate to map absent more context.
#plot map
pico_hearing_map_1 <- tm_shape(
  shp = map_hearing_diff,
  bbox = st_bbox(st_buffer(pico_tracts, 1250))
)+
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_polygons(
    col = "bin_hearing_diff",
    palette = c("#87A0B4", "#1A4378"),
    style = "cat",
    n = 3,
    border.col = "white",
    border.lwd = 0.3,
    title = "Legend\n─────────",
    # popup.format = list(fun = function(x) scales::comma(round(x))),
    # legend.format = list(fun = function(x) scales::comma_format()(x)),
    na.show = FALSE
  )+
  tm_shape(pico_locations_sf) +
  tm_dots(
    col = "#ffffff",
    border.col = "#28381e",
    border.lwd = 2,
    size = 0.4
  ) +
  tm_text(
    "name",
    size = .9,
    just = "center",        
    ymod = -0.5,          
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text(
    "name",
    size = 1,
    col = "black",
    shadow = TRUE,
    auto.placement = TRUE,
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.outside = TRUE,  
    legend.bg.color = "white",      
    legend.bg.alpha = 0.8,          
    legend.frame = TRUE,           
    legend.title.size = .8,
    legend.text.size = .9,  
    legend.title.color = "black",  
    legend.text.color = "black",
    legend.title.fontface = "bold"
  ) 

# Save as SVG
tmap_save(
  tm = pico_hearing_map_1,
  filename = file.path(file_path, "Pico/Outputs/pico_hearing_map_1.svg"),
  width = 12,
  height = 9,
  units = "in"
)

tmap_save(pico_hearing_map_1, filename = file.path(file_path, "Pico/Outputs/pico_hearing_map_1.png"), width = 12, height = 9, dpi = 300)

#pull vision difficulty status data
vision_diff <- pico_master_data%>%
  select(GEOID, NAME, contains("B18103"))

vision_wide_pico <- vision_diff %>%
  mutate(
    total_vision_diff = B18103_004 + B18103_007 + B18103_010 + B18103_013 +
      B18103_016 + B18103_019 + B18103_023 + B18103_026 +
      B18103_029 + B18103_032 + B18103_035 + B18103_038,
    share_vision_diff = total_vision_diff / B18103_001
  ) %>%
  select(GEOID, total_pop = B18103_001, total_vision_diff, share_vision_diff)


#creating custom bins for a map of %pop w/ vision diff
map_vision_diff <- vision_wide_pico%>% 
  mutate(bin_vision_diff = cut(total_vision_diff, breaks=c(0,50,100,150,99999),
                               labels  = c("Less than 50", "50-100", "100-150", "150 or more"),
                               include.lowest = TRUE))
#making each bin a factor
map_vision_diff$bin_vision_diff <- factor(map_vision_diff$bin_vision_diff, 
                                          levels = c("Less than 50", "50-100", "100-150", "150 or more"))

pico_vision_map_1 <- tm_shape(
  shp = map_vision_diff,
  bbox = st_bbox(st_buffer(pico_tracts, 1250))
)+
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_polygons(
    col = "bin_vision_diff",
    palette = c("#87A0B4", "#1A4378"),
    style = "cat",
    n = 3,
    border.col = "white",
    border.lwd = 0.3,
    title = "Legend\n─────────",
    # popup.format = list(fun = function(x) scales::comma(round(x))),
    # legend.format = list(fun = function(x) scales::comma_format()(x)),
    na.show = FALSE
  )+
  tm_shape(pico_locations_sf) +
  tm_dots(
    col = "#ffffff",
    border.col = "#28381e",
    border.lwd = 2,
    size = 0.4
  ) +
  tm_text(
    "name",
    size = .9,
    just = "center",        
    ymod = -0.5,          
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text(
    "name",
    size = 1,
    col = "black",
    shadow = TRUE,
    auto.placement = TRUE,
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.outside = TRUE,  
    legend.bg.color = "white",      
    legend.bg.alpha = 0.8,          
    legend.frame = TRUE,           
    legend.title.size = .8,
    legend.text.size = .9,  
    legend.title.color = "black",  
    legend.text.color = "black",
    legend.title.fontface = "bold"
  ) 

# Save as SVG
tmap_save(
  tm = pico_vision_map_1,
  filename = file.path(file_path, "Pico/Outputs/pico_vision_map_1.svg"),
  width = 12,
  height = 9,
  units = "in"
)

tmap_save(pico_vision_map_1, filename = file.path(file_path, "Pico/Outputs/pico_vision_map_1.png"), width = 12, height = 9, dpi = 300)

#pulling ambulatory difficulty data
ambulatory_diff <- pico_master_data%>%
  select(GEOID, NAME, contains("B18105"))

ambulatory_wide_pico <- ambulatory_diff %>%
  mutate(
    total_ambulatory_diff = B18105_004 + B18105_007 + B18105_010 + B18105_013 + B18105_016 +
      B18105_020 + B18105_023 + B18105_026 + B18105_029 + B18105_032,
    share_ambulatory_diff = total_ambulatory_diff / B18105_001
  ) %>%
  select(GEOID, total_pop = B18105_001, total_ambulatory_diff, share_ambulatory_diff)


#creating custom bins for a map of %pop w/ ambulatory diff
map_ambulatory_diff <- ambulatory_wide_pico%>% 
  mutate(bin_ambulatory_diff = cut(total_ambulatory_diff, breaks=c(0,50,100,200, 300,9999),
                                   labels  = c("Fewer than 50", "50-100", "100-200", "200-300", "300 or more"),
                                   include.lowest = TRUE))
#making each bin a factor
map_ambulatory_diff$bin_ambulatory_diff <- factor(map_ambulatory_diff$bin_ambulatory_diff, 
                                                  levels = c("Fewer than 50", "50-100", "100-200", "200-300", "300 or more"))

#make a map
pico_ambulatory_map_1 <- tm_shape(
  shp = map_ambulatory_diff,
  bbox = st_bbox(st_buffer(pico_tracts, 1250))
)+
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_polygons(
    col = "bin_ambulatory_diff",
    palette = c("#87A0B4", "#1A4378"),
    style = "cat",
    n = 3,
    border.col = "white",
    border.lwd = 0.3,
    title = "Legend\n─────────",
    # popup.format = list(fun = function(x) scales::comma(round(x))),
    # legend.format = list(fun = function(x) scales::comma_format()(x)),
    na.show = FALSE
  )+
  tm_shape(pico_locations_sf) +
  tm_dots(
    col = "#ffffff",
    border.col = "#28381e",
    border.lwd = 2,
    size = 0.4
  ) +
  tm_text(
    "name",
    size = .9,
    just = "center",        
    ymod = -0.5,          
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text(
    "name",
    size = 1,
    col = "black",
    shadow = TRUE,
    auto.placement = TRUE,
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.outside = TRUE,  
    legend.bg.color = "white",      
    legend.bg.alpha = 0.8,          
    legend.frame = TRUE,           
    legend.title.size = .8,
    legend.text.size = .9,  
    legend.title.color = "black",  
    legend.text.color = "black",
    legend.title.fontface = "bold"
  ) 

# Save as SVG
tmap_save(
  tm = pico_ambulatory_map_1,
  filename = file.path(file_path, "Pico/Outputs/pico_ambulatory_map_1.svg"),
  width = 12,
  height = 9,
  units = "in"
)

tmap_save(pico_ambulatory_map_1, filename = file.path(file_path, "Pico/Outputs/pico_ambulatory_map_1.png"), width = 12, height = 9, dpi = 300)

####----Internet / Computer Access----####
#pull internet data
internet_subs <- pico_master_data%>%
  select(GEOID, NAME, contains("B28002"))

internet_wide_pico <- internet_subs %>%
  rename(
    total_pop = B28002_001,
    no_int_access = B28002_013
  )%>%
  mutate(share_no_int = no_int_access/total_pop)%>%
  select(GEOID, total_pop, no_int_access, share_no_int)


#creating custom bins for a map of share w/out int access
map_no_int <- internet_wide_pico%>% 
  mutate(bin_no_int = cut(share_no_int, breaks=c(0,0.05, 0.1, 0.15,0.2,1),
                          labels  = c("Less than 5%", "5-10%", "10-15%", "15-20%", "20% or more"),
                          include.lowest = TRUE))
#making each bin a factor
map_no_int$bin_no_int <- factor(map_no_int$bin_no_int, 
                                levels = c("Less than 5%", "5-10%", "10-15%", "15-20%", "20% or more"))

#plot map
pico_no_int_map <- tm_shape(
  shp = map_no_int,
  bbox = st_bbox(st_buffer(pico_tracts, 1250))
)+
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_polygons(
    col = "bin_no_int",
    palette = c("#87A0B4", "#1A4378"),
    style = "cat",
    n = 3,
    border.col = "white",
    border.lwd = 0.3,
    title = "Legend\n─────────",
    # popup.format = list(fun = function(x) scales::comma(round(x))),
    # legend.format = list(fun = function(x) scales::comma_format()(x)),
    na.show = FALSE
  )+
  tm_shape(pico_locations_sf) +
  tm_dots(
    col = "#ffffff",
    border.col = "#28381e",
    border.lwd = 2,
    size = 0.4
  ) +
  tm_text(
    "name",
    size = .9,
    just = "center",        
    ymod = -0.5,          
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text(
    "name",
    size = 1,
    col = "black",
    shadow = TRUE,
    auto.placement = TRUE,
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.outside = TRUE,  
    legend.bg.color = "white",      
    legend.bg.alpha = 0.8,          
    legend.frame = TRUE,           
    legend.title.size = .8,
    legend.text.size = .9,  
    legend.title.color = "black",  
    legend.text.color = "black",
    legend.title.fontface = "bold"
  ) 

# Save as SVG
tmap_save(
  tm = pico_no_int_map,
  filename = file.path(file_path, "Pico/Outputs/pico_no_int_map.svg"),
  width = 12,
  height = 9,
  units = "in"
)

tmap_save(pico_no_int_map, filename = file.path(file_path, "Pico/Outputs/pico_no_int_map.png"), width = 12, height = 9, dpi = 300)

####----Tech Access----####
tech_access <- pico_master_data%>%
  select(GEOID, NAME, contains("B28001"))

tech_wide_pico <- tech_access %>%
  rename(
    total_pop = B28001_001,
    no_computer = B28001_011
  )%>%
  mutate(share_no_computer = no_computer/total_pop)%>%
  select(GEOID, total_pop, no_computer, share_no_computer)


#creating custom bins for a map of %pop w/ no access
map_tech_access <- tech_wide_pico%>% 
  mutate(bin_no_computer = cut(share_no_computer, breaks=c(0,0.01,0.05,0.1,1),
                               labels  = c("Less than 1%", "1-5%", "5-10%", "10% or higher"),
                               include.lowest = TRUE))
#making each bin a factor
map_tech_access$bin_no_computer <- factor(map_tech_access$bin_no_computer, 
                                          levels = c("Less than 1%", "1-5%", "5-10%", "10% or higher"))

#plot map
pico_tech_map <- tm_shape(
  shp = map_tech_access,
  bbox = st_bbox(st_buffer(pico_tracts, 1250))
)+
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_polygons(
    col = "bin_no_computer",
    palette = c("#87A0B4", "#1A4378"),
    style = "cat",
    n = 3,
    border.col = "white",
    border.lwd = 0.3,
    title = "Legend\n─────────",
    # popup.format = list(fun = function(x) scales::comma(round(x))),
    # legend.format = list(fun = function(x) scales::comma_format()(x)),
    na.show = FALSE
  )+
  tm_shape(pico_locations_sf) +
  tm_dots(
    col = "#ffffff",
    border.col = "#28381e",
    border.lwd = 2,
    size = 0.4
  ) +
  tm_text(
    "name",
    size = .9,
    just = "center",        
    ymod = -0.5,          
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text(
    "name",
    size = 1,
    col = "black",
    shadow = TRUE,
    auto.placement = TRUE,
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.outside = TRUE,  
    legend.bg.color = "white",      
    legend.bg.alpha = 0.8,          
    legend.frame = TRUE,           
    legend.title.size = .8,
    legend.text.size = .9,  
    legend.title.color = "black",  
    legend.text.color = "black",
    legend.title.fontface = "bold"
  ) 

tmap_save(
  tm = pico_tech_map,
  filename = file.path(file_path, "Pico/Outputs/pico_tech_map.svg"),
  width = 12,
  height = 9,
  units = "in"
)

tmap_save(pico_tech_map, filename = file.path(file_path, "Pico/Outputs/pico_tech_map.png"), width = 12, height = 9, dpi = 300)

####----Housing and Displacement----####

### Homeowners/Renters ###
#pull housing tenure data

tenure <- pico_master_data %>%
  select(GEOID, NAME, contains("B25003"))

tenure_wide_pico <-tenure%>%
  select(GEOID, NAME, B25003_001, B25003_002, B25003_003)%>%
  rename(
    total_occupied = "B25003_001",
    owner_occupied = "B25003_002",
    renter_occupied = "B25003_003"
  )%>%
  mutate(
    share_owner_occupied = owner_occupied/total_occupied,
    share_renter_occupied = renter_occupied/total_occupied)%>%
  select(GEOID,total_occupied,owner_occupied,share_owner_occupied, renter_occupied, share_renter_occupied)


#creating custom bins for a map of share of occupied HU that are renter
map_tenure <- tenure_wide_pico%>% 
  mutate(bin_tenure = cut(share_renter_occupied, breaks=c(0.5, 0.6, 0.7,0.8,0.9,1),
                          labels  = c("50-60%", "60-70%", "70-80%", "80-90%", "90-100%"),
                          include.lowest = TRUE))
#making each bin a factor
map_tenure$bin_tenure <- factor(map_tenure$bin_tenure, 
                                levels =c("50-60%", "60-70%", "70-80%", "80-90%", "90-100%"))

#plot map
pico_tenure_map <- tm_shape(
  shp = map_tenure,
  bbox = st_bbox(st_buffer(pico_tracts, 1250))
)+
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_polygons(
    col = "bin_tenure",
    palette = c("#87A0B4", "#1A4378"),
    style = "cat",
    n = 3,
    border.col = "white",
    border.lwd = 0.3,
    title = "Legend\n─────────",
    # popup.format = list(fun = function(x) scales::comma(round(x))),
    # legend.format = list(fun = function(x) scales::comma_format()(x)),
    na.show = FALSE
  )+
  tm_shape(pico_locations_sf) +
  tm_dots(
    col = "#ffffff",
    border.col = "#28381e",
    border.lwd = 2,
    size = 0.4
  ) +
  tm_text(
    "name",
    size = .9,
    just = "center",        
    ymod = -0.5,          
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text(
    "name",
    size = 1,
    col = "black",
    shadow = TRUE,
    auto.placement = TRUE,
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.outside = TRUE,  
    legend.bg.color = "white",      
    legend.bg.alpha = 0.8,          
    legend.frame = TRUE,           
    legend.title.size = .8,
    legend.text.size = .9,  
    legend.title.color = "black",  
    legend.text.color = "black",
    legend.title.fontface = "bold"
  )

# Save as SVG
tmap_save(
  tm = pico_tenure_map,
  filename = file.path(file_path, "Pico/Outputs/pico_tenure_map.svg"),
  width = 12,
  height = 9,
  units = "in"
)

#save as PNG
tmap_save(pico_tenure_map, filename = file.path(file_path, "Pico/Outputs/pico_tenure_map.png"), width = 12, height = 9, dpi = 300)

#check overall homeownership for the report
total_share_owner_occupied <- sum(tenure$owner_occupied) / sum(tenure$total_occupied)

##Affordable Housing Properties##
affordable_housing <- st_read(file.path(file_path, "Data/Affordable_Housing_Development/Affordable_Housing_Development.shp"))

#set so that CRS are the same
pico_tracts <- st_transform(pico_tracts, crs = 4326)
affordable_housing <- st_transform(affordable_housing, crs = 4326)

#make geometries valid
affordable_housing <- st_make_valid(affordable_housing)
pico_tracts <- st_make_valid(pico_tracts)

affordable_pico <- st_filter(affordable_housing, pico_tracts)

#create a map
aff_map_pico <- tm_shape(
  affordable_pico,
  bbox = st_bbox(st_buffer(pico_tracts, 1250))
) +
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_dots(
    palette= c("#87A0B4"),
    size = 0.35,
    alpha = 0.9,
    border.col = "black",
    border.lwd = 0.5,
    title = "Legend\n─────────"
  ) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),  # moved up
    legend.outside = TRUE,
    legend.bg.color = "white",
    legend.bg.alpha = 0.8,
    legend.frame = TRUE,
    legend.title.size = .8,
    legend.text.size = .9,
    legend.title.color = "black",
    legend.text.color = "black",
    legend.title.fontface = "bold"
  )


#save as PNG
tmap_save(aff_map_pico, filename = file.path(file_path, "Pico/Outputs/aff_map_pico.png"), width = 12, height = 9, dpi = 300)

# Save as SVG
tmap_save(
  tm = aff_map_pico,
  filename = file.path(file_path, "Pico/Outputs/aff_map_pico.svg"),
  width = 12,
  height = 9,
  units = "in"
)


##Senior Housing Properties##
senior_housing <- st_read(file.path(file_path, "Data/Senior_Housing-shp/Senior_Housing.shp"))

#set so that CRS are the same
pico_tracts <- st_transform(pico_tracts, crs = 4326)
senior_housing <- st_transform(senior_housing, crs = 4326)

#make geometries valid
senior_housing <- st_make_valid(senior_housing)
pico_tracts <- st_make_valid(pico_tracts)

senior_pico <- st_filter(senior_housing, pico_tracts)

#create a map
senior_map_pico <- tm_shape(
  senior_pico,
  bbox = st_bbox(st_buffer(pico_tracts, 1250))
) +
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_dots(
    palette= c("#87A0B4"),
    size = 0.35,
    alpha = 0.9,
    border.col = "black",
    border.lwd = 0.5,
    title = "Legend\n─────────"
  ) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),  # moved up
    legend.outside = TRUE,
    legend.bg.color = "white",
    legend.bg.alpha = 0.8,
    legend.frame = TRUE,
    legend.title.size = .8,
    legend.text.size = .9,
    legend.title.color = "black",
    legend.text.color = "black",
    legend.title.fontface = "bold"
  )


#save as PNG
tmap_save(senior_map_pico, filename = file.path(file_path, "Pico/Outputs/senior_map_pico.png"), width = 12, height = 9, dpi = 300)

# Save as SVG
tmap_save(
  tm = senior_map_pico,
  filename = file.path(file_path, "Pico/Outputs/senior_map_pico.svg"),
  width = 12,
  height = 9,
  units = "in"
)

####----Language----####

#pull from acs
##Other languages spoken in the area##
language_specific <- pico_master_data%>%
  select(GEOID, NAME, contains("C16001"))

#create bins for each language spoken
language_bins_1 <- list(
  total = "C16001_001",
  only_english = "C16001_002",
  spanish = "C16001_005",
  korean = "C16001_020",
  chinese = "C16001_023",
  other_language = c("C16001_008", "C16001_011", "C16001_014", "C16001_017","C16001_026","C16001_029", "C16001_032", "C16001_035", "C16001_038")
)

#summarise total by language, for each tract
spoken_language_wide_pico <- language_specific %>%
  pivot_longer(
    cols = starts_with("C16001"),
    names_to = "variable",
    values_to = "estimate"
  ) %>%
  filter(variable %in% unlist(language_bins_1)) %>%
  mutate(language_spoken = case_when(
    variable %in% language_bins_1$total ~ "Total",
    variable %in% language_bins_1$only_english ~ "English Only",
    variable %in% language_bins_1$spanish ~ "Spanish",
    variable %in% language_bins_1$korean ~ "Korean",
    variable %in% language_bins_1$chinese ~ "Chinese",
    variable %in% language_bins_1$other_language ~ "Other Languages",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(language_spoken)) %>%
  group_by(GEOID, language_spoken) %>%
  summarise(population = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = language_spoken,
    values_from = population,
    values_fill = list(population = 0)
  ) %>%
  left_join(
    select(st_drop_geometry(language), GEOID, NAME) %>% distinct(),
    by = "GEOID"
  )


#calculate totals for bar chart
language_spoken_totals <- spoken_language_wide_pico %>%
  select(-GEOID, -NAME) %>%
  summarise(across(everything(), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "language_spoken",
    values_to = "population"
  )


# Calculate percentages for each language spoken category
spoken_language_wide_pico_pct <- spoken_language_wide_pico %>%
  mutate(
    share_english_only = `English Only` / Total,
    share_spanish = Spanish / Total,
    share_korean = Korean / Total,
    share_chinese = Chinese / Total,
    share_other_language = `Other Languages` / Total
  ) %>%
  select(GEOID, NAME, geometry, starts_with("share")) %>%
  rename(
    `English Only` = share_english_only,
    `Spanish` = share_spanish,
    `Korean` = share_korean,
    `Chinese` = share_chinese,
    `Other Languages` = share_other_language
  )

# Reorder the factor levels
language_spoken_totals_pct$language_spoken <- factor(
  language_spoken_totals_pct$language_spoken,
  levels = c("English Only", "Spanish", "Korean", "Chinese","Other Languages")
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

spanish_map_pico <- tm_shape(
  shp = spoken_language_wide_pico_pct,
  bbox = st_bbox(st_buffer(pico_tracts, 1250))
)+
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_polygons(
    col = "Spanish",
    palette = c("#87A0B4", "#1A4378"),
    style = "cont",
    n = 3,
    border.col = "white",
    border.lwd = 0.3,
    title = "Legend\n─────────",
    popup.format = list(fun = function(x) scales::percent(x, accuracy = 1)),
    legend.format = list(fun = function(x) scales::percent(x, accuracy = 1)),
    na.show = FALSE
  )+
  tm_shape(pico_locations_sf) +
  tm_dots(
    col = "#ffffff",
    border.col = "#28381e",
    border.lwd = 2,
    size = 0.4
  ) +
  tm_text(
    "name",
    size = .9,
    just = "center",        
    ymod = -0.5,          
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text(
    "name",
    size = 1,
    col = "black",
    shadow = TRUE,
    auto.placement = TRUE,
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.outside = TRUE,  
    legend.bg.color = "white",      
    legend.bg.alpha = 0.8,          
    legend.frame = TRUE,           
    legend.title.size = .8,
    legend.text.size = .9,  
    legend.title.color = "black",  
    legend.text.color = "black",
    legend.title.fontface = "bold"
  ) 

#SAVE
tmap_save(
  tm = spanish_map_pico,
  filename = file.path(file_path, "Pico/Outputs/spanish_map_pico.svg"),
  width = 12,
  height = 9,
  units = "in"
)

tmap_save(spanish_map_pico, filename = file.path(file_path, "Pico/Outputs/spanish_map_pico.png"), width = 12, height = 9, dpi = 300)

#korean language map
korean_map_pico <- tm_shape(
  shp = spoken_language_wide_pico_pct,
  bbox = st_bbox(st_buffer(pico_tracts, 1250))
)+
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_polygons(
    col = "Korean",
    palette = c("#87A0B4", "#1A4378"),
    style = "cont",
    n = 3,
    border.col = "white",
    border.lwd = 0.3,
    title = "Legend\n─────────",
    popup.format = list(fun = function(x) scales::percent(x, accuracy = 1)),
    legend.format = list(fun = function(x) scales::percent(x, accuracy = 1)),
    na.show = FALSE
  )+
  tm_shape(pico_locations_sf) +
  tm_dots(
    col = "#ffffff",
    border.col = "#28381e",
    border.lwd = 2,
    size = 0.4
  ) +
  tm_text(
    "name",
    size = .9,
    just = "center",        
    ymod = -0.5,          
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text(
    "name",
    size = 1,
    col = "black",
    shadow = TRUE,
    auto.placement = TRUE,
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.outside = TRUE,  
    legend.bg.color = "white",      
    legend.bg.alpha = 0.8,          
    legend.frame = TRUE,           
    legend.title.size = .8,
    legend.text.size = .9,  
    legend.title.color = "black",  
    legend.text.color = "black",
    legend.title.fontface = "bold"
  ) 

#SAVE
tmap_save(
  tm = korean_map_pico,
  filename = file.path(file_path, "Pico/Outputs/korean_map_pico.svg"),
  width = 12,
  height = 9,
  units = "in"
)

tmap_save(korean_map_pico, filename = file.path(file_path, "Pico/Outputs/korean_map_pico.png"), width = 12, height = 9, dpi = 300)

####----Housing Cost Burden----####
#pull cost burden data

### for renters

#running this one with the original ACS code because the new code was not working. Can update later
renter_burden <-   
  get_acs(
    geography = "tract",
    variables = c(
      total = "B25070_001",
      spend_30_35 = "B25070_007",
      spend_35_40 = "B25070_008",
      spend_40_50="B25070_009",
      spend_50_more = "B25070_010",
      median_share_inc_housing = "B25071_001"
    ),
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE,
  )%>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)%>% #pivoting
  mutate(
    total_over_30 = (spend_30_35 + spend_35_40+spend_40_50 + spend_50_more),
    share_over_30 = (spend_30_35 + spend_35_40+spend_40_50 + spend_50_more)/total,
    share_over_50 = spend_50_more/total)%>% #generating shares of HH that are rent burdened and extremely rent burdened
  select(GEOID,total,spend_50_more,total_over_30,share_over_30,share_over_50,median_share_inc_housing)%>% #reducing df to necessary variables
  filter(GEOID %in% pico_tracts$GEOID) #limiting to pico tracts

#adding geometry for map
st_geometry(renter_burden) <- st_geometry(pico_tracts[match(renter_burden$GEOID, pico_tracts$GEOID), ])
class(renter_burden)

## first, map out the median income spent on housing by census tract
#making bins
map_med_inc_housing <- renter_burden%>%
  mutate(bin_median_share = cut(median_share_inc_housing, breaks=c(25, 30, 35, 40, 45, 50),
                                labels = c("less than 30%", "30-35%", "35-40%", "40-45%", "45-50%"),
                                include.lowest = TRUE))
#make each bin a factor
map_med_inc_housing$bin_median_share <- factor(map_med_inc_housing$bin_median_share,
                                               levels = c("less than 30%", "30-35%", "35-40%", "40-45%", "45-50%"))

#plotting
pico_rt_burden_map_1 <- tm_shape(
  shp = map_med_inc_housing,
  bbox = st_bbox(st_buffer(pico_tracts, 1250))
)+
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_polygons(
    col = "bin_median_share",
    palette = c("#87A0B4", "#1A4378"),
    style = "cat",
    n = 3,
    border.col = "white",
    border.lwd = 0.3,
    title = "Legend\n─────────",
    # popup.format = list(fun = function(x) scales::comma(round(x))),
    # legend.format = list(fun = function(x) scales::comma_format()(x)),
    na.show = FALSE
  )+
  tm_shape(pico_locations_sf) +
  tm_dots(
    col = "#ffffff",
    border.col = "#28381e",
    border.lwd = 2,
    size = 0.4
  ) +
  tm_text(
    "name",
    size = .9,
    just = "center",        
    ymod = -0.5,          
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text(
    "name",
    size = 1,
    col = "black",
    shadow = TRUE,
    auto.placement = TRUE,
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.outside = TRUE,  
    legend.bg.color = "white",      
    legend.bg.alpha = 0.8,          
    legend.frame = TRUE,           
    legend.title.size = .8,
    legend.text.size = .9,  
    legend.title.color = "black",  
    legend.text.color = "black",
    legend.title.fontface = "bold"
  ) 

# Save as SVG
tmap_save(
  tm = pico_rt_burden_map_1,
  filename = file.path(file_path, "Pico/Outputs/pico_rt_burden_map_1.svg"),
  width = 12,
  height = 9,
  units = "in"
)

#save as png
tmap_save(pico_rt_burden_map_1, filename = file.path(file_path, "Pico/Outputs/pico_rt_burden_map_1.png"), width = 12, height = 9, dpi = 300)

## next, map out the share of individuals spending more than 30% of income on rental housing
#making bins
map_30_pct <- renter_burden%>%
  mutate(bin_30_pct = cut(share_over_30, breaks=c(.30, .40, .50, .60, .70, .80),
                          labels = c("less than 40%", "40-50%", "50-60%", "60-70%", "70-80%"),
                          include.lowest = TRUE))
#make each bin a factor
map_30_pct$bin_30_pct <- factor(map_30_pct$bin_30_pct,
                                levels = c("less than 40%", "40-50%", "50-60%", "60-70%", "70-80%"))
#plotting
pico_rt_burden_map_2 <- tm_shape(
  shp = map_30_pct,
  bbox = st_bbox(st_buffer(pico_tracts, 1250))
)+
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_polygons(
    col = "bin_30_pct",
    palette = c("#87A0B4", "#1A4378"),
    style = "cat",
    n = 3,
    border.col = "white",
    border.lwd = 0.3,
    title = "Legend\n─────────",
    # popup.format = list(fun = function(x) scales::comma(round(x))),
    # legend.format = list(fun = function(x) scales::comma_format()(x)),
    na.show = FALSE
  )+
  tm_shape(pico_locations_sf) +
  tm_dots(
    col = "#ffffff",
    border.col = "#28381e",
    border.lwd = 2,
    size = 0.4
  ) +
  tm_text(
    "name",
    size = .9,
    just = "center",        
    ymod = -0.5,          
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text(
    "name",
    size = 1,
    col = "black",
    shadow = TRUE,
    auto.placement = TRUE,
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.outside = TRUE,  
    legend.bg.color = "white",      
    legend.bg.alpha = 0.8,          
    legend.frame = TRUE,           
    legend.title.size = .8,
    legend.text.size = .9,  
    legend.title.color = "black",  
    legend.text.color = "black",
    legend.title.fontface = "bold"
  ) 

# Save as SVG
tmap_save(
  tm = pico_rt_burden_map_2,
  filename = file.path(file_path, "Pico/Outputs/pico_rt_burden_map_2.svg"),
  width = 12,
  height = 9,
  units = "in"
)

#save as png
tmap_save(pico_rt_burden_map_2, filename = file.path(file_path, "Pico/Outputs/pico_rt_burden_map_2.png"), width = 12, height = 9, dpi = 300)


## repeating for share severely rent burdened (spending over 50% income on housing)
#making bins
map_50_pct <- renter_burden%>%
  mutate(bin_50_pct = cut(share_over_50, breaks=c(.20, .25, .30, .35, .40, .45),
                          labels = c("less than 25%", "25-30%", "30-35%", "35-40%", "40% or higher"),
                          include.lowest = TRUE))
#make each bin a factor
map_50_pct$bin_50_pct <- factor(map_50_pct$bin_50_pct,
                                levels = c("less than 25%", "25-30%", "30-35%", "35-40%", "40% or higher"))
#plotting
pico_rt_burden_map_3 <- tm_shape(
  shp = map_50_pct,
  bbox = st_bbox(st_buffer(pico_tracts, 1250))
)+
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_polygons(
    col = "bin_50_pct",
    palette = c("#87A0B4", "#1A4378"),
    style = "cat",
    n = 3,
    border.col = "white",
    border.lwd = 0.3,
    title = "Legend\n─────────",
    # popup.format = list(fun = function(x) scales::comma(round(x))),
    # legend.format = list(fun = function(x) scales::comma_format()(x)),
    na.show = FALSE
  )+
  tm_shape(pico_locations_sf) +
  tm_dots(
    col = "#ffffff",
    border.col = "#28381e",
    border.lwd = 2,
    size = 0.4
  ) +
  tm_text(
    "name",
    size = .9,
    just = "center",        
    ymod = -0.5,          
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text(
    "name",
    size = 1,
    col = "black",
    shadow = TRUE,
    auto.placement = TRUE,
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.outside = TRUE,  
    legend.bg.color = "white",      
    legend.bg.alpha = 0.8,          
    legend.frame = TRUE,           
    legend.title.size = .8,
    legend.text.size = .9,  
    legend.title.color = "black",  
    legend.text.color = "black",
    legend.title.fontface = "bold"
  ) 

# Save as SVG
tmap_save(
  tm = pico_rt_burden_map_3,
  filename = file.path(file_path, "Pico/Outputs/pico_rt_burden_map_3.svg"),
  width = 12,
  height = 9,
  units = "in"
)

#save as png
tmap_save(pico_rt_burden_map_3, filename = file.path(file_path, "Pico/Outputs/pico_rt_burden_map_3.png"), width = 12, height = 9, dpi = 300)



####----Disadvantaged Communities----####

#load in CES disadvantaged community data
ces_disadvantaged <- read.xlsx(file.path(file_path, "Data/Disadvantaged/sb535_tract_all_data.xlsx")) %>%
  rename(GEOID = Census.Tract) %>%
  mutate(GEOID = str_pad(GEOID, width = 11, pad = "0"))%>% #need to add a leading zero to match GEOID structure
  filter(GEOID %in% pico_tracts$GEOID) #limiting to pico tracts

### 40 of 46 Pico tracts are disadvantaged

####----Transportation Access and Safety----####
##Car free and one car##                
#pull car data
vehicles <- pico_master_data%>%
  select(GEOID, NAME, contains("B25044"))

vehicles_wide_pico <- vehicles%>%
  rename(
    total = "B25044_001",
    no_car_o = "B25044_003",
    one_car_o = "B25044_004",
    no_car_r = "B25044_010",
    one_car_r = "B25044_011"
  )%>%
  mutate(
    no_car = (no_car_o + no_car_r),
    one_car = (one_car_o + one_car_r),
    share_no_car = (no_car/total),
    share_one_car = (one_car/total),
    multiple_car = (total - no_car - one_car)
  )%>%
  select(GEOID, total, no_car, one_car, share_no_car, share_one_car, multiple_car)

#creating custom bins for a map of share of occupied HU that are renter
map_no_car <- vehicles_wide_pico%>% 
  mutate(bin_no_car = cut(share_no_car, breaks=c(0, 0.1, 0.2,0.3,0.4,0.5,1),
                          labels  = c("Less than 10%", "10-20%", "20-30%", "30-40%", "40-50%", "50% or more"),
                          include.lowest = TRUE))
#making each bin a factor
map_no_car$bin_no_car <- factor(map_no_car$bin_no_car, 
                                levels =c("Less than 10%", "10-20%", "20-30%", "30-40%", "40-50%", "50% or more"))

#plot map
pico_vehicle_map <- tm_shape(
  shp = map_no_car,
  bbox = st_bbox(st_buffer(pico_tracts, 1250))
)+
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_polygons(
    col = "bin_no_car",
    palette = c("#87A0B4", "#1A4378"),
    style = "cat",
    n = 3,
    border.col = "white",
    border.lwd = 0.3,
    title = "Legend\n─────────",
    # popup.format = list(fun = function(x) scales::comma(round(x))),
    # legend.format = list(fun = function(x) scales::comma_format()(x)),
    na.show = FALSE
  )+
  tm_shape(pico_locations_sf) +
  tm_dots(
    col = "#ffffff",
    border.col = "#28381e",
    border.lwd = 2,
    size = 0.4
  ) +
  tm_text(
    "name",
    size = .9,
    just = "center",        
    ymod = -0.5,          
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text(
    "name",
    size = 1,
    col = "black",
    shadow = TRUE,
    auto.placement = TRUE,
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.outside = TRUE,  
    legend.bg.color = "white",      
    legend.bg.alpha = 0.8,          
    legend.frame = TRUE,           
    legend.title.size = .8,
    legend.text.size = .9,  
    legend.title.color = "black",  
    legend.text.color = "black",
    legend.title.fontface = "bold"
  ) 

# Save as SVG
tmap_save(
  tm = pico_vehicle_map,
  filename = file.path(file_path, "Pico/Outputs/pico_vehicle_map_1.svg"),
  width = 12,
  height = 9,
  units = "in"
)

tmap_save(pico_vehicle_map, filename = file.path(file_path, "Pico/Outputs/pico_vehicle_map.png"), width = 12, height = 9, dpi = 300)

### pull travel time data
travel_time <- pico_master_data%>%
  select(GEOID, NAME, contains("B08303"))

travel_time_wide_pico <- travel_time%>%
  rename(
    total = B08303_001,
    commute_5_less = B08303_002,
    commute_5_9 = B08303_003,
    commute_10_14 = B08303_004,
    commute_15_19 = B08303_005,
    commute_20_24 = B08303_006,
    commute_25_29 = B08303_007,
    commute_30_34 = B08303_008,
    commute_35_39 = B08303_009,
    commute_40_44 = B08303_010,
    commute_45_59 = B08303_011,
    commute_60_89 = B08303_012,
    commute_90_plus = B08303_013
  )%>%
  mutate(
    commute_5_14 = commute_5_9 + commute_10_14,
    commute_15_24 = commute_15_19 + commute_20_24,
    commute_25_34 = commute_25_29 + commute_30_34,
    commute_35_44 = commute_35_39 + commute_40_44,
    commute_hour_plus = commute_60_89 + commute_90_plus,
    share_commute_hour_plus = commute_hour_plus/total
  )%>%
  select(
    GEOID, total, commute_5_less, commute_5_14, commute_15_24, commute_25_34, commute_35_44, commute_45_59,
    commute_60_89, commute_90_plus, commute_hour_plus, share_commute_hour_plus
  )

travel_time <- travel_time%>%
  mutate(
    commute_hour_plus = commute_60_89 + commute_90_plus,
    share_commute_hour_plus = commute_hour_plus/total 
  )

#create a df of totals across all tracts and compute shares of each commute bin
travel_time_sums <- colSums(travel_time_wide_pico[, 1:9], na.rm = TRUE)

travel_time_sums <- data.frame(t(travel_time_sums))
#generating shares of transport types
for (i in 2:9) {
  col_name <- names(travel_time_sums)[i]
  new_col_name <- paste0("share_", col_name)
  travel_time_sums[[new_col_name]] <- travel_time_sums[[i]] / travel_time_sums[[1]]
}

#custom bins for share of residents with an hour + commute
map_travel_time <- travel_time_wide_pico%>% 
  mutate(bin_hour_plus = cut(share_commute_hour_plus, breaks=c(0,0.05,0.1,0.15,0.2,0.25,1),
                             labels  = c("Less than 5%", "5-10%", "10-15%", "15-20%", "20-25%", "25% or more"),
                             include.lowest = TRUE))
#making each bin a factor
map_travel_time$bin_hour_plus <- factor(map_travel_time$bin_hour_plus, 
                                        levels = c("Less than 5%", "5-10%", "10-15%", "15-20%", "20-25%", "25% or more"))

#plot map
travel_map_pico <- tm_shape(
  shp = map_travel_time,
  bbox = st_bbox(st_buffer(pico_tracts, 1250))
)+
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_polygons(
    col = "bin_hour_plus",
    palette = c("#87A0B4", "#1A4378"),
    style = "cat",
    n = 3,
    border.col = "white",
    border.lwd = 0.3,
    title = "Legend\n─────────",
    # popup.format = list(fun = function(x) scales::comma(round(x))),
    # legend.format = list(fun = function(x) scales::comma_format()(x)),
    na.show = FALSE
  )+
  tm_shape(pico_locations_sf) +
  tm_dots(
    col = "#ffffff",
    border.col = "#28381e",
    border.lwd = 2,
    size = 0.4
  ) +
  tm_text(
    "name",
    size = .9,
    just = "center",        
    ymod = -0.5,          
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text(
    "name",
    size = 1,
    col = "black",
    shadow = TRUE,
    auto.placement = TRUE,
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.outside = TRUE,  
    legend.bg.color = "white",      
    legend.bg.alpha = 0.8,          
    legend.frame = TRUE,           
    legend.title.size = .8,
    legend.text.size = .9,  
    legend.title.color = "black",  
    legend.text.color = "black",
    legend.title.fontface = "bold"
  ) 

# Save as SVG
tmap_save(
  tm = travel_map_pico,
  filename = file.path(file_path, "Pico/Outputs/travel_map_pico.svg"),
  width = 12,
  height = 9,
  units = "in"
)

tmap_save(travel_map_pico, filename = file.path(file_path, "Pico/Outputs/travel_map_pico.png"), width = 12, height = 9, dpi = 300)

#load in data from TIMS
crashes_pico <- read.csv(file.path(file_path, "Pico/Outputs/crash_data_pico_blvd.csv"))
##Collisions - not sure about best data here
#convert to an SF
crashes_pico_sf <- st_as_sf(
  crashes_pico,
  coords = c("POINT_X", "POINT_Y"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(crs = st_crs(pico_tracts))

#pivot longer
crash_types_pico <- crashes_pico_sf%>%
  select(
    id = CASE_ID,
    geometry,
    ped_acc = PEDESTRIAN_ACCIDENT,
    bike_acc = BICYCLE_ACCIDENT
  ) %>%
  mutate(
    ped_acc = if_else(is.na(ped_acc) | ped_acc == "", "N", as.character(ped_acc)),
    bike_acc = if_else(is.na(bike_acc) | bike_acc == "", "N", as.character(bike_acc)),
    other_acc = if_else(ped_acc != "Y" & bike_acc != "Y", "Y", "N")) %>%
  pivot_longer(cols = c(ped_acc, bike_acc, other_acc), 
               names_to = "accident_type", 
               values_to = "accident_flag") %>%
  filter(accident_flag == "Y")%>%
  mutate(accident_type = case_when(
    accident_type == "ped_acc" ~ "Pedestrian Collision",
    accident_type == "bike_acc" ~ "Bicycle Collision",
    accident_type == "other_acc" ~ "All Other Crashes"
  ))


#statistics for report
table(crash_types_pico$accident_type)

(st_crs(pico_tracts))

#make the map
crash_map_pico <- tm_shape(
  crash_types_pico,
  bbox = st_bbox(st_buffer(pico_tracts, 400))
) +
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_dots(
    col = "accident_type",
    palette = c("Pedestrian Collision" = "#1A4378", "Bicycle Collision" = "#1A7843", "All Other Crashes" = "#78431A"),
    size = 0.35,
    alpha = 0.9,
    border.col = "black",
    border.lwd = 0.5,
    title = "Legend\n─────────"
  ) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),  # moved up
    legend.outside = TRUE,
    legend.bg.color = "white",
    legend.bg.alpha = 0.8,
    legend.frame = TRUE,
    legend.title.size = .8,
    legend.text.size = .9,
    legend.title.color = "black",
    legend.text.color = "black",
    legend.title.fontface = "bold"
  )


#save as PNG
tmap_save(crash_map_pico, filename = file.path(file_path, "Pico/Outputs/crash_map_pico.png"), width = 12, height = 9, dpi = 300)

# Save as SVG
tmap_save(
  tm = crash_map_pico,
  filename = file.path(file_path, "Pico/Outputs/crash_map_pico.svg"),
  width = 12,
  height = 9,
  units = "in"
)


####----H+T Index----####

#pull H+T data
ht_index <- read.csv(file.path(file_path, "Data/htaindex2022_data_tracts_06.csv"))%>%
  rename(GEOID = tract)%>%
  mutate(GEOID = gsub('"', '', GEOID))%>%
  mutate(GEOID = str_pad(GEOID, width = 11, pad = "0"))%>% #need to add a leading zero to match GEOID structure
  filter(GEOID %in% pico_tracts$GEOID) # limiting to pico tracts

#add geometry for map
st_geometry(ht_index) <- st_geometry(pico_tracts[match(ht_index$GEOID, pico_tracts$GEOID), ])
class(ht_index)


### housing and transportation costs as a percentage of income - map ###

#H+T index for AMI
#custom bins
map_ht_percent_income <- ht_index%>% 
  mutate(bin_ht_ami = cut(ht_ami, breaks=c(0,30,40,50,100),
                          labels  = c("Less than 30%", "30-40%", "40-50%", "50% or more"),
                          include.lowest = TRUE))
#making each bin a factor
map_ht_percent_income$bin_ht_ami <- factor(map_ht_percent_income$bin_ht_ami, 
                                           levels = c("Less than 30%", "30-40%", "40-50%", "50% or more"))

#plot map
ht_percent_income_map_pico <- tm_shape(
  shp = map_ht_percent_income,
  bbox = st_bbox(st_buffer(pico_tracts, 1250))
)+
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_polygons(
    col = "bin_ht_ami",
    palette = c("#87A0B4", "#1A4378"),
    style = "cat",
    n = 3,
    border.col = "white",
    border.lwd = 0.3,
    title = "Legend\n─────────",
    # popup.format = list(fun = function(x) scales::comma(round(x))),
    # legend.format = list(fun = function(x) scales::comma_format()(x)),
    na.show = FALSE
  )+
  tm_shape(pico_locations_sf) +
  tm_dots(
    col = "#ffffff",
    border.col = "#28381e",
    border.lwd = 2,
    size = 0.4
  ) +
  tm_text(
    "name",
    size = .9,
    just = "center",        
    ymod = -0.5,          
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text(
    "name",
    size = 1,
    col = "black",
    shadow = TRUE,
    auto.placement = TRUE,
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.outside = TRUE,  
    legend.bg.color = "white",      
    legend.bg.alpha = 0.8,          
    legend.frame = TRUE,           
    legend.title.size = .8,
    legend.text.size = .9,  
    legend.title.color = "black",  
    legend.text.color = "black",
    legend.title.fontface = "bold"
  ) 

# Save as SVG
tmap_save(
  tm = ht_percent_income_map_pico,
  filename = file.path(file_path, "Pico/Outputs/ht_percent_income_map_pico.svg"),
  width = 12,
  height = 9,
  units = "in"
)

tmap_save(ht_percent_income_map_pico, filename = file.path(file_path, "Pico/Outputs/ht_percent_income_map_pico.png"), width = 12, height = 9, dpi = 300)


#Repeating using H+T index for 80% AMI
map_ht_80percent_income <- ht_index%>% 
  mutate(bin_ht_80ami = cut(ht_80ami, breaks=c(0,30,40,50,60,100),
                            labels  = c("Less than 30%", "30-40%", "40-50%", "50-60%", "60% or more"),
                            include.lowest = TRUE))
#making each bin a factor
map_ht_80percent_income$bin_ht_80ami <- factor(map_ht_80percent_income$bin_ht_80ami, 
                                               levels = c("Less than 30%", "30-40%", "40-50%", "50-60%", "60% or more"))

#plot map
#plot map
ht_percent_income_map_pico_1 <- tm_shape(
  shp = map_ht_80percent_income,
  bbox = st_bbox(st_buffer(pico_tracts, 1250))
)+
  tm_basemap("Esri.WorldStreetMap", alpha = 0.5) +
  tm_polygons(
    col = "bin_ht_80ami",
    palette = c("#87A0B4", "#1A4378"),
    style = "cat",
    n = 3,
    border.col = "white",
    border.lwd = 0.3,
    title = "Legend\n─────────",
    # popup.format = list(fun = function(x) scales::comma(round(x))),
    # legend.format = list(fun = function(x) scales::comma_format()(x)),
    na.show = FALSE
  )+
  tm_shape(pico_locations_sf) +
  tm_dots(
    col = "#ffffff",
    border.col = "#28381e",
    border.lwd = 2,
    size = 0.4
  ) +
  tm_text(
    "name",
    size = .9,
    just = "center",        
    ymod = -0.5,          
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text(
    "name",
    size = 1,
    col = "black",
    shadow = TRUE,
    auto.placement = TRUE,
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.outside = TRUE,  
    legend.bg.color = "white",      
    legend.bg.alpha = 0.8,          
    legend.frame = TRUE,           
    legend.title.size = .8,
    legend.text.size = .9,  
    legend.title.color = "black",  
    legend.text.color = "black",
    legend.title.fontface = "bold"
  ) 

# Save as SVG
tmap_save(
  tm = ht_percent_income_map_pico_1,
  filename = file.path(file_path, "Pico/Outputs/ht_percent_income_map_pico_1.svg"),
  width = 12,
  height = 9,
  units = "in"
)

tmap_save(ht_percent_income_map_pico_1, filename = file.path(file_path, "Pico/Outputs/ht_percent_income_map_pico_1.png"), width = 12, height = 9, dpi = 300)

####-Creating a Zoning Map---####
library(lwgeom)
zoning_data <- st_read(file.path(file_path, "Data/Zoning/Zoning.shp"))

#set so that CRS are the same
pico_tracts <- st_transform(pico_tracts, crs = 4326)
zoning_data <- st_transform(zoning_data, crs = 4326)

#make geometries valid
zoning_data <- st_make_valid(zoning_data)
pico_tracts <- st_make_valid(pico_tracts)

zoning_pico <- st_filter(zoning_data, pico_tracts)


zone_levels <- c(
  "Agricultural", "Commercial", "Commercial-Mixed", "Industrial",
  "Industrial-Mixed", "Industrial-Mixed & Manufacturing",
  "Multiple Family Residential", "Open Space", "Public",
  "Residential-Mixed", "Single-Family Residential"
)

zoning_pico$CATEGORY <- factor(zoning_pico$CATEGORY, levels = zone_levels)

pico_zoning_map <- 
  tm_shape(
    shp = zoning_pico,
    bbox = st_bbox(st_buffer(pico_tracts, 1500))
  )+
  tm_basemap("Esri.WorldStreetMap", alpha = 0.3) +
  tm_polygons(
    col = "CATEGORY",
    palette = "Set3",  # or a custom vector of colors if needed
    title = "Legend\n─────────",
    border.col = "white",
    border.alpha = 0.1
  ) +
  tm_shape(pico_locations_sf) +
  tm_dots(
    col = "#ffffff",
    border.col = "#28381e",
    border.lwd = 2,
    size = 0.4
  ) +
  tm_text(
    "name",
    size = .9,
    just = "center",        
    ymod = -0.5,          
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
  tm_shape(pico_blvd) +
  tm_lines(col = "#F4AB0B", lwd = 4, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text(
    "name",
    size = 1,
    col = "black",
    shadow = TRUE,
    auto.placement = TRUE,
    fontface = "bold",
    fontfamily = "Calibri"
  )+
  tm_add_legend(
    type = "line",
    labels = c("Project Site", "Project Study Area"),
    col = c("#F4AB0B", "#9e400f"),
    lty = c("solid", "dashed"),
    lwd = c(2.5, 2),
    title = ""
  )+
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.outside = TRUE,  
    legend.bg.color = "white",      
    legend.bg.alpha = 0.8,          
    legend.frame = TRUE,           
    legend.title.size = .8,
    legend.text.size = .9,  
    legend.title.color = "black",  
    legend.text.color = "black",
    legend.title.fontface = "bold"
  ) 

#save as svg
tmap_save(
  tm = pico_zoning_map,
  filename = file.path(file_path, "Pico/Outputs/pico_zoning_map.svg"),
  width = 12,
  height = 9,
  units = "in"
)

#save as png
tmap_save(pico_zoning_map, filename = file.path(file_path, "Pico/Outputs/pico_zoning_map.png"), width = 12, height = 9, dpi = 300)


####----Measure Voting Data---####
#load the data
hla_vote <- st_read(file.path(file_path, "Data/Measure HLA Vote/HLA.shp"))

#set tracts crs to match that of voting data
pico_buffer_tracts_1 <- st_transform(pico_buffer_tracts, crs = 4326)
hla_vote <- st_transform(hla_vote, crs = 4326)


#find intersection
voting_intersection <- st_intersection(pico_buffer_tracts_1, hla_vote)


voting_map_pico <- ggplot() +
  geom_sf(data = pico_buffer_tracts_1) +
  geom_sf(data = voting_intersection, aes(fill = V3_LOS_A_5), color = NA) +
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) + #adding streets to map
  scale_fill_gradientn(
    colors = palette_urbn_cyan[c(2, 4, 6, 8)], 
    name = "Share of Votes in Favor",
    labels = scales::percent_format()
  ) +
  theme_urbn_map() +  
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(1.2, "cm")
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(8, "cm"),
      barheight = unit(0.6, "cm")
    )
  )


ggsave(file.path(file_path, "Pico/Outputs/voting_map_pico.png"), width = 14, height = 6, dpi = 300)



