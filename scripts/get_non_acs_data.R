library(tidyverse)
library(sf)
library(janitor)

#' Get non-ACS data for a boulevard area
#'
#' @param base_path The base path to the data directory.
#' @param dataset_name The name of the dataset to retrieve. Options include "affordable_housing", "senior_housing", "zoning", "h_and_t", "crash", and "hla".
#' @param area The area for which to retrieve data. Options include "LA", "Pico", "MLK", and "Hoover". 
#'
#' @return A spatial data frame containing the requested dataset filtered to the specified area.
get_non_acs_data = function(
    base_path,
    dataset_name = c("affordable_housing", "senior_housing", "zoning", "h_and_t", "crash", "hla"),
    area = c("LA", "Pico", "MLK", "Hoover")) {

  if (area == "LA") file_path = file.path(base_path, "Data", "LA tracts", "LA_City_2020_Census_Tracts_.shp")
  if (area != "LA") file_path = file.path(base_path, "Data", "Mapping", "Boulevards", area, str_c(area, "_buffer_tracts.shp"))

  tracts_sf = st_read(file_path, quiet = TRUE) %>%
    st_transform(4326) %>%
    st_make_valid()

  if (dataset_name == "senior_housing") {
    result <- st_read(file.path(base_path, "Data/Senior_Housing-shp/Senior_Housing.shp")) %>%
      st_transform(4326) %>%
      st_make_valid() %>%
      st_filter(tracts_sf) }
  
  if (dataset_name == "affordable_housing") {
    result <- st_read(file.path(base_path, "Data/Affordable_Housing_Development/Affordable_Housing_Development.shp")) %>%
      st_transform(4326) %>%
      st_make_valid() %>%
      st_filter(tracts_sf) }
  
  if (dataset_name == "zoning") {
    ## these data are large, so we process and save to disk
    ## if they already are available on disk, we read from there rather than reprocessing
    if (area %in% st_layers(file.path(base_path, "Data", "Zoning", "zoning_simplified.gpkg"))$name) {
      
      result = st_read(
        file.path(base_path, "Data", "Zoning", "zoning_simplified.gpkg"), 
        layer = area, 
        quiet = TRUE) %>%
        st_transform(4326) %>%
        st_make_valid() 
      
    } else {
      
      result1 <- st_read(file.path(base_path, "Data/Zoning/Zoning.shp")) %>%
        st_transform(4326) %>%
        st_make_valid() 
      
      buffer_area = tracts_sf %>% st_bbox() %>% st_as_sfc()
      
      result2 = result1 %>%
        st_filter(buffer_area %>% st_transform(4326)) %>%
        st_intersection(tracts_sf %>% st_transform(4326)) %>%
        janitor::clean_names() %>%
        mutate(
          zoning_category = case_when(
            category %in% c("Commercial", "Manufacturing", "Industrial", "Commercial-Mixed", "Industrial-Mixed") ~ "Commercial or Industrial",
            category %in% c("Public", "Public Facilities") ~ "Public",
            category %in% c("Parking", "Freeway") ~ "Highways or Parking",
            category %in% c("Agricultural", "Open Space") ~ "Agricultural or Open Space",
            str_detect(category, "Multi.* Resi") ~ "Multi-family Residential",
            str_detect(category, "Single.* Resi") ~ "Single-family Residential",
            TRUE ~ category) %>%
            factor(
              levels = c(
                "Single-family Residential", "Multi-family Residential", "Public",
                "Agricultural or Open Space", "Commercial or Industrial", 
                "Highways or Parking")))
      
      if (result2 %>% filter(is.na(zoning_category)) %>% nrow() != 0) {
        stop("Some zoning categories have not been categorized. Update code in `get_non_acs_data.R`")}
      
      result3 = result2 %>%
        group_by(zoning_category) %>%
        summarize()
      
      st_write(
        result3, 
        dsn = file.path(base_path, "Data", "Zoning", "zoning_simplified.gpkg"), 
        layer = area, 
        delete_dsn = FALSE, 
        delete_layer = TRUE,
        quiet = TRUE)
      
      result = result3 }}
  
  if (dataset_name == "h_and_t") {
    result = read_csv(file.path(base_path, "Data/htaindex2022_data_tracts_06.csv")) %>%
      rename(GEOID = tract) %>%
      mutate(GEOID = gsub('"', '', GEOID) %>% str_pad(width = 11, pad = "0", side = "left")) %>%
      filter(GEOID %in% tracts_sf$GEOID) %>% 
      mutate(
        bin_ht_ami = cut(
            ht_ami, 
            breaks=c(0,30,40,50,100),
            labels  = c("Less than 30%", "30-40%", "40-50%", "50% or more"),
            include.lowest = TRUE,
            ordered_result = TRUE),
        bin_ht_80ami = cut(
            ht_80ami, 
            breaks=c(0,30,40,50,60,100),
            labels  = c("Less than 30%", "30-40%", "40-50%", "50-60%", "60% or more"),
            include.lowest = TRUE,
            ordered_result = TRUE)) %>%
      select(GEOID, matches("bin"), everything()) }
  
  if (dataset_name == "crash") {
    result <- read_csv(file.path(base_path, "Pico/Outputs/crash_data_pico_blvd.csv")) %>%
     st_as_sf(
       coords = c("POINT_X", "POINT_Y"),
       crs = 4326,
       remove = FALSE) %>%
      st_transform(4326) %>%
      st_make_valid() %>%
      st_filter(tracts_sf) %>%
      select(
        id = CASE_ID,
        pedestrian_crash = PEDESTRIAN_ACCIDENT,
        bike_crash = BICYCLE_ACCIDENT) %>%
      mutate(
        pedestrian_involved_crash = if_else(is.na(pedestrian_crash) | pedestrian_crash == "", "N", as.character(pedestrian_crash)),
        bicyclist_invovled_crash = if_else(is.na(bike_crash) | bike_crash == "", "N", as.character(bike_crash)),
        other_crash_types = if_else(pedestrian_crash != "Y" & bike_crash != "Y", "Y", "N")) %>%
      pivot_longer(
        cols = matches("crash"), 
        names_to = "crash_type", 
        values_to = "crash_flag") %>%
      filter(crash_flag == "Y")%>%
      mutate(
        crash_type = crash_type %>% str_replace_all("_", " ") %>% str_to_sentence()) }
  
  if (dataset_name == "hla") {
    result = st_read(file.path(base_path, "Data/Measure HLA Vote/HLA.shp")) %>%
      st_make_valid(hla_vote) %>%
      st_transform(crs = 4326) %>%
      st_filter(tracts_sf) }
  
  return(result)
}