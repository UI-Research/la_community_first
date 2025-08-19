library(tidyverse)
library(sf)
library(janitor)

#' Get non-ACS data for a boulevard area
#'
#' @param base_path The base path to the data directory.
#' @param dataset_name The name of the dataset to retrieve. Options include "affordable_housing", "senior_housing", "zoning", "h_and_t", "crash", and "hla".
#' @param area The area for which to retrieve data. Options include "LA", "Pico", "mlk", and "Hoover". 
#'
#' @return A spatial data frame containing the requested dataset filtered to the specified area.
get_non_acs_data = function(
    base_path,
    dataset_name = c("affordable_housing", "senior_housing", "zoning", "h_and_t", "crash", "hla"),
    area = c("LA", "pico", "mlk", "hoover")) {
  
  ## used for filtering crash data
  primary_road_string = "pico"
  if (area == "mlk") primary_road_string = "MARTIN LUTHER|MLK"
  if (area == "hoover") primary_road_string = "HOOVER"
  
  if (area == "LA") file_path = file.path(base_path, "Data", "LA tracts", "LA_City_2020_Census_Tracts_.shp")
  if (area != "LA") file_path = file.path(file_path, boulevard, "maps", str_c(boulevard, "_tracts.shp"))

  tracts_sf = st_read(file_path, quiet = TRUE) %>%
    st_set_crs(4326) %>% 
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
       st_filter(tracts_sf) 
    }
  
  if (dataset_name == "zoning") {
    ## these data are large, so we process and save to disk
    ## if they already are available on disk, we read from there rather than reprocessing
    # if (area %in% st_layers(file.path(base_path, "Data", "Zoning", "zoning_simplified.gpkg"))$name) {
    #   
    #   result = st_read(
    #     file.path(base_path, "Data", "Zoning", "zoning_simplified.gpkg"), 
    #     layer = area, 
    #     quiet = TRUE) %>%
    #     st_transform(4326) %>%
    #     st_make_valid() %>%
    #     mutate(
    #       zoning_category = factor(zoning_category, levels = c(
    #         "Agricultural or Open Space",
    #         "Highways or Parking",
    #         "Commercial or Industrial",
    #         "Multi-family Residential",
    #         "Single-family Residential",
    #         "Public")))
    # 
    # } else {
    # browser()
   
      result1 <- st_read(file.path(base_path, "Data/Zoning/Zoning.shp")) %>%
        st_transform(4326) %>%
        st_make_valid() 
      buffer_area = tracts_sf %>% st_bbox() %>% st_as_sfc()
      
      result <- st_read(file.path(base_path, "Data/Zoning/samo_Zoning.shp")) %>% 
        rename(zoning_category = znng_ct) %>% 
        st_filter(buffer_area %>% st_transform(4326)) %>%
        st_intersection(tracts_sf %>% st_transform(4326))

      result2 = result1 %>%
        st_filter(buffer_area %>% st_transform(4326)) %>%
        st_intersection(tracts_sf %>% st_transform(4326)) %>%
        janitor::clean_names() %>%
        mutate(
          zoning_category = case_when(
            category %in% c("Commercial", "Manufacturing", "Industrial", "Commercial-Mixed", "Industrial-Mixed", "Hybrid Industrial") ~ "Commercial or Industrial",
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
                "Highways or Parking"))) %>% 
        bind_rows(result)
      
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
      
      result = result3 }
  #}
  
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
    result <- read_csv(file.path(base_path, area, "Outputs", paste0(area, "_crashes.csv"))) %>%
      filter(!is.na(POINT_X), !is.na(POINT_Y)) %>%
      st_as_sf(
       coords = c("POINT_X", "POINT_Y"),
       crs = 4326,
       remove = FALSE) %>%
      st_transform(4326) %>%
      st_make_valid() %>%
      st_filter(tracts_sf) %>%
      janitor::clean_names() %>%
      filter(
        ## only crashes on the primary street
        #str_detect(primary_rd, primary_road_string),
        ## only crashes that aren't on a state highway
        state_hwy_ind == "N") %>%
      transmute(
        id = case_id, 
        pedestrian_involved_crash = pedestrian_accident,
        bicyclist_invovled_crash = bicycle_accident,
        other_vehicle_crash = if_else(
          is.na(pedestrian_involved_crash) & is.na(bicyclist_invovled_crash), "Y", NA)) %>%
      pivot_longer(matches("crash")) %>%
      filter(value == "Y") %>%
      transmute(
        id,
        crash_type = name %>% str_replace_all(c("_" = " ")) %>% str_to_sentence(),
      ) %>% 
      mutate(crash_type = case_when(
        crash_type == "Pedestrian involved crash" ~ "Pedestrian involved severe crash", 
        crash_type == "Bicyclist invovled crash" ~ "Bicyclist involved severe crash", 
        crash_type == "Other vehicle crash" ~ "Other vehicle severe crash"
      )
      )  
  }
  
  if (dataset_name == "crash_full") {
    result <- read_csv(file.path(base_path, area, "Outputs", paste0(area, "_crashes_study_area.csv"))) %>%
      filter(!is.na(POINT_X), !is.na(POINT_Y)) %>%
      st_as_sf(
        coords = c("POINT_X", "POINT_Y"),
        crs = 4326,
        remove = FALSE) %>%
      st_transform(4326) %>%
      st_make_valid() %>%
      st_filter(tracts_sf) %>%
      janitor::clean_names() %>%
      filter(
        ## only crashes that aren't on a state highway
        state_hwy_ind == "N") %>%
      transmute(
        id = case_id, 
        pedestrian_involved_crash = pedestrian_accident,
        bicyclist_invovled_crash = bicycle_accident,
        other_vehicle_crash = if_else(
          is.na(pedestrian_involved_crash) & is.na(bicyclist_invovled_crash), "Y", NA)) %>%
      pivot_longer(matches("crash")) %>%
      filter(value == "Y") %>%
      transmute(
        id,
        crash_type = name %>% str_replace_all(c("_" = " ")) %>% str_to_sentence(),
        ) %>% 
      mutate(crash_type = case_when(
        crash_type == "Pedestrian involved crash" ~ "Pedestrian involved severe crash", 
        crash_type == "Bicyclist invovled crash" ~ "Bicyclist involved severe crash", 
        crash_type == "Other vehicle crash" ~ "Other vehicle severe crash"
      )
      #,
      # crash_type = factor(crash_type, levels = c(
      #   "Pedestrian involved severe crash",
      #   "Bicyclist involved severe crash",
      #   "Other vehicle severe crash"))
      )  
  }

  if (dataset_name == "hla") {
    result = st_read(file.path(base_path, "Data/Measure HLA Vote/HLA.shp")) %>%
      st_make_valid() %>%
      st_transform(crs = 4326) %>%
      st_filter(tracts_sf) %>%
      transmute(
        hla_total_vote_count = V3_LOS_A_4,
        hla_affirmative_vote_count = V3_LOS_A_2,
        hla_affirmative_vote_share = V3_LOS_A_5) }
  
  return(result)
}