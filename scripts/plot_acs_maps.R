library(tidyverse)
library(sf)

source(here("scripts", "plot_boulevard_map.R"))

#' Iteratively plot ACS maps for a boulevard study area
#'
#' @param sf A spatial data frame (polygon) containing the tract-level ACS data to be visualized.
#' @param boulevard_sf A spatial data frame (polyline) representing the boulevard, used for highlighting in the map.
#' @param locations_sf A spatial data frame (points) containing locations of interest, such as parks or schools, to be plotted on the map.
#' @param study_area_outline_sf A spatial data frame (polygon) representing the outline of the study area, used for context in the map.
#' @param streets_sf A spatial data frame (point) containing street names, used for labeling streets in the map.
#' @param fill_column The name of the column in `sf` that contains the data to be visualized as a choropleth.
#' @param legend_title The title for the legend in the map.
#' @param save A logical indicating whether to save the map to file. If `TRUE`, the map will be saved using the specified `file_extension`.
#' @param file_extension The file extension to use when saving the map, e.g., ".png", ".svg". Defaults to ".png".
#' @param outpath The path where the map will be saved.
#'
#' @return A list of tmap objects, each representing a choropleth map for a specific ACS variable.
plot_acs_maps = function(
    sf,
    boulevard_sf,
    locations_sf,
    study_area_outline_sf,
    streets_sf,
    fill_column,
    legend_title,
    save = FALSE,
    file_extension = ".png",
    outpath) {
  
  english_less_than_very_well_denominator_vars = c(
    "C16001_005", "C16001_020", "C16001_023", "C16001_008", "C16001_011", 
    "C16001_014", "C16001_017", "C16001_026", "C16001_029", "C16001_032", 
    "C16001_035", "C16001_038")
  
  ####----Calculating Percentage Variables to be Plotted----####
  #sf = acs_data_boulevard
  sf = sf %>%
    rename(
      commute_total = B08303_001,
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
      commute_90_plus = B08303_013) %>%
    mutate(
      ## disability
      total_disability = B18101_004 + B18101_007 + B18101_010 + B18101_013 +
        B18101_016 + B18101_019 + B18101_023 + B18101_026 +
        B18101_029 + B18101_032 + B18101_035 + B18101_038,
      share_disability = total_disability / B18101_001,
      # bin_disability = 
      #   cut(
      #     total_disability, 
      #     breaks = c(0,200, 300, 400, 500,800),
      #     labels  = c("Less than 200", "200-300", "300-400", "400-500", "500 or more"),
      #     include.lowest = TRUE,
      #     ordered_result = TRUE),
      # total_hearing_diff = B18102_004 + B18102_007 + B18102_010 + B18102_013 +
      #   B18102_016 + B18102_019 + B18102_023 + B18102_026 +
      #   B18102_029 + B18102_032 + B18102_035 + B18102_038,
      # share_hearing_diff = total_hearing_diff / B18102_001,
      # bin_hearing_diff = 
      #   cut(
      #     total_hearing_diff, 
      #     breaks = c(0,50,100,150,99999),
      #     labels = c("Less than 50", "50-100", "100-150", "150 or more"),
      #     include.lowest = TRUE,
      #     ordered_result = TRUE),
      # total_vision_diff = B18103_004 + B18103_007 + B18103_010 + B18103_013 +
      #   B18103_016 + B18103_019 + B18103_023 + B18103_026 +
      #   B18103_029 + B18103_032 + B18103_035 + B18103_038,
      # share_vision_diff = total_vision_diff / B18103_001,
      # bin_vision_diff = 
      #   cut(
      #     total_vision_diff, 
      #     breaks = c(0,50,100,150,99999),
      #     labels = c("Less than 50", "50-100", "100-150", "150 or more"),
      #     include.lowest = TRUE,
      #     ordered_result = TRUE),
      # total_ambulatory_diff = B18105_004 + B18105_007 + B18105_010 + B18105_013 + 
      #   B18105_016 + B18105_020 + B18105_023 + B18105_026 + B18105_029 + B18105_032,
      # share_ambulatory_diff = total_ambulatory_diff / B18105_001,
      # bin_ambulatory_diff = 
      #   cut(
      #     total_ambulatory_diff, breaks=c(0,50,100,200, 300,9999),
      #     labels  = c("Fewer than 50", "50-100", "100-200", "200-300", "300 or more"),
      #     include.lowest = TRUE,
      #     ordered_result = TRUE),
      ## poverty
      share_below_poverty = B17001_002 / B17001_001,
      # pov_bin = case_when(
      #   share_below_poverty < 10 ~ "Less than 10%",
      #   share_below_poverty >= 10 & share_below_poverty < 20 ~ "10–20%",
      #   share_below_poverty >= 20 & share_below_poverty < 30 ~ "20–30%",
      #   share_below_poverty >= 30 ~ "30% or more",
      #   TRUE ~ NA_character_) %>%
        # factor(levels = c("Less than 10%", "10–20%", "20–30%", "30% or more")),
      share_no_internet = B28002_013/B28002_001,
      # bin_no_internet = 
      #   cut(
      #     share_no_internet, 
      #     breaks = c(0,0.05, 0.1, 0.15,0.2,1),
      #     labels = c("Less than 5%", "5-10%", "10-15%", "15-20%", "20% or more"),
      #     include.lowest = TRUE,
      #     ordered_result = TRUE),
      share_owner_occupied = B25003_002/B25003_001,
      share_renter_occupied = B25003_003/B25003_001,
      # bin_tenure = 
      #   cut(
      #     share_renter_occupied, 
      #     breaks = c(0.5, 0.6, 0.7,0.8,0.9,1),
      #     labels  = c("50-60%", "60-70%", "70-80%", "80-90%", "90-100%"),
      #     include.lowest = TRUE,
      #     ordered_result = TRUE),
      share_no_computer = B28001_011/B28001_001,
      # bin_no_computer = cut(
      #   share_no_computer, 
      #   breaks = c(0,0.01,0.05,0.1,1),
      #   labels  = c("Less than 1%", "1-5%", "5-10%", "10% or higher"),
      #   include.lowest = TRUE,
      #   ordered_result = TRUE),
      commute_5_14 = commute_5_9 + commute_10_14,
      commute_15_24 = commute_15_19 + commute_20_24,
      commute_25_34 = commute_25_29 + commute_30_34,
      commute_35_44 = commute_35_39 + commute_40_44,
      commute_hour_plus = commute_60_89 + commute_90_plus,
      share_commute_hour_plus = commute_hour_plus / commute_total,
      # bin_hour_plus = cut(
      #   share_commute_hour_plus, 
      #   breaks = c(0,0.05,0.1,0.15,0.2,0.25,1),
      #   labels  = c("Less than 5%", "5-10%", "10-15%", "15-20%", "20-25%", "25% or more"),
      #   include.lowest = TRUE,
      #   ordered_result = TRUE),
      no_car = (B25044_003 + B25044_010),
      one_car = (B25044_004 + B25044_011),
      share_no_car = (no_car/B25044_001),
      share_one_car = (one_car/B25044_001),
      multiple_car = (B25044_001 - no_car - one_car),
      # bin_no_car = 
      #   cut(
      #     share_no_car, 
      #     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 1),
      #     labels  = c("Less than 10%", "10-20%", "20-30%", "30-40%", "40-50%", "50% or more"),
      #     include.lowest = TRUE,
      #     ordered_result = TRUE),
      share_renter_housing_costburden_30plus = (B25070_007 + B25070_008 + B25070_009 + B25070_010) / B25070_001,
      share_renter_housing_costburden_50plus = B25070_010 / B25070_001,
      english_less_than_very_well_denominator = rowSums(
        select(st_drop_geometry(.), all_of(english_less_than_very_well_denominator_vars)), na.rm = TRUE),
      share_speak_spanish_at_home = C16001_005 / english_less_than_very_well_denominator,
      share_speak_korean_at_home = C16001_020 / english_less_than_very_well_denominator,
      B25071_001 = B25071_001 / 100) # median share of income spent on housing, adjusting to decimal point for consistency
      # bin_renter_housing_costburden_30plus = cut(
      #   share_renter_housing_costburden_30plus, 
      #   breaks = c(.30, .40, .50, .60, .70, .80),
      #   labels = c("less than 40%", "40-50%", "50-60%", "60-70%", "70-80%"),
      #   include.lowest = TRUE,
      #   ordered_result = TRUE),
      # bin_renter_housing_costburden_50plus = cut(
      #   share_renter_housing_costburden_50plus, 
      #   breaks = c(.20, .25, .30, .35, .40, .45),
      #   labels = c("less than 25%", "25-30%", "30-35%", "35-40%", "40% or higher"),
      #   include.lowest = TRUE,
      #   ordered_result = TRUE),
      # bin_median_share_income_housing_cost = cut(
      #   B25071_001, 
      #   breaks = c(25, 30, 35, 40, 45, 50),
      #   labels = c("less than 30%", "30-35%", "35-40%", "40-45%", "45-50%"),
      #   include.lowest = TRUE,
      #   ordered_result = TRUE))
      
  ####----Iteratively Plotting----####
  plot_metadata = data.frame(
    fill_column = fill_column,
    legend_title = legend_title) %>% 
    mutate(
      style = if_else(str_detect(fill_column, "bin"), "cat", "cont"),
      map_type = "choropleth")

  ## this iterates over each row in `plot_metadata` and applies each of the columns 
  ## from `plot_metadata` as arguments to the parameter of the same name in `map_variable()`
  results = pmap(
    plot_metadata,
    plot_boulevard_map,
    sf = sf,
    locations_sf = locations_sf,
    boulevard_sf = boulevard_sf,
    study_area_outline_sf = study_area_outline_sf,
    streets_sf = streets_sf,
    save = save,
    file_extension = file_extension,
    outpath = outpath)

  names(results) = plot_metadata$legend_title %>% janitor::make_clean_names()
 
  return(results)

}