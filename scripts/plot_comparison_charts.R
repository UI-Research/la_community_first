####----Dependencies and Setup----####
library(tidyverse)
library(sf)
library(janitor)
library(here)
library(skimr)
library(lwgeom)

source(here("scripts", "plot_utility_functions.R"))

#' A function to plot comparison charts for demographic and socioeconomic indicators
#'
#' @param df_boulevard A data frame containing ACS data for a specific boulevard.
#' @param df_city A data frame containing ACS data for the entire city of Los Angeles.
#' @param boulevard_name A string of the name of the boulevard of interest
#' @param color A color palette 
#' @param constructs A character vector of demographic or socioeconomic constructs to be plotted.
#' @param denominators A character vector of column names in `df_boulevard` and `df_city` that will be used as denominators for calculating shares.
#' @param save A logical indicating whether to save the map to file. If `TRUE`, the map will be saved using the specified `file_extension`.
#' @param file_extension The file extension to use when saving the map, e.g., ".png", ".svg". Defaults to ".png".
#' @param outpath The path where the map will be saved.
#'
#' @return A list of ggplot objects, each representing a comparison chart for a specific demographic or socioeconomic indicator.
plot_comparison_charts = function(
    df_boulevard, 
    df_city,
    boulevard_name,
    color, 
    constructs, 
    denominators,
    save = FALSE,
    file_extension = ".png",
    outpath) {
    
  #make sure everything is in Arial font per LA DOT style guide
  theme_set(theme_minimal(base_family = "Arial"))
  ####----Income----####
  ## Note: these should map to survey categories to the extent possible, which have 
  ## upper breaks at: 30, 60, 100, 150, 200, and infinite
  income_vars = list(
    income_0_30 = str_c("B19001_00", c(2:6)),
    income_30_60 = c("B19001_007", "B19001_008", "B19001_009", "B19001_010", "B19001_011"),
    income_60_100 = str_c("B19001_0", c(12:13)),
    income_100_150 = str_c("B19001_0", c(14:15)),
    income_150_200 = "B19001_016",
    income_200plus = "B19001_017")
  income_labels = c(
    "share_income_0_30" = "Less than $30,000",
    "share_income_30_60" = "$30,000 – $59,999",
    "share_income_60_100" = "$60,00 – $99,999",
    "share_income_100_150" = "$100,000 – $149,999",
    "share_income_150_200" = "$150,000 – $199,999",
    "share_income_200plus" = "$200,000 or more")
  income_bar_order = c(
    "Less than $30,000", 
    "$30,000 – $59,999", 
    "$60,00 – $99,999", 
    "$100,000 – $149,999", 
    "$150,000 – $199,999", 
    "$200,000 or more")
  
  ####----Age----####
  age_vars <- list(
    age_0_17  = c("B01001_003", "B01001_004", "B01001_005", "B01001_006",
                  "B01001_027", "B01001_028", "B01001_029", "B01001_030"),
    age_18_29 = c("B01001_007", "B01001_008", "B01001_009", "B01001_010", "B01001_011",  
                  "B01001_031", "B01001_032", "B01001_033", "B01001_034", "B01001_035"),
    age_30_44 = c("B01001_012", "B01001_013", "B01001_014", 
                  "B01001_036", "B01001_037", "B01001_038"),
    age_45_59 = c("B01001_015", "B01001_016", "B01001_017", 
                  "B01001_039", "B01001_040", "B01001_041"),
    age_60_up = c("B01001_018", "B01001_019", "B01001_020", "B01001_021", "B01001_022", "B01001_023", 
                  "B01001_024", "B01001_025", "B01001_042", "B01001_043", "B01001_044", "B01001_045", 
                  "B01001_046", "B01001_047", "B01001_048", "B01001_049"))
  age_labels <- c(
    "share_age_0_17"  = "0–17",
    "share_age_18_29" = "18–29",
    "share_age_30_44" = "30–44",
    "share_age_45_59" = "45–59",
    "share_age_60_up" = "60+")
  age_bar_order <- c("0–17", "18–29", "30–44", "45–59", "60+")
  
  ####----Ethnicity----####
  ethnicity_vars <- list(
    white = "B03002_003",
    black = "B03002_004",
    indigenous = "B03002_005",
    asian = "B03002_006",
    pacific = "B03002_007",
    other = "B03002_010",
    hispanic = "B03002_012")
  ethnicity_labels <- c(
    "share_white" = "White",
    "share_black" = "Black",
    "share_indigenous" = "Indigenous",
    "share_asian" = "Asian",
    "share_pacific" = "Pacific Islander",
    "share_other" = "Other",
    "share_hispanic" = "Hispanic")
  ethnicity_bar_order <- c("Hispanic", "White", "Asian", "Black", "Other", "Indigenous", "Pacific Islander")
  
  ####----Gender----####
  gender_vars <- list(
    male = "B01001_002",
    female = "B01001_026")
  gender_labels <- c(
    "share_male" = "Male",
    "share_female" = "Female")
  gender_bar_order <- c("Male", "Female")
  
  ####----Family Structure----####
  family_vars <- list(
    married_children = "B11003_003",
    married_no_children = "B11003_007",
    single_children = c("B11003_010", "B11003_016"),
    single_no_children = c("B11003_014", "B11003_020"))
  family_labels <- c(
    "share_married_children" = "Married with children",
    "share_married_no_children" = "Married without children",
    "share_single_children" = "Single parent with children",
    "share_single_no_children" = "Single without children")
  family_bar_order <- c("Married with children", "Married without children", "Single parent with children", "Single without children")
  
  ####----English Speaking Ability----####
  english_vars <- list(
    very_well = c("B16004_003", "B16004_025","B16004_047", "B16004_005", "B16004_010", "B16004_015", "B16004_020", "B16004_027", "B16004_032", "B16004_037", "B16004_042", "B16004_049", "B16004_054", "B16004_059", "B16004_064"),
    well = c("B16004_006", "B16004_011", "B16004_016", "B16004_021", "B16004_028", "B16004_033", "B16004_038", "B16004_043", "B16004_050", "B16004_055", "B16004_060", "B16004_065"),
    not_well = c("B16004_007", "B16004_012", "B16004_017", "B16004_022", "B16004_029", "B16004_034", "B16004_039", "B16004_044", "B16004_051", "B16004_056", "B16004_061", "B16004_066"),
    none =c("B16004_008", "B16004_013", "B16004_018", "B16004_023", "B16004_030", "B16004_035", "B16004_040", "B16004_045", "B16004_052", "B16004_057", "B16004_062", "B16004_067"))
  english_labels <- c(
    "share_very_well" = "Native or Very Well",
    "share_well" = "Well",
    "share_not_well" = "Not well",
    "share_none" = "None")
  english_bar_order <- c("Native or Very Well", "Well", "Not well", "None")
  
  ####----Primary Language----####
  #(we specifically care about the languages spoken for people who do not speak english well)
  #note that you may have to change which languages to display on the chart depending on which languages are most common in each study area
  language_vars <- list(
    spanish = "C16001_005",
    korean = "C16001_020",
    chinese = "C16001_023",
    french = "C16001_008",
    #tagalog = "C16001_029",
    #arabic = "C16001_035",
    #russian = "C16001_014",
    other_indo = "C16001_017",
    other = c("C16001_011",
             # "C16001_008",
              "C16001_014",
             # "C16001_017",
              "C16001_026",
              "C16001_029",
              "C16001_032",
              "C16001_035",
              "C16001_038")
    )
  language_labels <- c(
    "share_spanish" = "Spanish",
    "share_korean" = "Korean",
    "share_chinese" = "Chinese",
    "share_french" = "French",
    "share_tagalog" = "Tagalog",
    "share_arabic" = "Arabic",
    "share_russian" = "Russian",
    "share_other_indo" = "Other Indo-European",
    "share_other" = "Other languages")
  language_bar_order <- c("Spanish", "Other Indo-European", "Chinese", "Korean", "French", "Other languages")

  english_less_than_very_well_denominator_vars = language_vars %>% unlist() %>% as.character()
  
  ####----Device/Internet Access----####
  device_vars <- list(
    no_smartphone = c("B28001_004", "B28001_008", "B28001_010", "B28001_011"),
    no_computer = c("B28001_006", "B28001_008", "B28001_010", "B28001_011"), 
    no_internet = "B28002_013")
  device_labels <- c(
    "share_no_smartphone"  = "No smartphones",
    "share_no_computer" = "No computers",
    "share_no_internet" = "No internet access")
  device_bar_order <- c("No internet access", "No smartphones", "No computers")
  
  ####----Commuting Mode----####
  commute_mode_vars <- list(
    car_alone = "B08301_003",
    pt = "B08301_010",
    carpool = "B08301_004",
    wfh = "B08301_021",
    walk = "B08301_019",
    bike = "B08301_018")
  commute_mode_labels <- c(
    "share_car_alone"  = "Car, truck, or van",
    "share_pt" = "Public transportation",
    "share_carpool" = "Carpool",
    "share_wfh" = "Work from home",
    "share_walk" = "Walk",
    "share_bike" = "Bicycle")
  commute_mode_bar_order <- c("Car, truck, or van", "Work from home", "Public transportation", "Carpool", "Walk", "Bicycle")
  
  ####----Commute Time----####
  commute_time_vars <- list(
    commute_less_5 = "B08303_002",
    commute_5_14 = c("B08303_003", "B08303_004"),
    commute_15_24 = c("B08303_005", "B08303_006"),
    commute_25_34 = c("B08303_007", "B08303_008"),
    commute_35_44 = c("B08303_009", "B08303_010"),
    commute_45_59 = "B08303_011",
    commute_60_89 = "B08303_012",
    commute_90_plus = "B08303_013")
  commute_time_labels <- c(
    "share_commute_less_5"  = "Less than 5",
    "share_commute_5_14" = "5-14",
    "share_commute_15_24" = "15-24",
    "share_commute_25_34" = "25-34",
    "share_commute_35_44" = "35-44",
    "share_commute_45_59" = "45-59",
    "share_commute_60_89" = "60-89",
    "share_commute_90_plus" = "90 or more")
  commute_time_bar_order <- c("Less than 5", "5-14", "15-24", "25-34", "35-44", "45-59", "60-89", "90 or more")
  
  ####----Tenure----####
  tenure_vars <- list(
    owner = "B25003_002",
    renter = "B25003_003")
  tenure_labels <- c(
    "share_renter"  = "Renter occupied",
    "share_owner" = "Owner occupied")
  tenure_bar_order <- c("Renter occupied", "Owner occupied")
  
  ####----Rent Burden----####
  rent_burden_vars <- list(
    rent_burden = c("B25070_007", "B25070_008", "B25070_009", "B25070_010"),
    severe_rent_burden = "B25070_010")
  rent_burden_labels <- c(
    "share_rent_burden"  = "Rent burdened",
    "share_severe_rent_burden" = "Severely rent burdened")
  rent_burden_bar_order <- c("Rent burdened", "Severely rent burdened")
  
  rent_burden_denominator_vars <- c("B25070_001", "B25070_011")
  
  ####----Adult population in school----####
  school_vars <- list(
    in_school = c("B14007_015", "B14007_016", "B14007_017", "B14007_018"))
  school_labels <- c(
    "share_in_school"  = "Enrolled in school")
  school_bar_order <- c("Enrolled in school")
  
  ####----Foreign born population----####
  foreign_born_vars <- list(
    foreign_born = c("B05006_001"))
  foreign_born_labels <- c(
    "share_foreign_born"  = "Foreign-born")
  foreign_born_bar_order <- c("Foreign-born")
  
  ####----Employment----####
  #difficult bc the denominators differ for computing different bars ((un)employment 
  #uses labor force, not in labor force uses total population)
  employment_vars <- list(
    # total_pop = "B23001_001",
    # labor_force = c("B23001_006", "B23001_013", "B23001_020", "B23001_027", "B23001_034", "B23001_041", "B23001_048", "B23001_055", "B23001_062", "B23001_069",
    #                 "B23001_074", "B23001_079", "B23001_084", "B23001_092", "B23001_099", "B23001_106", "B23001_113", "B23001_120", "B23001_127", "B23001_134",
    #                 "B23001_141", "B23001_148", "B23001_155", "B23001_160", "B23001_165", "B23001_170"),
    employed = c("B23001_007", "B23001_014", "B23001_021", "B23001_028", "B23001_035", "B23001_042", "B23001_049", "B23001_056", "B23001_063", "B23001_070",
                 "B23001_075", "B23001_080", "B23001_085", "B23001_093", "B23001_100", "B23001_107", "B23001_114", "B23001_121", "B23001_128", "B23001_135",
                 "B23001_142", "B23001_149", "B23001_156", "B23001_161", "B23001_166", "B23001_171"),
    unemployed =  c("B23001_008", "B23001_015", "B23001_022", "B23001_029", "B23001_036", "B23001_043", "B23001_050", "B23001_057", "B23001_064", "B23001_071",
                    "B23001_076", "B23001_081", "B23001_086", "B23001_094", "B23001_101", "B23001_108", "B23001_115", "B23001_122", "B23001_129", "B23001_136",
                    "B23001_143", "B23001_150", "B23001_157", "B23001_162", "B23001_167", "B23001_172"),
    not_in_labor_force =  c("B23001_009", "B23001_016", "B23001_023", "B23001_030", "B23001_037", "B23001_044", "B23001_051", "B23001_058", "B23001_065", "B23001_072",
                            "B23001_077", "B23001_082", "B23001_087", "B23001_095", "B23001_102", "B23001_109", "B23001_116", "B23001_123", "B23001_130", "B23001_137",
                            "B23001_144", "B23001_151", "B23001_158", "B23001_163", "B23001_168", "B23001_173"))
  employment_labels = c(
    "share_employed" = "Employed",
    "share_unemployed" = "Unemployed",
    "share_not_in_labor_force" = "Not in labor force")
  
  employment_bar_order <- c("Employed", "Unemployed", "Not in labor force")
  
  ####----Plotting Variables Iteratively----####
  plot_metadata = tibble(
    var_list = str_c(constructs, "_vars"),
    total_var = denominators,
    labels = str_c(constructs, "_labels"),
    group_order = str_c(constructs, "_bar_order"),
    constructs = constructs,
    boulevard_name = boulevard_name) 

  ## this iterates over each row in `plot_metadata` and applies each of the columns 
  ## from `plot_metadata` as arguments to the parameter of the same name in `map_variable()`
  results = pmap(
    plot_metadata,
    plot_indicator,
    df_boulevard = df_boulevard %>%
      st_drop_geometry() %>%
      mutate(english_less_than_very_well_denominator = rowSums(select(., all_of(english_less_than_very_well_denominator_vars)), na.rm = TRUE),
             rent_burden_denominator = .data[[rent_burden_denominator_vars[1]]] - .data[[rent_burden_denominator_vars[2]]]),
    df_city = df_city %>%
      st_drop_geometry() %>%
      mutate(english_less_than_very_well_denominator = rowSums(select(., all_of(english_less_than_very_well_denominator_vars)), na.rm = TRUE),
             rent_burden_denominator = .[[rent_burden_denominator_vars[1]]] - .[[rent_burden_denominator_vars[2]]]),
    save = save,
    file_extension = file_extension,
    outpath = outpath)

  names(results) = constructs
  
  return(results)
}





# ===== commute times of greater than 60 by mode ====================================


# Use Urban Institute styling if available
if (requireNamespace("urbnthemes", quietly = TRUE)) {
  library(urbnthemes)
  set_urbn_defaults(style = "print")  # same look as your other charts
} else {
  message("Tip: install.packages('remotes'); remotes::install_github('UrbanInstitute/urbnthemes')")
}

year <- 2023

# 1) Pull B08134 for all LA County tracts (long format)
b08134_long <- tidycensus::get_acs(
  geography   = "tract",
  state       = "06",     # CA
  county      = "037",    # Los Angeles County
  table       = "B08134",
  year        = year,
  survey      = "acs5",
  cache_table = TRUE
)

# 2) Variable labels to parse mode/time
var_labels <- tidycensus::load_variables(year, "acs5", cache = TRUE) |>
  dplyr::filter(stringr::str_starts(name, "B08134_")) |>
  dplyr::select(variable = name, label)

# 3) Flags: Hoover tracts vs all LA City tracts (uses your existing helper)
hoover_geoids <- get_acs_data(file_path, area = "Hoover", census_geography = "tract")$GEOID
city_geoids   <- get_acs_data(file_path, area = "LA",     census_geography = "tract")$GEOID

b08134_flagged <- b08134_long |>
  dplyr::filter(GEOID %in% city_geoids) |>
  dplyr::mutate(group = dplyr::if_else(GEOID %in% hoover_geoids, "Hoover", "City"))

# 4) Summarize: percent with ≥60-minute commute by mode & group
over60_bins <- c("60 to 89 minutes", "90 or more minutes")

b08134_by_mode_group <- b08134_flagged |>
  dplyr::left_join(var_labels, by = "variable") |>
  tidyr::separate_wider_delim("label", "!!",
                              names = c("lvl1","lvl2","rest"), too_few = "align_start"
  ) |>
  dplyr::mutate(rest = stringr::str_remove(rest, "^Total:\\s*!!")) |>
  tidyr::separate_wider_delim("rest", "!!",
                              names = c("mode_raw","time"), too_few = "start"
  ) |>
  dplyr::mutate(
    mode = stringr::str_remove(mode_raw, ":$"),
    time = stringr::str_remove(dplyr::coalesce(time, ""), ":$")
  ) |>
  dplyr::filter(!is.na(time), time != "") |>
  dplyr::group_by(group, mode) |>
  dplyr::summarise(
    commuters_total  = sum(estimate, na.rm = TRUE),
    commuters_over60 = sum(dplyr::if_else(time %in% over60_bins, estimate, 0), na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    pct_over60 = 100 * commuters_over60 / commuters_total,
    # optional label tidy
    mode = mode |>
      stringr::str_replace("^Car, truck, or van: ", "Car/truck/van – ") |>
      stringr::str_replace("^Public transportation \\(excluding taxicab\\): ", "Transit – ") |>
      stringr::str_replace("^Taxicab, motorcycle, bicycle, or other means$", "Taxi/motorcycle/bike/other")
  )

# 5) Plot (inherits Urban theme if set_urbn_defaults ran)
p <- b08134_by_mode_group |>
  ggplot2::ggplot(ggplot2::aes(x = reorder(mode, pct_over60), y = pct_over60, fill = group)) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Commute ≥60 minutes by mode",
    subtitle = "Hoover tracts vs. all Los Angeles city tracts • ACS 2019–2023 (B08134)",
    x = NULL, y = "Percent of commuters"
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))

print(p)

# 6) Save outputs
ggplot2::ggsave("commute_over60_Hoover_vs_City.png", p, width = 9, height = 6, dpi = 300)
readr::write_csv(b08134_by_mode_group, "commute_over60_Hoover_vs_City.csv")