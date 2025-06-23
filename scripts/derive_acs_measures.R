#' Calculate percentages and other derived ACS measures
#'
#' @param df The dataframe with raw ACS data (namely, that returned from `get_acs_data()`)
#'
#' @return A dataframe with additional columns for derived measures
derive_acs_measures = function(df) {
  
  english_less_than_very_well_denominator_vars = c(
    "C16001_005", "C16001_020", "C16001_023", "C16001_008", "C16001_011", 
    "C16001_014", "C16001_017", "C16001_026", "C16001_029", "C16001_032", 
    "C16001_035", "C16001_038")

  df = df %>%
  rename(
    commute_time_total = B08303_001,
    commute_time_5_less = B08303_002,
    commute_time_5_9 = B08303_003,
    commute_time_10_14 = B08303_004,
    commute_time_15_19 = B08303_005,
    commute_time_20_24 = B08303_006,
    commute_time_25_29 = B08303_007,
    commute_time_30_34 = B08303_008,
    commute_time_35_39 = B08303_009,
    commute_time_40_44 = B08303_010,
    commute_time_45_59 = B08303_011,
    commute_time_60_89 = B08303_012,
    commute_time_90_plus = B08303_013) %>%
  mutate(
    ## disability
    disability_all =  B18101_004 + B18101_007 + B18101_010 + B18101_013 +
      B18101_016 + B18101_019 + B18101_023 + B18101_026 +
      B18101_029 + B18101_032 + B18101_035 + B18101_038,
    disability_hearing = B18102_004 + B18102_007 + B18102_010 + B18102_013 +
      B18102_016 + B18102_019 + B18102_023 + B18102_026 +
      B18102_029 + B18102_032 + B18102_035 + B18102_038,
    disability_ambulatory = B18105_004 + B18105_007 + B18105_010 + B18105_013 +
      B18105_016 + B18105_020 + B18105_023 + B18105_026 + B18105_029 + B18105_032,
    disability_vision = B18103_004 + B18103_007 + B18103_010 + B18103_013 +
      B18103_016 + B18103_019 + B18103_023 + B18103_026 +
      B18103_029 + B18103_032 + B18103_035 + B18103_038,
    across(matches("disability"), ~ .x / B18101_001, .names = "share_{.col}"),
    ## poverty
    share_poverty = B17001_002 / B17001_001,
    share_internet_none = B28002_013 / B28002_001,
    share_tenure_owner_occupied = B25003_002 / B25003_001,
    share_tenure_renter_occupied = B25003_003 / B25003_001,
    ## device access
    share_computer_non = B28001_011/B28001_001,
    ## commute
    commute_5_14 = commute_5_9 + commute_10_14,
    commute_15_24 = commute_15_19 + commute_20_24,
    commute_25_34 = commute_25_29 + commute_30_34,
    commute_35_44 = commute_35_39 + commute_40_44,
    commute_hour_plus = commute_60_89 + commute_90_plus,
    share_commute_hour_plus = commute_hour_plus / commute_total,
    ## vehicle access
    vehicle_none = (B25044_003 + B25044_010),
    vehicle_one = (B25044_004 + B25044_011),
    vehicle_multiple = (B25044_001 - no_car - one_car),
    across(matches("vehicle"), ~ .x / B25044_001, .names = "share_{.col}"),
    renter_costburden_30plus = B25070_007 + B25070_008 + B25070_009 + B25070_010,
    renter_costburden_50plus = B25070_010,
    ## we subtract the households for which this measure is not computed from the table universe
    ## thus this is: of renter households, the share that is costburdened is...
    ## in contrast to: of all households...
    across(matches("renter_costburden"), ~ .x / (B25070_001 - B25070_011), .names = "share_{.col}"),
    english_less_than_very_well_denominator = rowSums(
      select(st_drop_geometry(.), all_of(english_less_than_very_well_denominator_vars)), na.rm = TRUE),
    share_speak_spanish_at_home = C16001_005 / english_less_than_very_well_denominator,
    share_speak_korean_at_home = C16001_020 / english_less_than_very_well_denominator)

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

  ####----Ethnicity----####
  ethnicity_vars <- list(
    ethnicity_white = "B03002_003",
    ethnicity_black = "B03002_004",
    ethnicity_indigenous = "B03002_005",
    ethnicity_asian = "B03002_006",
    ethnicity_pacific = "B03002_007",
    ethnicity_other_race = "B03002_010",
    ethnicity_hispanic = "B03002_012")
  
  ####----Gender----####
  gender_vars <- list(
    gender_male = "B01001_002",
    gender_female = "B01001_026")
  
  ####----Family Structure----####
  family_vars <- list(
    family_married_children = "B11003_003",
    family_married_no_children = "B11003_007",
    family_single_children = c("B11003_010", "B11003_016"),
    family_single_no_children = c("B11003_014", "B11003_020"))
  
  ####----English Speaking Ability----####
  english_vars <- list(
    english_well = c("B16004_006", "B16004_011", "B16004_016", "B16004_021", "B16004_028", "B16004_033", "B16004_038", "B16004_043", "B16004_050", "B16004_055", "B16004_060", "B16004_065"),
    english_not_well = c("B16004_007", "B16004_012", "B16004_017", "B16004_022", "B16004_029", "B16004_034", "B16004_039", "B16004_044", "B16004_051", "B16004_056", "B16004_061", "B16004_066"),
    english_not_at_all =c("B16004_008", "B16004_013", "B16004_018", "B16004_023", "B16004_030", "B16004_035", "B16004_040", "B16004_045", "B16004_052", "B16004_057", "B16004_062", "B16004_067"))

  ####----Primary Language----####
  #(we specifically care about the languages spoken for people who do not speak english well)
  #note that you may have to change which languages to display on the chart depending on which languages are most common in each study area
  language_vars <- list(
    language_spanish = "C16001_005",
    language_korean = "C16001_020",
    language_chinese = "C16001_023",
    language_other = c("C16001_008", "C16001_011", "C16001_014", "C16001_017", "C16001_026", "C16001_029", "C16001_032", "C16001_035", "C16001_038"))
  
  ####----Device/Internet Access----####
  device_vars <- list(
    device_no_smartphone = c("B28001_004", "B28001_008", "B28001_010"),
    device_no_computer = c("B28001_006", "B28001_008", "B28001_010"), 
    device_no_internet = "B28002_013")
  
  ####----Commuting Mode----####
  commute_mode_vars <- list(
    commute_mode_car_alone = "B08301_003",
    commute_mode_public_transit = "B08301_010",
    commute_mode_carpool = "B08301_004",
    commute_mode_work_from_home = "B08301_021",
    commute_mode_walk = "B08301_019",
    commute_mode_bike = "B08301_018")

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
  
  ####----Tenure----####
  tenure_vars <- list(
    tenure_owner = "B25003_002",
    tenure_renter = "B25003_003")
  
  ####----Rent Burden----####
  rent_burden_vars <- list(
    rent_burden_30plus = c("B25070_007", "B25070_008", "B25070_009", "B25070_010"),
    rent_burden_50plus = "B25070_010")

  ####----Employment----####
  #difficult bc the denominators differ for computing different bars ((un)employment 
  #uses labor force, not in labor force uses total population)
  employment_vars <- list(
    employment_labor_force = c("B23001_006", "B23001_013", "B23001_020", "B23001_027", "B23001_034", "B23001_041", "B23001_048", "B23001_055", "B23001_062", "B23001_069",
                    "B23001_074", "B23001_079", "B23001_084", "B23001_092", "B23001_099", "B23001_106", "B23001_113", "B23001_120", "B23001_127", "B23001_134",
                    "B23001_141", "B23001_148", "B23001_155", "B23001_160", "B23001_165", "B23001_170"),
    employment_employed = c("B23001_007", "B23001_014", "B23001_021", "B23001_028", "B23001_035", "B23001_042", "B23001_049", "B23001_056", "B23001_063", "B23001_070",
                 "B23001_075", "B23001_080", "B23001_085", "B23001_093", "B23001_100", "B23001_107", "B23001_114", "B23001_121", "B23001_128", "B23001_135",
                 "B23001_142", "B23001_149", "B23001_156", "B23001_161", "B23001_166", "B23001_171"),
    employment_unemployed =  c("B23001_008", "B23001_015", "B23001_022", "B23001_029", "B23001_036", "B23001_043", "B23001_050", "B23001_057", "B23001_064", "B23001_071",
                    "B23001_076", "B23001_081", "B23001_086", "B23001_094", "B23001_101", "B23001_108", "B23001_115", "B23001_122", "B23001_129", "B23001_136",
                    "B23001_143", "B23001_150", "B23001_157", "B23001_162", "B23001_167", "B23001_172"),
    employment_not_in_labor_force =  c("B23001_009", "B23001_016", "B23001_023", "B23001_030", "B23001_037", "B23001_044", "B23001_051", "B23001_058", "B23001_065", "B23001_072",
                            "B23001_077", "B23001_082", "B23001_087", "B23001_095", "B23001_102", "B23001_109", "B23001_116", "B23001_123", "B23001_130", "B23001_137",
                            "B23001_144", "B23001_151", "B23001_158", "B23001_163", "B23001_168", "B23001_173"))
  
  vars = c(
    income_vars,
    age_vars,
    gender_vars,
    ethnicity_vars,
    family_vars,
    english_vars,
    language_vars,
    device_vars,
    commute_mode_vars,
    commute_time_vars,
    tenure_vars,
    rent_burden_vars,
    employment_vars)
  
  ## iterate over each of the named vectors of variables and use them to calculate
  ## new variables
  new_vars = imap(
    vars,
    ~ {
      name = .y
      values = as.character(.x)
      
      df %>%
        st_drop_geometry() %>%
        transmute(
          !!name := rowSums(
            select(st_drop_geometry(.), all_of(values)), na.rm = TRUE)) }) %>%
    reduce(bind_cols)
  
  result = df %>% 
    bind_cols(new_vars) %>%
    mutate(
      english_less_than_very_well_denominator = rowSums(
        select(st_drop_geometry(.), all_of(english_less_than_very_well_denominator_vars)), na.rm = TRUE),
      across(matches("income_"), ~ .x / B19001_001, .names = "share_{.col}"),
      across(.cols = c(employment_employed, employment_unemployed), ~ .x / employment_labor_force, .names = "share_{.col}"),
      across(.cols = c(employment_not_in_labor_force), ~ .x / B23001_001, .names = "share_{.col}"),
      across(matches("age_"), ~ .x / B01001_001, .names = "share_{.col}"),
      across(matches("ethnicity"), ~ .x / B03002_001, .names = "share_{.col}"),
      across(matches("family"), ~ .x / B11003_001, .names = "share_{.col}"),
      across(matches("english"), ~ .x / B16004_001, .names = "share_{.col}"),
      across(matches("language"), ~ .x / english_less_than_very_well_denominator, .names = "share_{.col}"),
      across(matches("device"), ~ .x / B28001_001, .names = "share_{.col}"),
      across(matches("commute_mode"), ~ .x / B08301_001, .names = "share_{.col}"),
      across(matches("commute_time"), ~ .x / B08303_001, .names = "share_{.col}"),
      across(matches("tenure"), ~ .x / B25003_001, .names = "share_{.col}"),
      across(matches("rent_burden"), ~ .x / (B25070_001 - B25070_011), .names = "share_{.col}"))
  
  ## no percentage should be greater than one
  stopifnot(
    result %>%
      select(matches("share")) %>%
      st_drop_geometry() %>%
      pivot_longer(everything()) %>%
      filter(value > 1) %>%
      nrow == 0)
  
  return(result)
}
