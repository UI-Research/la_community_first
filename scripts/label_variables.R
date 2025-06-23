rename_variables = function(df) {
  
  ## just the geometry column
  geometries = df %>% select()
  
  labels = c(
      "share_income_0_30" = "Less than $30,000",
      "share_income_30_60" = "$30,000 – $59,999",
      "share_income_60_100" = "$60,00 – $99,999",
      "share_income_100_150" = "$100,000 – $149,999",
      "share_income_150_200" = "$150,000 – $199,999",
      "share_income_200plus" = "$200,000 or more",
      "share_age_0_17"  = "0–17",
      "share_age_18_29" = "18–29",
      "share_age_30_44" = "30–44",
      "share_age_45_59" = "45–59",
      "share_age_60_up" = "60+",
      "share_ethnicity_white" = "White",
      "share_ethnicity_black" = "Black",
      "share_ethnicity_indigenous" = "Indigenous",
      "share_ethnicity_asian" = "Asian",
      "share_ethnicity_pacific" = "Pacific Islander",
      "share_ethnicity_other_race" = "Other Race",
      "share_ethnicity_hispanic" = "Hispanic",
      "share_gender_male" = "Male",
      "share_gender_female" = "Female",
      "share_family_married_children" = "Married with children",
      "share_family_married_no_children" = "Married without children",
      "share_family_single_children" = "Single parent with children",
      "share_family_single_no_children" = "Single without children",
      "share_english_well" = "Well",
      "share_english_not_well" = "Not well",
      "share_english_not_at_all" = "Not at all",
      "share_language_spanish" = "Spanish",
      "share_language_korean" = "Korean",
      "share_language_chinese" = "Chinese",
      "share_language_other" = "Other languages",
      "share_device_no_smartphone"  = "No smartphones",
      "share_device_no_computer" = "No computers",
      "share_device_no_internet" = "No internet access",
      "share_commute_mode_car_alone"  = "Car, truck, or van",
      "share_commute_mode_public_transit" = "Public transportation",
      "share_commute_mode_carpool" = "Carpool",
      "share_commute_mode_work_from_home" = "Work from home",
      "share_commute_mode_walk" = "Walk",
      "share_commute_mode_bike" = "Bicycle",
      "share_commute_time_less_5"  = "Less than 5",
      "share_commute_time_5_14" = "5-14",
      "share_commute_time_15_24" = "15-24",
      "share_commute_time_25_34" = "25-34",
      "share_commute_time_35_44" = "35-44",
      "share_commute_time_45_59" = "45-59",
      "share_commute_time_60_89" = "60-89",
      "share_commute_time_90_plus" = "90 or more",
      "share_tenure_renter"  = "Renter occupied",
      "share_tenure_owner" = "Owner occupied",
      "share_rent_burden_30plus"  = "Rent burdened",
      "share_rent_burden_50plus" = "Severely rent burdened",
      "share_employment_employed" = "Employed",
      "share_employment_unemployed" = "Unemployed",
      "share_employment_not_in_labor_force" = "Not in labor force")
  
  result = df %>% 
    st_drop_geometry() %>%
    select(all_of(labels %>% names())) %>% 
    rename_with(~labels) %>%
    bind_cols(geometries)
  
}