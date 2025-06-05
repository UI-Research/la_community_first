### Load the script for pico data ###
source("C:/Users/tmaginn/Documents/GitHub/la_community_first/data_pull.R")


# Set working directory and file paths
username <- getwd() %>% str_split("\\/") %>% unlist() %>% .[3]
file_path <- file.path("C:", "Users", username, "Box", "LA Transit project/Social Climate Analysis")

# Call the function and store the result
pico_master_data <- clean_and_prepare_data(file_path)


### same thing as above but now loading in script for LA city data ###

source("C:/Users/tmaginn/Documents/GitHub/la_community_first/la_city_data_pull.R")

username <- getwd() %>% str_split("\\/") %>% unlist() %>% .[3]
file_path <- file.path("C:", "Users", username, "Box", "LA Transit project/Social Climate Analysis")

# Call the function and store the result
la_city_master_data <- clean_and_prepare_data(file_path)




################ Making charts ##############

###load list of 2023 acs variables
v23 <- load_variables(2023, "acs5", cache = TRUE)

#### age distribution ####

#define age bins
age_bins <- list(
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
                "B01001_046", "B01001_047", "B01001_048", "B01001_049")
)

# Step 2: Summarize total population and age bins across all rows
age_pico <- pico_master_data %>%
  summarize(
    total_population = sum(B01001_001, na.rm = TRUE),
    
    age_0_17  = sum(across(all_of(age_bins$age_0_17)),  na.rm = TRUE),
    age_18_29 = sum(across(all_of(age_bins$age_18_29)), na.rm = TRUE),
    age_30_44 = sum(across(all_of(age_bins$age_30_44)), na.rm = TRUE),
    age_45_59 = sum(across(all_of(age_bins$age_45_59)), na.rm = TRUE),
    age_60_up = sum(across(all_of(age_bins$age_60_up)), na.rm = TRUE)
  ) %>%
  mutate(
    share_age_0_17  = age_0_17  / total_population,
    share_age_18_29 = age_18_29 / total_population,
    share_age_30_44 = age_30_44 / total_population,
    share_age_45_59 = age_45_59 / total_population,
    share_age_60_up = age_60_up / total_population
  )

#LA city age shares
age_la <- la_city_master_data %>%
  summarize(
    total_population = sum(B01001_001, na.rm = TRUE),
    
    age_0_17  = sum(across(all_of(age_bins$age_0_17)),  na.rm = TRUE),
    age_18_29 = sum(across(all_of(age_bins$age_18_29)), na.rm = TRUE),
    age_30_44 = sum(across(all_of(age_bins$age_30_44)), na.rm = TRUE),
    age_45_59 = sum(across(all_of(age_bins$age_45_59)), na.rm = TRUE),
    age_60_up = sum(across(all_of(age_bins$age_60_up)), na.rm = TRUE)
  ) %>%
  mutate(
    share_age_0_17  = age_0_17  / total_population,
    share_age_18_29 = age_18_29 / total_population,
    share_age_30_44 = age_30_44 / total_population,
    share_age_45_59 = age_45_59 / total_population,
    share_age_60_up = age_60_up / total_population
  )







