### click on sections below to see code



#### Loading stuff in ####

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



###load list of 2023 acs variables
v23 <- load_variables(2023, "acs5", cache = TRUE)

#make sure everything is in Arial font per LA DOT style guide
theme_set(theme_minimal(base_family = "Arial"))


#### We will make charts by creating some modular functions, then calling functions according to the census variable we want to analyze ###


#### Function definitions ####
### Function to compute group shares ###
compute_shares <- function(df, var_list, total_var) {
  total_population <- sum(df[[total_var]], na.rm = TRUE)
  
  if (is.na(total_population) || total_population == 0) {
    stop("Total population is NA or zero — check total_var input!") #this is to make sure we're not dividing by zero
  }
  
  shares <- sapply(var_list, function(vars) {
    selected <- df %>% select(all_of(vars))
    numeric_data <- selected %>% mutate(across(everything(), ~ as.numeric(.)))
    row_total <- rowSums(numeric_data, na.rm = TRUE)
    sum(row_total, na.rm = TRUE) / total_population
  })
  
  tibble(!!!setNames(as.list(shares), paste0("share_", names(shares))))
}


### Function to reshape to long format and label groups ###
prepare_long_data <- function(df, area_name, group_label_map, col_prefix = "share_", group_col = "group") {
  df %>%
    select(starts_with(col_prefix)) %>%
    mutate(area = area_name) %>%
    pivot_longer(cols = starts_with(col_prefix), names_to = group_col, values_to = "share") %>%
    mutate(
      !!group_col := recode(.data[[group_col]], !!!group_label_map),
      label = paste0(round(share * 100), "%")
    )
}


### Function to create bar plot ###
plot_comparison <- function(df, x_var = "group", fill_var = "area", y_var = "share",
                            label_var = "label", colors, group_order = NULL, title = NULL) {
  df <- df %>%
    mutate(
      !!sym(x_var) := factor(stringr::str_wrap(.data[[x_var]], width = 12), 
                             levels = stringr::str_wrap(group_order, width = 12)),
      !!sym(fill_var) := factor(.data[[fill_var]], levels = names(colors))
    )
  
  ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[fill_var]])) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(aes(label = ifelse(.data[[y_var]] >= 0.015, .data[[label_var]], "")),
              position = position_dodge(width = 0.8),
              vjust = 1.5,
              color = "white",
              size = 4)+
    scale_fill_manual(values = colors, name = NULL) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      x = NULL,
      y = NULL,
      title = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "top",
      panel.grid.major.x = element_blank()
    )
}


### Wrapper function - this combines all functions into one piece of code, so all we need to do is call the wrapper function ###
plot_indicator <- function(pico_data, la_data, var_list, total_var,
                           labels, group_order, title, colors) {
  pico <- compute_shares(pico_data, var_list, total_var)
  la   <- compute_shares(la_data, var_list, total_var)
  
  pico_long <- prepare_long_data(pico, "Pico", labels)
  la_long   <- prepare_long_data(la, "LA City", labels)
  
  combined <- bind_rows(pico_long, la_long)
  
  plot_comparison(combined,
                  colors = colors,
                  group_order = group_order,
                  title = title)
}






#### steps for making charts ##########################################

## After running the code above, making charts requires 3/4 steps:

  # 1. Define which variables belong to which bins (if applicable) and make custom labels

  # 2. set the custom color pallette that LA DOT wants (you can just reuse the "colors" code below)

  # 3. (if necessary) custom order the axis

  # 4. Run the 'plot_indicator' function by passing through the necessary arguments (see below for an example)







#### age distribution ####
#define age bins and make custom labels
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

age_labels <- c(
  "share_age_0_17"  = "0–17",
  "share_age_18_29" = "18–29",
  "share_age_30_44" = "30–44",
  "share_age_45_59" = "45–59",
  "share_age_60_up" = "60+"
)

#custom color palette per LA DOT style guide
colors <- c("Pico" = rgb(135, 160, 180, maxColorValue = 255),
            "LA City" = rgb(26, 67, 120, maxColorValue = 255))

#bar order for plotting
bar_order <- c("0–17", "18–29", "30–44", "45–59", "60+")

#plot
plot_indicator(
  pico_data = pico_master_data,
  la_data = la_city_master_data,
  var_list = age_bins,
  total_var = "B01001_001",
  labels = age_labels,
  group_order = bar_order,
  title = "Age Distribution: Pico vs LA City",
  colors = colors
)





#### Ethnicity ####

#custom variables and labels
ethnicity_vars <- list(
  white = "B03002_003",
  black = "B03002_004",
  indigenous = "B03002_005",
  asian = "B03002_006",
  pacific = "B03002_007",
  other = "B03002_010",
  hispanic = "B03002_012"
)

ethnicity_labels <- c(
  "share_white" = "White",
  "share_black" = "Black",
  "share_indigenous" = "Indigenous",
  "share_asian" = "Asian",
  "share_pacific" = "Pacific Islander",
  "share_other" = "Other",
  "share_hispanic" = "Hispanic"
)

#LA DOT colors
colors <- c("Pico" = rgb(135, 160, 180, maxColorValue = 255),
            "LA City" = rgb(26, 67, 120, maxColorValue = 255))


#group order for plotting
bar_order <- c("Hispanic", "White", "Asian", "Black", "Other", "Indigenous", "Pacific Islander")

#plot
plot_indicator(
  pico_data = pico_master_data,
  la_data = la_city_master_data,
  var_list = ethnicity_vars,
  total_var = "B03002_001",
  labels = ethnicity_labels,
  group_order = bar_order,
  title = "Ethnicity Distribution: Pico vs LA City",
  colors = colors
)




#### Employment status - this one is slightly different because our bars are calculated differently ####

# employment bins and labels
employment_bins <- list(
  labor_force = c("B23001_006", "B23001_013", "B23001_020", "B23001_027", "B23001_034", "B23001_041", "B23001_048", "B23001_055", "B23001_062", "B23001_069",
                  "B23001_074", "B23001_079", "B23001_084", "B23001_092", "B23001_099", "B23001_106", "B23001_113", "B23001_120", "B23001_127", "B23001_134",
                  "B23001_141", "B23001_148", "B23001_155", "B23001_160", "B23001_165", "B23001_170"),
  employed = c("B23001_007", "B23001_014", "B23001_021", "B23001_028", "B23001_035", "B23001_042", "B23001_049", "B23001_056", "B23001_063", "B23001_070",
               "B23001_075", "B23001_080", "B23001_085", "B23001_093", "B23001_100", "B23001_107", "B23001_114", "B23001_121", "B23001_128", "B23001_135",
               "B23001_142", "B23001_149", "B23001_156", "B23001_161", "B23001_166", "B23001_171"),
  unemployed =  c("B23001_008", "B23001_015", "B23001_022", "B23001_029", "B23001_036", "B23001_043", "B23001_050", "B23001_057", "B23001_064", "B23001_071",
                  "B23001_076", "B23001_081", "B23001_086", "B23001_094", "B23001_101", "B23001_108", "B23001_115", "B23001_122", "B23001_129", "B23001_136",
                  "B23001_143", "B23001_150", "B23001_157", "B23001_162", "B23001_167", "B23001_172"),
  not_in_labor_force =  c("B23001_009", "B23001_016", "B23001_023", "B23001_030", "B23001_037", "B23001_044", "B23001_051", "B23001_058", "B23001_065", "B23001_072",
                          "B23001_077", "B23001_082", "B23001_087", "B23001_095", "B23001_102", "B23001_109", "B23001_116", "B23001_123", "B23001_130", "B23001_137",
                          "B23001_144", "B23001_151", "B23001_158", "B23001_163", "B23001_168", "B23001_173")
)


employment_labels <- c(
  "share_employed" = "Employed",
  "share_unemployed" = "Unemployed"
)


#custom color palette per LA DOT style guide
colors <- c("Pico" = rgb(135, 160, 180, maxColorValue = 255),
            "LA City" = rgb(26, 67, 120, maxColorValue = 255))


#bar order for plotting
bar_order <- c("Employed", "Unemployed")


#plot
plot_indicator(
  pico_data = pico_master_data,
  la_data = la_city_master_data,
  var_list = employment_bins,
  total_var = labor_force,
  labels = ethnicity_labels,
  group_order = bar_order,
  title = "Employment Distribution: Pico vs LA City",
  colors = colors
)






#### Gender ####

gender_vars <- list(
  male = "B01001_002",
  female = "B01001_026"
)

gender_labels <- c(
  "share_male" = "Male",
  "share_female" = "Female"
)
  

colors <- c("Pico" = rgb(135, 160, 180, maxColorValue = 255),
            "LA City" = rgb(26, 67, 120, maxColorValue = 255))

bar_order <- c("Male", "Female")

plot_indicator(
  pico_data = pico_master_data,
  la_data = la_city_master_data,
  var_list = gender_vars,
  total_var = "B01001_001",
  labels = gender_labels,
  group_order = bar_order,
  title = "Gender Composition: Pico vs LA City",
  colors = colors
)

  


#### Family Structure ####

family_bins <- list(
  married_children = "B11003_003",
  married_no_children = "B11003_007",
  single_children = c("B11003_010", "B11003_016"),
  single_no_children = c("B11003_014", "B11003_020")
)
  
family_labels <- c(
  "share_married_children" = "Married with children",
  "share_married_no_children" = "Married without children",
  "share_single_children" = "Single parent with children",
  "share_single_no_children" = "Single without children"
)

colors <- c("Pico" = rgb(135, 160, 180, maxColorValue = 255),
            "LA City" = rgb(26, 67, 120, maxColorValue = 255))

bar_order <- c("Married with children", "Married without children", "Single parent with children", "Single without children")
  
  
plot_indicator(
  pico_data = pico_master_data,
  la_data = la_city_master_data,
  var_list = family_bins,
  total_var = "B11003_001",
  labels = family_labels,
  group_order = bar_order,
  title = "Family Structure: Pico vs LA City",
  colors = colors
)




#### English speaking ability ####

language_bins <- list(
  very_well = c("B16004_005", "B16004_010", "B16004_015", "B16004_020", "B16004_027", "B16004_032", "B16004_037", "B16004_042", "B16004_049", "B16004_054", "B16004_059", "B16004_064"),
  well = c("B16004_006", "B16004_011", "B16004_016", "B16004_021", "B16004_028", "B16004_033", "B16004_038", "B16004_043", "B16004_050", "B16004_055", "B16004_060", "B16004_065"),
  not_well = c("B16004_007", "B16004_012", "B16004_017", "B16004_022", "B16004_029", "B16004_034", "B16004_039", "B16004_044", "B16004_051", "B16004_056", "B16004_061", "B16004_066"),
  none =c("B16004_008", "B16004_013", "B16004_018", "B16004_023", "B16004_030", "B16004_035", "B16004_040", "B16004_045", "B16004_052", "B16004_057", "B16004_062", "B16004_067")
)


language_labels <- c(
  "share_very_well"  = "Very well",
  "share_well" = "Well",
  "share_not_well" = "Not well",
  "share_none" = "None"
)

colors <- c("Pico" = rgb(135, 160, 180, maxColorValue = 255),
            "LA City" = rgb(26, 67, 120, maxColorValue = 255))

bar_order <- c("Very well", "Well", "Not well", "None")



plot_indicator(
  pico_data = pico_master_data,
  la_data = la_city_master_data,
  var_list = language_bins,
  total_var = "B16004_001",
  labels = language_labels,
  group_order = bar_order,
  title = "English Speaking Ability: Pico vs LA City",
  colors = colors
)





#### Primary language (we specifically care about the languages spoken for people who do not speak english well) ####
#note that you may have to change which languages to display on the chart depending on which languages are most common in each study area

language_bins <- list(
  english_only = "C16001_002",
  spanish= "C16001_005",
  korean = "C16001_020",
  chinese = "C16001_023",
  other = c("C16001_008", "C16001_011", "C16001_014", "C16001_017", "C16001_026", "C16001_029", "C16001_032", "C16001_035", "C16001_038")
)

language_labels <- c(
  "share_english_only"  = "English only",
  "share_spanish" = "Spanish",
  "share_korean" = "Korean",
  "share_chinese" = "Chinese",
  "share_other" = "Other languages"
)

colors <- c("Pico" = rgb(135, 160, 180, maxColorValue = 255),
            "LA City" = rgb(26, 67, 120, maxColorValue = 255))

bar_order <- c("English only", "Spanish", "Korean", "Chinese", "Other languages")

plot_indicator(
  pico_data = pico_master_data,
  la_data = la_city_master_data,
  var_list = language_bins,
  total_var = "C16001_001",
  labels = language_labels,
  group_order = bar_order,
  title = "Primary Language: Pico vs LA City",
  colors = colors
)



#### Device/Internet Access ####
device_bins <- list(
  no_smartphone = c("B28001_004", "B28001_008", "B28001_010"),
  no_computer = c("B28001_006", "B28001_008", "B28001_010"), 
  no_internet = "B28002_013"
)

device_labels <- c(
  "share_no_smartphone"  = "No smartphones",
  "share_no_computer" = "No computers",
  "share_no_internet" = "No internet access"
)


colors <- c("Pico" = rgb(135, 160, 180, maxColorValue = 255),
            "LA City" = rgb(26, 67, 120, maxColorValue = 255))

bar_order <- c("No internet access", "No smartphones", "No computers")

plot_indicator(
  pico_data = pico_master_data,
  la_data = la_city_master_data,
  var_list = device_bins,
  total_var = "B28001_001",
  labels = device_labels,
  group_order = bar_order,
  title = "Device Access: Pico vs LA City",
  colors = colors
)




#### Commuting modes ####

commute_bins <- list(
  car_alone = "B08301_003",
  pt = c("B08301_010", "B08301_011","B08301_012", "B08301_013", "B08301_014", "B08301_015"),
  carpool = "B08301_004",
  wfh = "B08301_021",
  walk = "B08301_019",
  bike = "B08301_018"
)

commute_labels <- c(
  "share_car_alone"  = "Car, truck, or van",
  "share_pt" = "Public transportation (excluding taxicab)",
  "share_carpool" = "Carpool",
  "share_wfh" = "Work from home",
  "share_walk" = "Walk",
  "share_bike" = "Bicycle"
)

colors <- c("Pico" = rgb(135, 160, 180, maxColorValue = 255),
            "LA City" = rgb(26, 67, 120, maxColorValue = 255))

bar_order <- c("Car, truck, or van", "Work from home", "Public transportation (excluding taxicab)", "Carpool", "Walk", "Bicycle")

plot_indicator(
  pico_data = pico_master_data,
  la_data = la_city_master_data,
  var_list = commute_bins,
  total_var = "B08301_001",
  labels = commute_labels,
  group_order = bar_order,
  title = "Commute Mode: Pico vs LA City",
  colors = colors
)




#### Commute time ####
commute_bins <- list(
  commute_less_5 = "B08303_002",
  commute_5_14 = c("B08303_003", "B08303_004"),
  commute_15_24 = c("B08303_005", "B08303_006"),
  commute_25_34 = c("B08303_007", "B08303_008"),
  commute_35_44 = c("B08303_009", "B08303_010"),
  commute_45_59 = "B08303_011",
  commute_60_89 = "B08303_012",
  commute_90_plus = "B08303_013"
)

commute_labels <- c(
  "share_commute_less_5"  = "Less than 5",
  "share_commute_5_14" = "5-14",
  "share_commute_15_24" = "15-24",
  "share_commute_25_34" = "25-34",
  "share_commute_35_44" = "35-44",
  "share_commute_45_59" = "45-59",
  "share_commute_60_89" = "60-89",
  "share_commute_90_plus" = "90 or more"
)


colors <- c("Pico" = rgb(135, 160, 180, maxColorValue = 255),
            "LA City" = rgb(26, 67, 120, maxColorValue = 255))

bar_order <- c("Less than 5", "5-14", "15-24", "25-34", "35-44", "45-59", "60-89", "90 or more")

plot_indicator(
  pico_data = pico_master_data,
  la_data = la_city_master_data,
  var_list = commute_bins,
  total_var = "B08303_001",
  labels = commute_labels,
  group_order = bar_order,
  title = "Commute Time: Pico vs LA City",
  colors = colors
)




#### Tenure ####
tenure_vars <- list(
  owner = "B25003_002",
  renter = "B25003_003"
)

tenure_labels <- c(
  "share_renter"  = "Renter occupied",
  "share_owner" = "Owner occupied"
)

colors <- c("Pico" = rgb(135, 160, 180, maxColorValue = 255),
            "LA City" = rgb(26, 67, 120, maxColorValue = 255))

bar_order <- c("Renter occupied", "Owner occupied")

plot_indicator(
  pico_data = pico_master_data,
  la_data = la_city_master_data,
  var_list = tenure_vars,
  total_var = "B25003_001",
  labels = tenure_labels,
  group_order = bar_order,
  title = "Tenure: Pico vs LA City",
  colors = colors
)







#### HLA voting data - this section of code doesn't use the functions above ####

hla_vote <- st_read(file.path(file_path, "Data/Measure HLA Vote/HLA.shp"))
library(lwgeom)

hla_vote <- st_make_valid(hla_vote)


CA_tracts <- st_read(file.path(file_path, "Hoover/Mapping/CA_census_Tracts.shp"), quiet = TRUE)
la_shp <- st_read(file.path(file_path, "Data/tl_2023_06_tract/tl_2023_06_tract.shp"), quiet = TRUE)
pico_buffer_tracts <- st_read(file.path(file_path, "Pico/Maps/pico_buffer_tracts.shp"), quiet = TRUE)

pico_tracts <- la_shp %>%
  filter(GEOID %in% pico_buffer_tracts$GEOID)


la_city_tracts <- st_read(file.path(file_path, "/Data/LA tracts/LA_City_2020_Census_Tracts_.shp"))%>%
  mutate(GEOID = paste0("06037", CT20))

#make sure crs matches
pico_tracts <- st_transform(pico_buffer_tracts, crs = 4326)
la_city_tracts <- st_transform(la_city_tracts, crs = 4326)
hla_vote <- st_transform(hla_vote, crs = 4326)



#find intersections with la and pico
voting_in_pico <- st_filter(hla_vote, pico_tracts)
voting_in_la <- st_filter(hla_vote, la_city_tracts)



# summing data
pico_hla <- voting_in_pico %>%
  summarize(
    total_yes = sum(V3_LOS_A_2, na.rm = T),
    total = sum(V3_LOS_A_4, na.rm = T)
  )%>%
  mutate(
    vote_share = total_yes/total
  )

la_hla  <- voting_in_la %>%
  summarize(
    total_yes = sum(V3_LOS_A_2, na.rm = T),
    total = sum(V3_LOS_A_4, na.rm = T)
  )%>%
  mutate(
    vote_share = total_yes/total
  )


#reshape and combine df's
pico_long <- pico_hla %>%
  select(starts_with("vote_")) %>%
  mutate(area = "Pico") %>%
  pivot_longer(cols = starts_with("vote_"), 
               names_to = "vote", values_to = "share")

city_long <- la_hla %>%
  select(starts_with("vote_")) %>%
  mutate(area = "LA City") %>%
  pivot_longer(cols = starts_with("vote_"), 
               names_to = "vote", values_to = "share")

combined <- bind_rows(pico_long, city_long)%>%
  mutate(
    label = paste0(round(share * 100, 1), "%")  # Add percentage labels
  )



# custom colors per LA DOT style guide
colors <- c("Pico" = rgb(135, 160, 180, maxColorValue = 255), 
            "LA City" = rgb(26, 67, 120, maxColorValue = 255))


#plot
ggplot(combined, aes(x = area, y = share, fill = area)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = label),
            vjust = 1.5,
            color = "white",
            size = 4) +
  scale_fill_manual(values = colors, name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )























