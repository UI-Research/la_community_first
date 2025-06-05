### Load the script for pico data ###
source("C:/Users/tmaginn/Documents/GitHub/la_community_first/data_pull.R")
source("C:/Users/gsamuels/Documents/GitHub/la_community_first/data_pull.R")



# Set working directory and file paths
username <- getwd() %>% str_split("\\/") %>% unlist() %>% .[3]
file_path <- file.path("C:", "Users", username, "Box", "LA Transit project/Social Climate Analysis")

# Call the function and store the result
pico_master_data <- clean_and_prepare_data(file_path)


### same thing as above but now loading in script for LA city data ###

source("C:/Users/tmaginn/Documents/GitHub/la_community_first/la_city_data_pull.R")
source("C:/Users/gsamuels/Documents/GitHub/la_community_first/la_city_data_pull.R")


username <- getwd() %>% str_split("\\/") %>% unlist() %>% .[3]
file_path <- file.path("C:", "Users", username, "Box", "LA Transit project/Social Climate Analysis")

# Call the function and store the result
la_city_master_data <- clean_and_prepare_data(file_path)