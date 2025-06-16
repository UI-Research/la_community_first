####----Setup----####

library(tmap)
library(cols4all)
library(tidyverse)
library(tidycensus)
library(readxl)
library(openxlsx)
library(geofacet)
library(sf)
library(scales)
library(osmdata)
library(urbnthemes)
options(scipen=999)
set_urbn_defaults(style="print")

##WCG: try to never use paths with a hard-coded username--these are destined to fail
username = getwd() %>% str_split("\\/") %>% unlist %>% .[3]
file_path <- file.path("C:", "Users", username, "Box", "LA Transit project/Social Climate Analysis")


#set fips codes
state_fips <- "06"
county_fips <- "037"

tmap_mode("view")

# Create the map object

pico_population_map_1 <- 



# Save as SVG
tmap_save(
  tm = pico_population_map_1,
  filename = file.path(file_path, "Pico/Outputs/pico_population_map_1.svg"),
  width = 8,
  height = 6,
  units = "in"
)

pico_population_map_2 <- tm_shape(pop_wide_pico) +
  tm_basemap("CartoDB.PositronNoLabels") +
  tm_polygons(
    col = "population",
    palette = c("#87A0B4", "#1A4378"),
    style = "cont",
    border.col = "white",
    border.lwd = 0.3,
    title = "",
    popup.format = list(fun = function(x) scales::comma(round(x))),
    legend.format = list(fun = function(x) scales::comma_format()(x)),
    na.show = FALSE
  ) +
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "black", lwd = 2) +
  tm_shape(pico_blvd) +
  tm_lines(col = "black", lwd = 2.5, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text("name", size = 0.6, col = "black", shadow = TRUE, auto.placement = TRUE) +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.bg.color = NA,
    legend.bg.alpha = 0,
    legend.frame = FALSE
  )

# Save as SVG
tmap_save(
  tm = pico_population_map_2,
  filename = file.path(file_path, "Pico/Outputs/pico_population_map_2.svg"),
  width = 8,
  height = 6,
  units = "in"
)

pico_population_map_3 <- tm_shape(pop_wide_pico) +
  tm_basemap("Esri.WorldStreetMap") +
  tm_polygons(
    col = "population",
    palette = c("#87A0B4", "#1A4378"),
    style = "cont",
    border.col = "white",
    border.lwd = 0.3,
    title = "",
    popup.format = list(fun = function(x) scales::comma(round(x))),
    legend.format = list(fun = function(x) scales::comma_format()(x)),
    na.show = FALSE
  ) +
  tm_shape(pico_buffer_outline) +
  tm_borders(col = "black", lwd = 2) +
  tm_shape(pico_blvd) +
  tm_lines(col = "black", lwd = 2.5, alpha = 0.8) +
  tm_tiles("CartoDB.PositronOnlyLabels", group = "Streets & Labels") +
  tm_shape(pico_streets_single) +
  tm_text("name", size = 0.6, col = "black", shadow = TRUE, auto.placement = TRUE) +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_layout(
    frame = FALSE,
    legend.position = c("left", "bottom"),
    legend.bg.color = NA,
    legend.bg.alpha = 0,
    legend.frame = FALSE
  )

# Save as SVG
tmap_save(
  tm = pico_population_map_3,
  filename = file.path(file_path, "Pico/Outputs/pico_population_map_3.svg"),
  width = 8,
  height = 6,
  units = "in"
)


ggsave(file.path(file_path, "Pico/Outputs/population_map_pico.png"), width = 14, height = 6, dpi = 300)







       