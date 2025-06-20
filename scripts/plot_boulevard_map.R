library(sf)
library(tidyverse)
library(maptiles)
library(tmap)

#' A wrapper function for plotting standard ACS choropleth maps
#'
#' @param sf The tract-level (polygon) spatial data frame containing the data to be visualized.
#' @param boulevard_sf A spatial data frame (polyline) representing the boulevard, used for highlighting in the map.
#' @param fill_column The name of the column in `sf` that contains the data to be visualized as a choropleth.
#' @param locations_sf The spatial data frame (points) containing locations of interest, such as parks or schools, to be plotted on the map.
#' @param study_area_outline_sf A spatial data frame (polygon) representing the outline of the study area, used for context in the map.
#' @param streets_sf A spatial data frame (polyline) containing street data, used for visualizing and labeling streets in the map.
#' @param palette_colors A vector of colors to be used for the choropleth fill. Defaults to a blue palette.
#' @param legend_title The title for the legend in the map.
#' @param style The style of the map, either "cat" for categorical or "cont" for continuous. Defaults to "cat".
#' @param map_type The type of map to create, one of c("choropleth", "point").
#' @param save A logical indicating whether to save the map to file. If `TRUE`, the map will be saved using the specified `file_extension`.
#' @param file_extension The file extension to use when saving the map, e.g., ".png", ".svg". Defaults to ".png".
#' @param outpath The path where the map will be saved.
#' @param height height of saved plot
#' @param width width of saved plot
#' @param ... 
#'
#' @return A tmap object, which in turn can be decorated with additional tmap expressions

plot_boulevard_map = function(
    sf, 
    boulevard_sf,
    fill_column, 
    locations_sf, 
    study_area_outline_sf,
    streets_sf,
    legend_title,
    palette_colors = c("#73bfe2", "#062635"),
    style = "cat",
    map_type = c("choropleth", "point"),
    border_color = "white",
    save,
    file_extension,
    outpath,
    height = 4.5,
    width = 6.5,
    ...) {
  ## for testing
  # sf = acs_data_boulevard
  # fill_column = "B01001_001"
  # style = "cont"
  # map_type = "choropleth"
  # n = 4
  # tmap_mode("plot")
  # legend_title = "Population"
  
  if (map_type == "choropleth") {
    
    ## set the scale depending on the type of input data
    scale = tm_scale_intervals(
      n = 3,
      as.count = TRUE,
      style = "pretty",
      values = palette_colors,
      label.format = list(fun = function(x) scales::comma_format()(x)))
    
    if (style == "cat") {
      scale = tm_scale_ordinal(
        n = 4,
        values = palette_colors,
        label.format = list(fun = function(x) scales::comma_format()(x))) }
    
    map1 = 
      tm_shape(sf, bbox = st_bbox(sf %>% st_buffer(250))) +
      tm_polygons(
        fill = fill_column,
        fill_alpha = .7,
        fill.scale = scale,
        ## most legend aesthetics specified in `tm_layout()` below
        fill.legend = tm_legend(
          title = legend_title, 
          frame = FALSE,
          item.space = .75),
        col = border_color) 
  }
    
  if (map_type == "point") {
    map1 = 
      tm_shape(sf, bbox = st_bbox(sf %>% st_buffer(1000))) +
      tm_dots(
        #palette = palette_colors,
        col = fill_column,
        col_alpha = 0.7,
        # style = "cat",
        # border.col = border_color,
        # border.lwd = 0.3,
        size = 0.5,
        col.legend = tm_legend(
          title = legend_title, 
          frame = FALSE,
          item.space = .75)
      )
  
    }

  
  
  ## points of interest
  map2 = 
    ## study area outline
    tm_shape(boulevard_sf %>% st_buffer(804.672), unit = "mi") +
      tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
    ## the boulevard
    tm_shape(boulevard_sf) +
      tm_lines(col = "#F4AB0B", lwd = 4) +
    tm_shape(
      locations_sf %>% mutate(name = str_wrap(name, 18))) +
    tm_dots(
      col = "black",
      size = 0.4) +
    tm_text(
      "name",
      size = 0.6,
      col = "black",
      options = opt_tm_text(just = "center"),
      ymod = 1.5,
      fontface = "bold",
      fontfamily = "Calibri") +
    tm_shape(streets_sf) +
    tm_text(
      "name",
      size = 0.6,
      col = "black",
      angle = "orientation",
      options = opt_tm_text(just = "center"),
      fontface = "bold",
      fontfamily = "Calibri") +
    ## a compass
    tm_compass(
      type = "arrow", 
      size = 1,
      #group_id = "bottom_right",
      position = c(.72, .12)
      ) +
    ## a scalebar
    tm_scalebar(
      breaks = c(0, 1, 2),
      #group_id = "bottom_right",
      position = c(.75, .1),
      text.size = .7) +
    ## the legend
    tm_layout(
      legend.position = tm_pos_in("left", "bottom"),
      frame = FALSE,
      legend.frame = FALSE,
      legend.width = 8,
      legend.bg.color = "white",    
      legend.bg.alpha = 0.9,
      legend.title.size = .8,        
      legend.title.color = "black",  
      legend.text.color = "black",   
      legend.title.fontface = "bold") +
    ## the basemap
    tm_basemap("Esri.WorldGrayCanvas") #+
    #tm_comp_group("bottom_right", position = tm_pos_on_top("right", "BOTTOM"), stack = "horizontal")
  
  map = map1 + map2
  
  if (save == TRUE) {
    tmap_save(
      tm = map,
      filename = file.path(
        outpath, 
        str_c(legend_title %>% janitor::make_clean_names(), file_extension)),
      width = width,
      height = height,
      dpi = 1000,
      units = "in")
  }
  
  return(map)
}
