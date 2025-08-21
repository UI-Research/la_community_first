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
#' @param bins the number of bins to split the scale into
#' @param map_type The type of map to create, one of c("choropleth", "point").
#' @param xpad horizontal buffer for map bounding box
#' @param ypad vertical buffer for map bounding box
#' @param scale_bar_position horizontal position of scale bar
#' @param compass_position horizontal position of compass
#' @param rotate logical indicator for whether the map should be rotated or not
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
    bins = 4,
    save,
    file_extension,
    outpath,
    xpad,
    ypad,
    scale_bar_position = .72,
    compass_position = .70, 
    height,
    width, ## the document is unusually wide, so we use a wider map
    rotate,
    projection = 2229,
    tracts_sf = sf,
    ...) {
  
  
  sf = sf %>% st_transform(projection)
  boulevard_sf = boulevard_sf %>% st_transform(projection)
  locations_sf = locations_sf %>% st_transform(projection)
  study_area_outline_sf = study_area_outline_sf %>% st_transform(projection)
  streets_sf = streets_sf %>% st_transform(projection)
  tracts_sf = tracts_sf %>% st_transform(projection)
  


  ## most legend aesthetics specified in `tm_layout()` below
  legend = tm_legend(
    title = legend_title %>% str_wrap(width = 25), 
    frame = FALSE,
    item.space = .7,
    item.height = .7,
    title.size = .7,
    title.padding = c(.25, .25, .25, .25))
  
  ## setting up bounding box for map
  bbox <- st_bbox(tracts_sf)
  ## Add padding to x or y direction
  bbox["xmin"] <- bbox["xmin"] - xpad
  bbox["xmax"] <- bbox["xmax"] + xpad
  
  bbox["ymin"] <- bbox["ymin"] - ypad
  bbox["ymax"] <- bbox["ymax"] + ypad
  

  
  
  label_type = scales::percent
  if (!str_detect(fill_column, "share|B25071_001|t_ami")) label_type = scales::comma
  if (str_detect(fill_column, "B19013")) label_type = scales::dollar
  
  if (map_type == "choropleth") {
    ## set the scale depending on the type of input data
    scale = tm_scale_intervals(
      n = bins,
      style = "pretty",
      values = palette_colors,
      label.format = label_type)
    
    if (style == "cat") {
      
      scale = tm_scale_categorical(
        values = palette_colors,
        label.format = list(fun = function(x) scales::comma_format()(x))) 

      map1 = 
        tm_shape(sf, bbox = bbox, unit = "mi") +
        #tm_shape(tracts_sf %>% st_buffer(3000), bbox = bbox, is.main = TRUE, unit = "mi") +
        tm_polygons(
          fill = fill_column,
          fill_alpha = .5,
          fill.scale = scale,
          fill.legend = legend,
          col = border_color,
          col_alpha = .1 ) 
      } else {
    
    map1 = 
      #tm_shape(tracts_sf %>% st_buffer(3000), bbox = bbox, is.main = TRUE, unit = "mi") +
      tm_shape(sf, bbox = bbox, unit = "mi") +
      tm_polygons(
        fill = fill_column,
        fill_alpha = if_else(style == "cat", .5, .7),
        fill.scale = scale,
        fill.legend = legend,
        col = border_color) }
  }
    
  if (map_type == "point") {
    map1 = 
      tm_shape(tracts_sf %>% st_buffer(3000), bbox = bbox, unit = "mi") +
      tm_shape(sf, bbox = bbox) +
      tm_dots(
        fill.scale = tm_scale_categorical(values = palette_colors),
        fill = fill_column,
        fill_alpha = 0.55,
        size = 0.5,
        fill.legend = legend) 
  }
  

  ## points of interest
  map2 = 
    ## study area outline
    tm_shape(boulevard_sf %>% st_buffer(2640), bbox = bbox, unit = "mi") +
      tm_borders(col = "#9e400f", lwd = 2, lty = "dashed") +
    ## the boulevard
    tm_shape(boulevard_sf, bbox = bbox) +
      tm_lines(col = "#F4AB0B", lwd = 4) +
    tm_add_legend(
      type = "lines",
      col = c("#F4AB0B", "#9e400f"),
      lty = c("solid", "dashed"),
      lwd = c(2, 2),
      labels = c("Project Site", "Study Area"),
      frame.lwd = 0,
      position = tm_pos_on_top("left", "top"),
      bg.color = "white",    
      bg.alpha = 0.9,
      title.size = .8,        
      title.color = "black",  
      text.color = "black",   
      title.fontface = "bold"
    ) +
    tm_add_legend(
      type = "symbols",
      fill = "black",
      size = 0.5,
      labels = "Points of Interest",
      frame.lwd = 0,
      position = tm_pos_on_top("left", "top"),
      bg.color = "white",
      bg.alpha = 0.9,
      title.size = .8,
      title.color = "black",
      text.color = "black",
      title.fontface = "bold"
    ) +
    tm_shape(
      locations_sf %>% mutate(name = str_wrap(name, 20)), bbox = bbox) +
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
    tm_shape(
      streets_sf, bbox = bbox) +
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
      group_id = "bottom_right",
      position = c(compass_position, .09),
      stack = "horizontal"
      ) +
    ## a scalebar
    tm_scalebar(
      breaks = c(0, .5, 1),
      group_id = "bottom_right",
      position = c(scale_bar_position, .07),
      text.size = .7,
      stack = "horizontal"
      ) +
    ## the legend
    tm_layout(
      legend.position = tm_pos_on_top("left", "bottom"),
      frame = FALSE,
      legend.frame = FALSE,
      #legend.width = 8,
      legend.bg.color = "white",    
      legend.bg.alpha = 0.9,
      legend.title.size = .8,        
      legend.title.color = "black",  
      legend.text.color = "black",   
      legend.title.fontface = "bold") +
    ## the basemap
    tm_basemap("Esri.WorldGrayCanvas")
  

  if (map_type == "point") {
    map = map2 + map1 } else {
    map = map1 + map2
    }
  

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
