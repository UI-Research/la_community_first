library(tidyverse)
library(urbnthemes)

## Function to compute group shares
compute_shares <- function(df, var_list, total_var) {
  total_population <- sum(df[[total_var]], na.rm = TRUE)
  
  #this is to make sure we're not dividing by zero
  if (is.na(total_population) || total_population == 0) {
    stop("Total population is NA or zero — check total_var input!") 
  }
  
  shares <- sapply(var_list, function(vars) {
    selected <- df %>% select(all_of(vars))
    numeric_data <- selected %>% mutate(across(everything(), ~ as.numeric(.)))
    row_total <- rowSums(numeric_data, na.rm = TRUE)
    sum(row_total, na.rm = TRUE) / total_population
  })
  
  tibble(!!!setNames(as.list(shares), paste0("share_", names(shares))))
}

##Function to reshape to long format and label groups
prepare_long_data <- function(
    df, 
    area_name, 
    group_label_map, 
    col_prefix = "share_", 
    group_col = "group") {
  
  df %>%
    select(starts_with(col_prefix)) %>%
    mutate(area = area_name) %>%
    pivot_longer(cols = starts_with(col_prefix), names_to = group_col, values_to = "share") %>%
    mutate(
      !!group_col := recode(.data[[group_col]], !!!group_label_map),
      label = paste0(round(share * 100), "%"))
}

### Function to create bar plot ###
plot_comparison <- function(
    df, 
    x_var = "group", 
    fill_var = "area", 
    y_var = "share",
    label_var = "label", 
    colors, 
    group_order, 
    title) {
  
  df <- df %>%
    mutate(
      !!sym(x_var) := factor(
        stringr::str_wrap(.data[[x_var]], width = 12), 
        levels = stringr::str_wrap(group_order, width = 12)),
      !!sym(fill_var) := factor(.data[[fill_var]], levels = names(colors)))
  
  df %>%
    ggplot(
      aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[fill_var]])) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(
      aes(label = .data[[label_var]]),
      position = position_dodge(width = 0.8),
      vjust = -.5,
      color = "black",
      size = 8,
      size.unit = "pt") +
    scale_fill_manual(values = colors, name = NULL) +
    scale_y_continuous(
      expand = expansion(mult = c(0,0.1)),
      labels = scales::percent_format(accuracy = 1)) +
    labs(
      x = NULL,
      y = NULL,
      title = title) +
    theme_urbn_print() +
    theme(
      legend.position = "top",
      panel.grid.major.x = element_blank(),
      text = element_text(family = "Arial", color = "black"))
}

plot_indicator <- function(
    df_boulevard = df_boulevard, 
    df_city = df_city, 
    var_list, 
    total_var,
    labels, 
    group_order, 
    title = NULL, 
    constructs,
    colors = color_palette,
    save = save,
    file_extension = file_extension,
    outpath = outpath,
    ...) {

  var_list = get(var_list, envir = sys.frames()[[1]])
  labels = get(labels, envir = sys.frames()[[1]])
  group_order = get(group_order, envir = sys.frames()[[1]])
  
  pico <- compute_shares(df_boulevard, var_list, total_var)
  la <- compute_shares(df_city, var_list, total_var)
  
  pico_long <- prepare_long_data(pico, "Pico", labels)
  la_long   <- prepare_long_data(la, "LA City", labels)
  
  combined <- bind_rows(pico_long, la_long)
  
  plot = plot_comparison(
    df = combined,
    colors = colors,
    group_order = group_order,
    title = title)
  
  if (save == TRUE) {
    ggsave(
      plot = plot,
      filename = file.path(
        outpath, 
        str_c(constructs %>% janitor::make_clean_names(), file_extension)),
      width = 6.5,
      height = 2.5,
      units = "in",
      dpi = 1000) }
  
  return(plot)

}
