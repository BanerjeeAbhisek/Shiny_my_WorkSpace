# packages
source(here::here("packages.R"))

# load data 
load(here::here("example_data_shiny.RData"))

example_data %>%
  dplyr::distinct(exercise_name) %>%
  dplyr::arrange(exercise_name)

# Grouping variables for now 
c('stage', 'var_value')

# var value
example_data %>%
  dplyr::filter(exercise_name == '04 Semantik',
                variablenname == 'satzAntonym') %>%
  dplyr::select(var_value) %>%
  dplyr::distinct(var_value) %>%
  # unnest 
  tidyr::unnest(var_value) %>%
  # alphabetic order 
  dplyr::arrange(var_value)

# var value
example_data %>%
  dplyr::filter(exercise_name == '05 Mündlichkeit - Schriftlichkeit',
                variablenname == 'satzKMMM') %>%
  dplyr::select(var_value) %>%
  dplyr::distinct(var_value) %>%
  unlist()

# resources 
https://t-redactyl.io/blog/2016/01/creating-plots-in-r-using-ggplot2-part-4-stacked-bar-plots.html
https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2


  



