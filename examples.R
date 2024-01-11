# packages
source(here::here("packages.R"))

library('plotly')
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
                variablenname == 'satzAntonym',
                feldname == 'dropdown1') %>%
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

red_line <- example_data %>%
  dplyr::filter(exercise_name == '04 Semantik',
                variablenname == 'satzAntonym', 
                stage == 1,
                feldname == 'dropdown1'
  ) %>%
  dplyr::mutate(right = dplyr::case_when(
    punkte == 100 ~ 'right',
    .default = 'false' 
  )) %>%
  dplyr::add_count(name = 'N') %>%
  dplyr::group_by(right, N) %>%
  dplyr::count(name = 'n_i') %>%
  dplyr::mutate(percent = n_i/N*100) %>%
  pull(percent) %>%
  .[2]

plotly_data <- example_data %>%
  dplyr::filter(exercise_name == '04 Semantik',
                variablenname == 'satzAntonym', 
                stage == 1,
                feldname == 'dropdown1'
                ) %>%
  tidyr::unnest(var_value) %>%
  dplyr::mutate(right = dplyr::case_when(
    punkte == 100 ~ 'right',
    .default = 'false' 
  )) %>%
  dplyr::group_by(var_value) %>%
  dplyr::add_count(name = 'N') %>%
  dplyr::group_by(var_value, right, N) %>%
  dplyr::count(name = 'n_i') %>%
  dplyr::arrange(var_value, desc(right)) %>%
  dplyr::mutate(percent = n_i/N*100)

plotly_data %>%
  plot_ly(x = ~var_value, y = ~percent,  color = ~right,
          colors = c('right' = '#008000', 'false' = '#FF0000')) %>%
  add_bars() %>%
  layout(barmode = "stack",
         shapes = list(list(type = "line",line = list(color = "black"),
                            x0 = -0.5, x1 = 12.5,
                            y0 = red_line, y1 = red_line)),
         title = "\n Verteilung der Antworten in Abhängigkeit der Variable",showlegend = FALSE,
         xaxis = list(title = ""),
         yaxis = list (title = "Prozent"))



layout(shapes = list(hline(red_line), list(type = "rect",line = list(color = "black"),
                                      x0 = 0.9, x1 = 2)), plot_bgcolor = "#e5ecf6")



# changing the dataset 
# load data 
load(here::here("example_data_shiny.RData"))

example_data %<>%
  dplyr::filter(!str_detect(variablenname, 'liste'))

save(example_data, file = here::here("example_data_shiny.RData"))
