source("packages.R")
source("functions.R")

# load and modify the data
load("data_overall.RData")
load("data_sinput.RData")

# Define color range from 0 to 100 points
# green part 
green_pal <- colorRampPalette(c("lawngreen", "green4"))
# red part
red_pal <- colorRampPalette(c("red", "coral"))
# data frame with points and colour 
color_df <- tibble::tibble(
  color_value = as.character(0:100),
  colour = c(red_pal(51),
             green_pal(50))
)

# points_individual as character to match
data_sinput <- data_sinput %>%
  mutate(color_value = as.character(points_individual))

# add colour to the original data frame via left_join
data_sinput <- data_sinput %>%
  dplyr::left_join(color_df,
                   by = 'color_value')

##### data preparation
## input in debugging

# Allgemeiner Teil in der App
antwortanalyse_data <-
  data_sinput %>%
  dplyr::filter(Semester %in% input$semester_sinput,
                exercise_name %in% input$task_name_sinput,
                stage %in% input$stage_sinput,
                modulcode %in% input$module_sinput)

# Fill-In Input 
antwortanalyse_data_fillin <- antwortanalyse_data %>%
  dplyr::filter(!is.na(master_id)) %>%
  dplyr::filter(!is.na(points_individual)) %>%
  # is that right? 
  dplyr::filter(extract_suffix(master_id) %in% input$fillin_options) %>%
  dplyr::mutate(master_id = extract_suffix(master_id))%>%
  dplyr::group_by(master_id, feldname) %>%
  dplyr::add_count(name = 'N') %>%
  dplyr::group_by(master_id, feldname, points_individual, N, colour) %>%
  dplyr::count(name = 'n_i') %>%
  dplyr::mutate(percent = round(n_i/N*100,2)) 


# Plot 
unique_master_id = unique(antwortanalyse_data_fillin$master_id)

plotlylist= list()

for( i in 1:length(unique_master_id)){
  
  filtered_data <- antwortanalyse_data_fillin %>%
    dplyr::filter(master_id == unique_master_id[i])
  
  # Calculate the midpoint of the filtered data for the x-axis
  x_midpoint <- (length(unique(filtered_data$feldname)) - 1) / 2
  
  
  plotlylist[[i]] = antwortanalyse_data_fillin %>% 
    dplyr::filter(master_id == unique_master_id[i]) %>%
    plot_ly(x = ~feldname, y = ~percent, color = ~as.factor(points_individual), colors = ~colour , 
            text = ~paste(#"Feldinhalt :", ~feldinhalt_trimmed,
              "<br> Anzahl :", n_i," von ",N,
              "<br> Proportion :", round(percent,2)),
            textposition = "none",
            hoverinfo = "text") %>%
    add_bars() %>%
    layout(barmode = "stack",
           showlegend = FALSE,
           title = list (text = unique_master_id[i]),
           yaxis = list (title = "Prozent"),
           xaxis = list (title = ' ',
                         tickangle = 90)) %>%
    hide_colorbar()  # %>% add_annotations(text =  unique_master_id[i] ,x =x_midpoint,y=107, showarrow = FALSE)
}

subplot(plotlylist, titleX = FALSE, shareY = TRUE)  %>%
  layout(barmode = 'stack', showlegend = FALSE)
