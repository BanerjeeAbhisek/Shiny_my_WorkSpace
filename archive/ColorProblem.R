# Sources the packages used
source(here::here("packages.R"))

# load Dataa..
load(here::here("data_overall.RData"))
load(here::here("data_sinput.RData"))


# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("user11", "user2"),
  password = sapply(c("pass11", "pass2"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two") 
)

# truncate labels
truncate_and_wrap <- function(x, width = 25, max_chars = 30) {
  if (is.na(x)) {
    return("")
  } else if (nchar(x) <= max_chars) {
    return(x)
  } else {
    truncated_label <- substr(x, 1, width)  # Truncate to max_chars characters
    last_three_chars <- substr(x, nchar(x) - 3, nchar(x)-1)  # Get last three characters
    truncated_label <- paste0(truncated_label, "...", last_three_chars )  # Insert ... after max_chars-th character
    return(truncated_label)
    
  }
}

# Make a common function for the paste
paste_fun <- function(task, stage){ return(paste("Aufgabe:", task, " - Stage: ", stage)) }
# Make a function for wrapping texts
truncate_and_wrap <- function(x, width = 20, max_chars = 30) {
  if (is.na(x)) {
    return("")
  } else if (nchar(x) <= max_chars) {
    return(x)
  } else {
    # Truncate to the first 18 characters
    truncated_label <- substr(x, 1, width) 
    
    # Get the median character
    #median_character <- substr(x, nchar(x) %/% 2 + 1, nchar(x) %/% 2 + 4)
    
    # Get the last three characters
    last_five_chars <- substr(x, nchar(x) - 5, nchar(x))
    
    # Combine all components with "..." in between
    #truncated_label <- paste0(truncated_label, "...", median_character,"...", last_three_chars) 
    truncated_label <- paste0(truncated_label, "...", last_five_chars) 
    
    return(truncated_label)
  }
}

# Make a common function for trimming master id
extract_suffix <- function(master_id) {
  suffix <- gsub("^.*_(\\w+_\\d+)$", "\\1", master_id)
  return(suffix)
}

extract_suffix <- function(master_id) {
  suffix <- gsub("^.*_(\\w+_\\d+)$", "\\1", master_id)
  return(suffix)
}



green_pal <- colorRampPalette(c("lawngreen", "green4"))
red_pal <- colorRampPalette(c("red", "coral"))
color_df <- tibble::tibble(
  color_value = as.character(0:100),
  colour = c(red_pal(51),
             green_pal(50))
)
data_sinput_1 <- data_sinput %>%
  mutate(color_value = as.character(points_individual))
data_sinput_1 <- data_sinput_1 %>%
  dplyr::left_join(color_df,
                   by = 'color_value')



# Example data
m = c("ZAN71000","ZAN71004")
t = "02 Bildungsbenachteiligung"
s = 1


dt <- data_sinput %>%
  dplyr::filter(modulcode %in% m,
                exercise_name == t,
                stage == s
  )



mc = unique(extract_suffix(dt$master_id))


final_do =data_sinput_1 %>%
  dplyr::filter(modulcode %in% m,
                exercise_name == t,
                stage == s
  ) %>%
  dplyr::filter(!is.na(master_id)) %>%
  dplyr::filter(!is.na(feldinhalt)) %>%
  dplyr::filter(extract_suffix(master_id) %in% mc) %>%
  dplyr::mutate(right = dplyr::case_when(
    points_individual == 100 ~ '100',
    TRUE ~ as.character(points_individual) # Assign the unique values as factor levels
  )) %>%
  dplyr::mutate(master_id = extract_suffix(master_id))%>%
  dplyr::group_by(master_id,feldinhalt) %>%
  dplyr::add_count(name = 'N') %>%
  dplyr::group_by(master_id,feldinhalt, right, N,colour) %>%
  dplyr::count(name = 'n_i') %>%
  #   dplyr::arrange(feldinhalt, desc(right)) %>%
  dplyr::mutate(percent = round(n_i/N*100,2)) %>%
  dplyr::mutate(feldinhalt_trimmed = truncate_and_wrap(feldinhalt))



unique_master_id = unique(final_do$master_id)

plotlylist= list()



i=1

for( i in 1:length(unique(final_do$master_id))){
  
  filtered_data <- final_do %>%
    dplyr::filter(master_id == unique_master_id[i])
  
  # Calculate the midpoint of the filtered data for the x-axis
  x_midpoint <- (length(unique(filtered_data$feldinhalt)) - 1) / 2
  
  
  
  plotlylist[[i]] = final_do %>% 
    dplyr::filter(master_id == unique_master_id[i]) %>%
    plot_ly(x = ~feldinhalt, y = ~percent, color = ~right, colors = ~colour , 
            text = ~paste(#"Feldinhalt :", ~feldinhalt_trimmed,
              "<br> Anzahl :", n_i," von ",N,
              "<br> Proportion :", round(percent,2)),
            textposition = "none",
            hoverinfo = "text") %>%
    add_bars() %>%
    layout(barmode = "stack",
           showlegend = FALSE,
           #xaxis = list(title = paste(unique_feldinhalt[i]), tickangle = 45),
           yaxis = list (title = "Prozent"),
           xaxis = list (title =NULL, 
                         tickvals = ~feldinhalt,
                         tickangle = 90,
                         ticktext = ~feldinhalt_trimmed)) %>%
    hide_colorbar() %>% add_annotations(text =  unique_master_id[i] ,x = x_midpoint,y=107, showarrow = FALSE)
  
  
}


subplot(plotlylist, titleX = FALSE, shareY = TRUE)  %>%
  layout(barmode = 'stack', showlegend = FALSE)




#### sub plot try
i = 1

final_do %>% 
  dplyr::filter(master_id == unique_master_id[i]) %>%
  plot_ly(x = ~feldinhalt, y = ~percent, color = ~color_values, 
          marker = list(color = ~color_values,
                        colorscale = list(c(0,1), c("red", "green"))), 
          text = ~paste(#"Feldinhalt :", ~feldinhalt_trimmed,
            "<br> Anzahl :", n_i," von ",N,
            "<br> Proportion :", round(percent,2)),
          textposition = "none",
          hoverinfo = "text") %>%
  add_bars() %>%
  layout(barmode = "stack",
         showlegend = FALSE,
         #xaxis = list(title = paste(unique_feldinhalt[i]), tickangle = 45),
         yaxis = list (title = "Prozent"),
         xaxis = list (title =NULL, 
                       tickvals = ~feldinhalt,
                       tickangle = 90,
                       ticktext = ~feldinhalt_trimmed)) %>%
  hide_colorbar() %>% add_annotations(text =  unique_master_id[i] ,x = x_midpoint,y=107, showarrow = FALSE)

final_do %>%
  dplyr::filter(master_id == unique_master_id[i]) %>%
  plot_ly(x = ~feldinhalt, y = ~percent, color = ~right,
          colors = ~colour,
          text = ~paste(#"Feldinhalt :", ~feldinhalt_trimmed,
            "<br> Anzahl :", n_i," von ",N,
            "<br> Proportion :", round(percent,2)),
          textposition = "none",
          hoverinfo = "text") %>%
  add_bars() %>%
  layout(barmode = "stack",
       showlegend = FALSE,
       #xaxis = list(title = paste(unique_feldinhalt[i]), tickangle = 45),
       yaxis = list (title = "Prozent"),
       xaxis = list (title =NULL, 
                     tickvals = ~feldinhalt,
                     tickangle = 90,
                     ticktext = ~feldinhalt_trimmed)) %>%
  hide_colorbar() %>% add_annotations(text =  unique_master_id[i] ,x = x_midpoint,y=107, showarrow = FALSE)



## colorpalett

green_pal <- colorRampPalette(c("lawngreen", "green4"))
red_pal <- colorRampPalette(c("red", "coral"))

color_df <- tibble::tibble(
  right = as.character(0:100),
  colour = c(red_pal(51),
             green_pal(50))
)

# add colours to the df

final_do <- final_do %>%
  dplyr::left_join(color_df, 
                   by = 'right')
  