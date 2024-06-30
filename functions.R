######################################################
#####           preamble and functions           #####
######################################################
# Make a common function for the paste
paste_fun <- function(task, stage, options = ''){ return(sub(" ", "_",paste0("Aufgabe.", task, "-Stage.", stage, options)))}

## title 
title_fun <- function(data, task_name, stage, count = FALSE, main = TRUE){
  # Fallback method for main
  if(nrow(data) == 0 & main){
    title <- 'Bitte wählen Sie rechts im Dropdown-Menü eine Aufgabe und Stage aus.'
  }
  
  if(nrow(data) > 0 & count & main){
    title <- paste("Aufgabe:", task_name, " - Stage: ", stage," Total =", sum(data$Anzahl))
  }
  
  if(nrow(data) == 0 & !main & !count){
    title <- '...'
  } 
  
  if(nrow(data) > 0 & !count){
    title <- paste("Aufgabe:", task_name, " - Stage: ", stage)
  }

  return(title)
}

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

# ## new try with break argument
# truncate_and_wrap <- function(x, width = 40, max_chars = 50) {
#   if (is.na(x)) {
#     return("")
#   } else if (nchar(x) <= max_chars) {
#     return(x)
#   } else {
#     return(paste(strwrap(x, width = width), collapse = '<br>'))
#   }
# }

# Make a common function for trimming master id
extract_suffix <- function(master_id) {
  suffix <- gsub("^.*_(\\w+_\\d+)$", "\\1", master_id)
  return(suffix)
}

# Function to check if the provided password matches the stored hashed password
check_password <- function(username, password) {
  hashed_password_user <- user_base %>%
    filter(user == username) %>%
    pull(password)
  hashed_input_password <- hash_password(password)
  identical(hashed_password_user, hashed_input_password)
}

### export function for DT Table
download_button <- function(task_name, stage, options = ' ',...){
  return(
    list(paging = FALSE,
         dom = 'Bfrtip',
         buttons = list( 
           list(extend = 'csv',   filename = paste_fun(task_name,stage, options = options), title = paste_fun(task_name,stage, options = options)),
           list(extend = 'excel', filename = paste_fun(task_name,stage, options = options), title = paste_fun(task_name,stage, options = options)),
           list(extend = 'copy'))
         )
  )
}

# derive color palet for plotly
get_color_fun <- function(data, ...){
  return(
    data %>%
      dplyr::ungroup() %>%
      dplyr::distinct(points_individual, colour) %>% 
      dplyr::arrange(points_individual) %>%
      pull()
  )
}

# get the ordered shortened x labels for the plot
get_x_label_trimmed <- function(data, ...){
  return(
    data %>%
      dplyr::arrange(x_label) %>%
      dplyr::select(x_label, x_label_trimmed) %>%
      dplyr::distinct(x_label, .keep_all = TRUE) %>%
      pull(x_label_trimmed)
  )
}

# plot function
plot_fun <- function(data, ...){
  
  data %<>%
    dplyr::ungroup()
  
  # getting all master_ids
  master.ids = data %>% dplyr::distinct(master_id) %>% pull()
  
  # list to store all subplots
  plotlylist = list()
  
  for(i in 1:length(master.ids)){
    
    # filter data for the master id 
    filtered_data <- data %>%
      dplyr::filter(master_id %in% master.ids[i]) %>%
      dplyr::arrange(x_label)
    
    # get timmed x_labels 
    x_label_trimmed <- get_x_label_trimmed(filtered_data)
    
    # Calculate the midpoint of the x-axis
    # work around for the title function, which does not work
    x_midpoint <- (length(unique(filtered_data$x_label)) - 1) / 2
    
    # Plot
    plotlylist[[i]] <- filtered_data %>%
      plot_ly(x = ~x_label, 
              y = ~percent, 
              color = ~points_individual, 
              colors = get_color_fun(filtered_data), 
              text = ~paste(#"Feldinhalt :", ~ feldinhalt_trimmed,
                "<br> Anzahl :", n_i," von ", N,
                "<br> Proportion :", round(percent, 2), 
                '<br> Text:', x_label),
              textposition = "none",
              hoverinfo = "text") %>% # ,width = 400
      add_bars() %>%
      layout(barmode = "stack",
             showlegend = FALSE,
             yaxis = list (title = "Prozent") ,
             xaxis = list(
               ticktext = as.list(x_label_trimmed), 
               tickvals = as.list(0:(length(x_label_trimmed)-1)),
               tickmode = "array",
               tickangle = 90
            ),
             margin = list(l = 5, r = 10,
                           b = 100, t = 20
                           #,pad = 5
                           )
) %>%
      hide_colorbar()  %>% 
      add_annotations(text = master.ids[i], 
                      font = list(size = 16,
                        color = "#004c93"),
                        x = x_midpoint, y = 107, showarrow = FALSE)
    # annotation instead of of title, as title sometimes has glitches 
  }
  
  # combine the subplots
  plot <- subplot(plotlylist, titleX = FALSE, shareY = TRUE) 
  #%>% layout(barmode = 'stack', showlegend = FALSE)
  return(plot)
}

# wrap function to distingush the different exercise types
wrap_plot <- function(data, x, type){
  
  # MC Stage
  if(type == 'Multiple-Choice'){
    ## data wrangling
    new_data <- data %>%
      dplyr::arrange({{x}}) %>%
      dplyr::rename('x_label' = {{x}}) %>%
      dplyr::rename('x_label_trimmed' = paste0({{x}}, '_trimmed'))
    
    # plotting
    plot <- plot_fun(new_data)
  }
  
  # Dropdown Stage
  if(type == 'Dropdown'){
    plot <- 'Dropdown'
  }
  
  # Fill-In Stage
  if(type == 'Fill-In'){
    plot <- 'Fill-In'
  }
  
  return(plot)
}

