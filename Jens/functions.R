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
    title <- 'Waiting for input'
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
           list(extend = 'csv',   filename = paste_fun(task_name,stage, options = options), title = paste_fun(task_name,stage)),
           list(extend = 'excel', filename = paste_fun(task_name,stage, options = options), title = paste_fun(task_name,stage)),
           list(extend = 'copy'))
         )
  )
}
