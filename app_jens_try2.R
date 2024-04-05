# Sources the packages used
source(here::here("packages.R"))

# load and modify the data
# Data for the overall tab
load(here::here("data_overall.RData"))
# Data for the Student's Input tab

#load(here::here("results_per_input.RData"))

# Change later # inefficent 
#data <- example_data # later change 
#data_overall = points_per_stage
#data_sinput = results_per_input
load(here::here("data_sinput.RData"))


# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("user11", "user2"),
  password = sapply(c("pass11", "pass2"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two") 
)



# Make a common function for the paste
paste_fun <- function(task, stage){ return(paste("Aufgabe:", task, " - Stage: ", stage)) }
# Make a function for wrapping texts
truncate_and_wrap <- function(x, width = 18, max_chars = 30) {
  if (is.na(x)) {
    return("")
  } else if (nchar(x) <= max_chars) {
    return(x)
  } else {
    # Truncate to the first 18 characters
    truncated_label <- substr(x, 1, width) 
    
    # Get the median character
    median_character <- substr(x, nchar(x) %/% 2 + 1, nchar(x) %/% 2 + 4)
    
    # Get the last three characters
    last_three_chars <- substr(x, nchar(x) - 2, nchar(x))
    
    # Combine all components with "..." in between
    truncated_label <- paste0(truncated_label, "...", median_character,"...", last_three_chars) 
    
    return(truncated_label)
  }
}





######################################################
#####                   UI                       ##### 
######################################################

ui <- fluidPage(
  
  # CSS file
  includeCSS("format.css"),
  
  # logout button
  div(class = "pull-top", 
      p(class = 'own_h' ,' '), 
      p(shinyauthr::logoutUI(id = "logout",
                             label = 'Abmelden'))
  ),
  
  ###### ---------- Login page #####
  shinyauthr::loginUI(id = "login", 
                      title = 'Anmelden',
                      user_title = 'Benutzername',
                      pass_title = 'Passwort', 
                      login_title = 'Anmelden', 
                      additional_ui = 
                        shiny::tagList(
                          tags$p(
                            " ", 
                            class = "text-center"),
                          HTML("<center><div style=padding-top:30px;padding-bottom:5px;font-size:12px><p> Haben Sie ihr <a href=mailto:jens.klenke@vwl.uni-due.de class='login'>Passwort vergessen</a>?</p></div></center>")
                        )
  ),
  
  #####----- Dashboard UI -----#####
  uiOutput("dashboard_ui")
  
) # End of UI


######################################################
#####                 server                     ##### 
######################################################

server <- function(input, output, session) {
  
  #### Credential check
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout BUttion 
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  # Function to check if the provided password matches the stored hashed password
  check_password <- function(username, password) {
    hashed_password_user <- user_base %>%
      filter(user == username) %>%
      pull(password)
    hashed_input_password <- hash_password(password)
    identical(hashed_password_user, hashed_input_password)
  }
  
  
  # Dashboard UI after successful login
  output$dashboard_ui <- renderUI({
    req(credentials()$user_auth)
    
    
    dashboardPage(
      title = 'Analyseboard für Deutsch als Zweit- und Fremdsprache',
      
      # Dashboard header
      dashboardHeader(title = 'DaZ / DaF', titleWidth = 230),
      # Dashboard Sidebar
      dashboardSidebar(
        sidebarMenu(
          menuItem("Stages", tabName = "Overall"),
          menuItem("Eingabefelder", tabName = "Sinput")
        )
      ), # end of dashboardSidebar
      # dashboard Body
      dashboardBody(
        # Create left and Right margins or the Dashboard Controlbar
        includeCSS("format.css"),
        tabItems(
          tabItem(
            tabName = "Overall",
            dashboardControlbar(
              width = 250,  # Adjust the width as needed
              #Checkbox menu for Module
              checkboxGroupInput("module_overall", "Module", choices = sort(unique(data_overall$modulcode)), selected = sort(unique(data_overall$modulcode))),
              # Dropdown menu for Task Name
              selectizeInput("task_name_overall", "Aufgabe", choices = c(' ', sort(unique(data_overall$exercise_name)))),
              # Dropdown menu for Stage
              selectizeInput("stage_overall", "Stage", choices = NULL),
              # Radio button for versions
              checkboxGroupInput("master_id_overall", "Master ID", choices = NULL)
            ), # end of dashboard controlbar
            fluidRow(
              column(width = 10,
                     box(
                       title = uiOutput("plot_overall_title"),
                       status = "primary",
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       width = 12,  # Set the width (out of 12 columns)
                       height = 200, # Set the height in pixels
                       plotlyOutput("plot_overall") 
                     ),#End of Box
                     box(
                       title = uiOutput("table_overall_title"),
                       status = "primary",
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       width = 12,  # Set the width (out of 12 columns)
                       height = 200, # Set the height in pixels
                       DTOutput('table_output_overall')
                     )#End of Box
              )#End of column
            ) #End of fluidRow
          ), # end of tab 1
          # TAB 2
          tabItem(
            tabName = "Sinput",
            dashboardControlbar(
              width = 250,  # Adjust the width as needed
              # Dropdown menu for Rask Name
              #Checkbox menu for Module
              checkboxGroupInput("module_sinput", "Module", choices = sort(unique(data_sinput$modulcode)), selected = sort(unique(data_sinput$modulcode))),
              # Dropdown menu for Task Name
              selectizeInput("task_name_sinput", "Aufgabe", choices = sort(unique(data_sinput$exercise_name))),
              # Dropdown menu for Stage
              selectizeInput("stage_sinput", "Stage", choices = NULL),
              # Conditional input based on filtered data
              uiOutput("conditional_input")
              
              
              
              
              
            ), # end of dashboard controlbar
            fluidRow(
              column(width = 10,
                     box(
                       title =  uiOutput("plot_sinput_title"),
                       status = "primary",
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       width = 12,  # Set the width (out of 12 columns)
                       height = 200, # Set the height in pixels
                       uiOutput("conditional_plot_panel")  # Conditional panel for plots
                     ),#End of Box
                     box(
                       title = uiOutput("table_sinput_title"),
                       status = "primary",
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       width = 12,  # Set the width (out of 12 columns)
                       height = 200, # Set the height in pixels
                       uiOutput("conditional_table_panel")
                     )# End of Box
              )#End of column
            )#End of fluidRow
          ) # end of tab 2
        ) # end of tabItems
      ) # End of dashboard body
    ) # End of dashboard page
    
  })
  
  # Update choices 
  #observeEvent(input$module_overall, {
  
  #choices_1 = reactive({c(unique(data_overall[data_overall$exercise_name == input$task_name_overall, 4]))}) 
  choices_1 = reactive({c(as.character(unlist(unique(data_overall[data_overall$modulcode ==  input$module_overall , 3]))))})
  
  #updateSelectizeInput(session, "task_name_overall", choices = choices_1())
  #})
  
  
  # Update choices 
  observeEvent(input$task_name_overall, {
    
    #choices_1 = reactive({c(unique(data_overall[data_overall$exercise_name == input$task_name_overall, 4]))}) 
    choices_1 = reactive({c(as.integer(unlist(unique(data_overall[data_overall$exercise_name ==  input$task_name_overall, 4]))))}) 
    
    updateSelectizeInput(session, "stage_overall", choices = choices_1())
  })
  
  # Update choices 
  observeEvent(c(input$task_name_overall, input$stage_overall), {
    
    choices_1 = reactive({unique(subset(data_overall, 
                                        #modulcode == input$module_sinput &
                                        exercise_name == input$task_name_overall & 
                                          stage ==  input$stage_overall )$master_id)}) 
    
    updateCheckboxGroupInput(session, "master_id_overall", choices = choices_1(), selected = choices_1())
  })
  
  
  
  
  
  # Update choices 
  observeEvent(c(input$module_sinput,input$task_name_sinput), {
    
    #choices_1 = reactive({c(unique(data_overall[data_overall$exercise_name == input$task_name_overall, 4]))}) 
    choices_1 = reactive({c(as.integer(unlist(unique(data_sinput[data_sinput$modulcode == input$module_sinput & data_sinput$exercise_name ==  input$task_name_sinput, 5]))))}) 
    
    updateSelectizeInput(session, "stage_sinput", choices = choices_1())
  })
  
  
  # Define reactive expression to filter data based on input choices
  filtered_data <- reactive({
    req(input$module_sinput, input$task_name_sinput, input$stage_sinput)
    
    # Filter data based on module_sinput, task_name_sinput, and stage_sinput
    filtered <- c(as.character(unlist(unique(data_sinput[data_sinput$modulcode == input$module_sinput &
                                                           data_sinput$exercise_name == input$task_name_sinput &
                                                           data_sinput$stage == input$stage_sinput, 3]))))[1]
    
    
    
    return(filtered)
  })
  
  # Conditional Input Menu
  output$conditional_input <- renderUI({
    filtered <- filtered_data()
    
    # Conditional logic to display different input based on the value returned by filtered_data
    if (filtered == "Multiple-Choice") {
      # Render input for MC type
      # For example:
      checkboxGroupInput("mc_options", label = HTML("Multiple Choice Questions <br> Master ID"), choices = NULL)
    } else if (filtered == "Dropdown") {
      # Render dropdown menu for dropdown type
      # For example:
      tagList(
        selectizeInput("fieldname_sinput_1", label = HTML("Dropdown Questions <br> Field Name"), choices = NULL),
        checkboxGroupInput("master_id_dropdown_sinput", label = "Master ID", choices = NULL)
      )# End of tagList
      
    } else if (filtered == "Fill-In") {
      # Render input for fill-in type
      # For example:
      checkboxGroupInput("fillin_options", label = HTML("Fill-In Questions <br> Master ID"), choices = NULL)
    } else {
      # Render default input if type is not recognized
      # For example:
      textOutput("invalid_type_message", "Invalid Type")
    }
  })
  
  # Conditional Output
  output$conditional_plot_panel <- renderUI({
    filtered <- filtered_data()
    
    if (filtered == "Multiple-Choice") {
      plotlyOutput("plot_sinput_mc")
    } else if (filtered == "Dropdown") {
      plotlyOutput("plot_sinput_dropdown")
    } else if (filtered == "Fill-In") {
      plotlyOutput("plot_sinput_fillin")  
    } else {
      # Default output if type is not recognized
      plotOutput("INVALID CHOICE")
    }
  })
  
  output$conditional_table_panel <- renderUI({
    filtered <- filtered_data()
    
    if (filtered == "Multiple-Choice") {
      DTOutput('table_mc_sinput')
    } else if (filtered == "Dropdown") {
      DTOutput('table_dropdown_sinput')
    } else {
      # Default output if type is not recognized
      plotOutput("invalid_type_message")
    }
  })
  
  
  
  
  # Inputs for MC type questions - Master ID
  observeEvent(c(input$module_sinput,input$task_name_sinput,input$stage_sinput), {
    
    #choices_1 = reactive({c(unique(data_overall[data_overall$exercise_name == input$task_name_overall, 4]))}) 
    choices_1 = reactive({unique(subset(data_sinput,
                                        !is.na(master_id) &
                                        modulcode == input$module_sinput &
                                          exercise_name == input$task_name_sinput & 
                                          stage ==  input$stage_sinput )$master_id)}) 
    
    updateCheckboxGroupInput(session, "mc_options", choices = choices_1(), selected = choices_1())
  })
  
  # Inputs for Dropdown type questions - Fieldname
  observeEvent(c(input$module_sinput,input$task_name_sinput,input$stage_sinput), {
    
    #choices_1 = reactive({c(unique(data_overall[data_overall$exercise_name == input$task_name_overall, 4]))}) 
    choices_1 = reactive({sort(unique(subset(data_sinput, 
                                             modulcode == input$module_sinput &
                                               exercise_name == input$task_name_sinput & 
                                               stage ==  input$stage_sinput )$feldname))}) 
    
    updateSelectizeInput(session, "fieldname_sinput_1", choices = choices_1())
  })
  
  
  
  
  
  
  # Inputs for Dropdown type questions - Master ID
  observeEvent(c(input$module_sinput,input$task_name_sinput,input$stage_sinput), {
    
    #choices_1 = reactive({c(unique(data_overall[data_overall$exercise_name == input$task_name_overall, 4]))}) 
    choices_1 = reactive({data_overall %>%
        filter(!is.na(master_id),
               modulcode == input$module_sinput &
                 exercise_name == input$task_name_sinput & 
                 stage == input$stage_sinput) %>%
        pull(master_id) %>%
        unique() %>%
        sort()})          # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    updateCheckboxGroupInput(session, "master_id_dropdown_sinput", choices = choices_1(), selected = choices_1())
  })
  
  
  
  # Inputs for Fill-In type questions - Master ID
  observeEvent(c(input$module_sinput,input$task_name_sinput,input$stage_sinput), {
    
    #choices_1 = reactive({c(unique(data_overall[data_overall$exercise_name == input$task_name_overall, 4]))}) 
    choices_1 = reactive({unique(subset(data_sinput,
                                        !is.na(master_id) &
                                          modulcode == input$module_sinput &
                                          exercise_name == input$task_name_sinput & 
                                          stage ==  input$stage_sinput )$master_id)}) 
    
    updateCheckboxGroupInput(session, "fillin_options", choices = choices_1(), selected = choices_1())
  })
  
  
  
  
  
  # Create data for Histogram for the OVERALL tab when versions are not selected
  plotly_hist_data <- reactive({
    
    data_overall %>%
      dplyr::filter(exercise_name == input$task_name_overall,
                    stage == input$stage_overall,
                    modulcode %in% input$module_overall, 
                    master_id %in% input$master_id_overall) 
  })
  
  # Create the histogram for the OVERALL tab when versions are not selected
  output$plot_overall <- renderPlotly({
    
    
    xx=data.frame(table(plotly_hist_data()$punkte))
    
    
    # Calculate proportion
    total <- sum(xx$Freq)
    xx$proportion <- round(xx$Freq / total,2)
    
    plot_ly(x = xx$Var1, y = round(xx$Freq * 100/sum(xx$Freq),2), type = "bar",text = ~paste("Punkte :", xx$Var1,
                                                                                             "<br> Anzahl :", xx$Freq," von ",total,
                                                                                             "<br> Proportion :", xx$proportion),
            textposition = "none",
            hoverinfo = "text" ) %>%
      layout(
        xaxis = list(title = "Punkte"),
        yaxis = list(title = "Percentage"),
        barmode = "group",  # Set bar mode to "group" for side-by-side bars
        bargap = 0.2  # Adjust the bargap to make the bars thinner (you can experiment with different values)
        
      ) 
    
    
  })
  
  
  
  
  
  
  
  
  # Create data for the STUDENT's INPUT section for MC type questions
  plotly_bar_data <- reactive({
    
    data_sinput %>%
      dplyr::filter(
                    exercise_name == input$task_name_sinput,
                    stage == input$stage_sinput,
                    modulcode %in% input$module_sinput
      ) %>%
      dplyr::filter(!is.na(master_id)) %>%
      dplyr::filter(master_id %in% input$mc_options) %>%
      dplyr::mutate(right = dplyr::case_when(
        points_individual == 100 ~ '100',
        TRUE ~ as.character(points_individual) # Assign the unique values as factor levels
      )) %>%
      dplyr::group_by(master_id,feldinhalt) %>%
      dplyr::add_count(name = 'N') %>%
      dplyr::group_by(master_id,feldinhalt, right, N) %>%
      dplyr::count(name = 'n_i') %>%
      #   dplyr::arrange(feldinhalt, desc(right)) %>%
      dplyr::mutate(percent = n_i/N*100) %>%
      dplyr::mutate(feldinhalt_trimmed = truncate_and_wrap(feldinhalt))%>%
      dplyr::mutate(color_values =as.numeric(right) / 100)
    
  })
  
  
  
  # create the stacked bar plots or STUDENT's INPUT section for MC type
  output$plot_sinput_mc <- renderPlotly({
    
    unique_master_id = unique(plotly_bar_data()$master_id)
    
    plotlylist= list()
    
    for( i in 1:length(unique(plotly_bar_data()$master_id))){
      
      
      plotlylist[[i]] = plotly_bar_data() %>% 
        dplyr::filter(master_id == unique_master_id[i]) %>%
        plot_ly(x = ~feldinhalt_trimmed, y = ~percent, color = ~color_values, marker = list(color = ~color_values,
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
               xaxis = list (title = unique_master_id[i])) %>%
        hide_colorbar() 
      #xaxis = list (title = truncate_title(unique_master_id[i])))  
      
      
      
    }
    
    
    subplot(plotlylist, titleX = TRUE, shareY = TRUE)  %>%
      layout(barmode = 'stack', showlegend = FALSE)
    
    
    
  })
  
  
  
  
  
  # Create data for the STUDENT's INPUT section for Dropdown type questions
  plotly_bar_data_dropdown <- reactive({
    
    data_sinput %>%
      dplyr::filter(
                    exercise_name == input$task_name_sinput,
                    stage == input$stage_sinput,
                    modulcode %in% input$module_sinput
      ) %>%
      dplyr::filter(!is.na(master_id)) %>%
      dplyr::filter(master_id %in% input$master_id_dropdown_sinput) %>%
      dplyr::mutate(right = dplyr::case_when(
        points_individual == 100 ~ '100',
        TRUE ~ as.character(points_individual) # Assign the unique values as factor levels
      )) %>%
      dplyr::group_by(master_id,var_value) %>%
      dplyr::add_count(name = 'N') %>%
      dplyr::group_by(master_id,var_value, right, N) %>%
      dplyr::count(name = 'n_i') %>%
      #   dplyr::arrange(feldinhalt, desc(right)) %>%
      dplyr::mutate(percent = n_i/N*100) %>%
      rowwise() %>%
      dplyr::mutate(var_value_trimmed = truncate_and_wrap(var_value))%>%
      dplyr::ungroup() %>%
      dplyr::select(master_id, var_value, N,right,  n_i, percent, var_value_trimmed)%>%
      dplyr::mutate(color_values =as.numeric(right) / 100)%>%
      distinct() 
    
  })
  
  # create the stacked bar plots or STUDENT's INPUT section for Dropdown type
  output$plot_sinput_dropdown <- renderPlotly({
    
    unique_master_id = unique(plotly_bar_data_dropdown()$master_id)
    
    plotlylist= list()
    
    for( i in 1:length(unique(plotly_bar_data_dropdown()$master_id))){
      
      
      
      plotlylist[[i]] = plotly_bar_data_dropdown() %>% 
        dplyr::filter(master_id == unique_master_id[i]) %>%
        plot_ly(x = ~var_value_trimmed, y = ~percent, color = ~color_values, marker = list(color = ~color_values,
                                                                                           colorscale = list(c(0,1), c("red", "green"))), 
                text = ~paste(#"Feldinhalt :", ~feldinhalt_trimmed,
                  "<br> Anzahl :", n_i," von ",N,
                  "<br> Proportion :", round(percent,2)),
                textposition = "none",
                hoverinfo = "text") %>%
        add_bars() %>%
        layout(barmode = "stack",
               showlegend = FALSE,
               yaxis = list (title = "Prozent"),
               xaxis = list (title = unique_master_id[i]))  %>%
        hide_colorbar()  
      
      
      
      
    }
    
    
    subplot(plotlylist, titleX = TRUE, shareY = TRUE)  %>%
      layout(barmode = 'stack', showlegend = FALSE)
    
    
    
  })
  
  
  
  
  
  
  
  # Create data for the STUDENT's INPUT section for Fill-In type questions
  plotly_bar_data_fillin <- reactive({
    
    data_sinput %>%
      dplyr::filter(
        exercise_name == input$task_name_sinput,
        stage == input$stage_sinput,
        modulcode %in% input$module_sinput
      ) %>%
      dplyr::filter(!is.na(master_id)) %>%
      dplyr::filter(master_id %in% input$fillin_options) %>%
      dplyr::mutate(right = dplyr::case_when(
        points_individual == 100 ~ '100',
        TRUE ~ as.character(points_individual) # Assign the unique values as factor levels
      )) %>%
      dplyr::group_by(master_id,feldname) %>%
      dplyr::add_count(name = 'N') %>%
      dplyr::group_by(master_id,feldname, right, N) %>%
      dplyr::count(name = 'n_i') %>%
      #   dplyr::arrange(feldinhalt, desc(right)) %>%
      dplyr::mutate(percent = n_i/N*100) %>%
      dplyr::mutate(color_values =as.numeric(right) / 100)
    
  })
  
  
  
  # create the stacked bar plots or STUDENT's INPUT section for Fill-In type
  output$plot_sinput_fillin <- renderPlotly({
    
    unique_master_id = unique(plotly_bar_data_fillin()$master_id)
    
    plotlylist= list()
    
    for( i in 1:length(unique(plotly_bar_data_fillin()$master_id))){
      
      
      plotlylist[[i]] = plotly_bar_data_fillin() %>% 
        dplyr::filter(master_id == unique_master_id[i]) %>%
        plot_ly(x = ~feldname, y = ~percent, color = ~color_values, marker = list(color = ~color_values,
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
               xaxis = list (title = unique_master_id[i])) %>%
        hide_colorbar() 
      #xaxis = list (title = truncate_title(unique_master_id[i])))  
      
      
      
    }
    
    
    subplot(plotlylist, titleX = TRUE, shareY = TRUE)  %>%
      layout(barmode = 'stack', showlegend = FALSE)
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Create the data table for the OVERALL section when version not selected
  default_table_overall = reactive({
    
    xx=data.frame(table(plotly_hist_data()$punkte))
    x = xx$Var1
    y = xx$Freq
    summ= sum(y)
    z = round(y/summ,2)
    p = z*100
    result_df=data.frame("Punkte" = x, "Anzahl" = y,  "Percentage"= paste(p, "%", sep=""))
    
  })
  
  
  
  # Present the table for the OVERALL section when version not selected
  output$table_output_overall <- renderDT({
    
    req(default_table_overall())
  },extensions = 'Buttons',
  
  options = exprToFunction(
    list(paging = FALSE,
         dom = 'Bfrtip',
         buttons = list( 
           list(extend = 'csv',   filename = paste_fun(input$task_name_sinput,input$stage_sinput), title = paste_fun(input$task_name_sinput,input$stage_sinput)),
           list(extend = 'excel', filename = paste_fun(input$task_name_sinput,input$stage_sinput), title = paste_fun(input$task_name_sinput,input$stage_sinput)),
           list(extend = 'copy')))
  ),# end of options  
  
  
  class = "display"
  )
  
  
  
  
  
  
  
  # Create the data table for the STUDENT'S INPUT section for MC Type
  table_mc_sinput_tab = reactive({
    
    
    plotly_bar_data() %>%
      group_by(master_id,feldinhalt,N) %>%
      summarise(
        Number_of_right = sum(ifelse(right == 'right', n_i, 0)),
        Number_of_false = sum(ifelse(right == 'false', n_i, 0))
      )%>%
      dplyr::mutate(Percentage_right = round( Number_of_right/N * 100,2))%>%
      dplyr::mutate(Percentage_false = round( Number_of_false/N * 100,2))
  })
  
  
  
  # Present the table for the STUDENT'S INPUT section for MC Type
  output$table_mc_sinput <- renderDT({
    
    req(table_mc_sinput_tab())
    
    datatable(table_mc_sinput_tab(), extensions = 'Buttons', options = list(
      paging = FALSE,
      dom = 'Bfrtip',
      buttons = list( 
        list(extend = 'csv', filename = paste(input$task_name_sinput, " - Stage:", input$stage_sinput, 'grouping'), title = paste_fun(input$task_name_sinput,input$stage_sinput)),
        list(extend = 'excel', filename = paste(input$task_name_sinput, " - Stage:", input$stage_sinput, 'grouping'), title = paste_fun(input$task_name_sinput,input$stage_sinput)),
        list(extend = 'copy')
      )
    ))
  }, class = "display")
  
  
  
  
  
  
  # Create the data table for the STUDENT'S INPUT section for Dropdown Type
  table_dropdown_sinput_tab = reactive({
    
    
    plotly_bar_data_dropdown() %>%
      group_by(master_id,var_value,N) %>%
      summarise(
        Number_of_right = sum(ifelse(indicator == 'right', ni, 0)),
        Number_of_false = sum(ifelse(indicator == 'false', ni, 0))
      )%>%
      dplyr::mutate(Percentage_right = round( Number_of_right/N * 100,2))%>%
      dplyr::mutate(Percentage_false = round( Number_of_false/N * 100,2))
    
  })
  
  
  
  # Present the table for the STUDENT'S INPUT section for Dropdown Type
  output$table_dropdown_sinput <- renderDT({
    
    req(table_dropdown_sinput_tab())
    
    datatable(table_dropdown_sinput_tab(), extensions = 'Buttons', options = list(
      paging = FALSE,
      dom = 'Bfrtip',
      buttons = list( 
        list(extend = 'csv', filename = paste(input$task_name_sinput, " - Stage:", input$stage_sinput, 'grouping'), title = paste_fun(input$task_name_sinput,input$stage_sinput)),
        list(extend = 'excel', filename = paste(input$task_name_sinput, " - Stage:", input$stage_sinput, 'grouping'), title = paste_fun(input$task_name_sinput,input$stage_sinput)),
        list(extend = 'copy')
      )
    ))
  }, class = "display")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Create heading for histogram for OVERALL section when versions are not selected
  output$plot_overall_title <- renderUI({
    
    title_text = paste_fun(input$task_name_overall,input$stage_overall)
    h4(title_text)  
  })
  
  # Create heading for data table for OVERALL section when versions are not selected
  output$table_overall_title <- renderUI({
    
    title_text  = paste_fun(input$task_name_overall,input$stage_overall) 
    h4(title_text)  
  })
  
  # Create heading for histogram for OVERALL section when versions are selected
  output$plot_overall_title_version <- renderUI({
    
    title_text = paste_fun(input$task_name_overall,input$stage_overall)
    h4(title_text)  
  })
  
  # Create heading for data table for OVERALL section when versions are selected
  output$table_overall_title_version <- renderUI({
    
    title_text  = paste_fun(input$task_name_overall,input$stage_overall) 
    h4(title_text)  
  })
  
  
  
} # End of server 

# Run the application
shinyApp(ui, server)
