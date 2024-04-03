# Sources the packages used
source(here::here("packages.R"))

# load and modify the data
# Example data
load(here::here("example_data_shiny.RData"))
# Data for the overall tab
load(here::here("results_points_per_stage.RData"))
# Data for the Student's Input tab
load(here::here("results_per_input.RData"))

# Change later # inefficent 
data <- example_data # later change 
data_overall = points_per_stage
data_sinput = results_per_input

# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("user11", "user2"),
  password = sapply(c("pass11", "pass2"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two") 
)

# Make a common function for the paste
paste_fun <- function(task, stage){ return(paste("Aufgabe:", task, " - Stage: ", stage)) }


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
              selectizeInput("task_name_overall", "Aufgabe", choices = sort(unique(data_overall$exercise_name))),
              # Dropdown menu for Stage
              selectizeInput("stage_overall", "Stage", choices = NULL),
              # Radio button for versions
              radioButtons("version_button", "Version", choices = c("Yes","No"), selected = "No", inline = TRUE)
            ), # end of dashboard controlbar
            fluidRow(
              column(width = 10,
                     conditionalPanel(
                       condition = "input.version_button == 'No'",
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
                     ),#End of conditional panel
                     conditionalPanel(
                       condition = "input.version_button == 'Yes'",
                       box(
                         title = uiOutput("plot_overall_title_version"),
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 12,  # Set the width (out of 12 columns)
                         height = 200, # Set the height in pixels
                         plotlyOutput("plot_overall_version") 
                       ),#End of Box
                       box(
                         title = uiOutput("table_overall_title_version"),
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 12,  # Set the width (out of 12 columns)
                         height = 200, # Set the height in pixels
                         DTOutput('table_output_overall_versions')
                       )#End of Box
                     )#End of conditional panel
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
                       plotlyOutput("plot_sinput") 
                     ),#End of Box
                     box(
                       title = uiOutput("table_sinput_title"),
                       status = "primary",
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       width = 12,  # Set the width (out of 12 columns)
                       height = 200, # Set the height in pixels
                       DTOutput('table_output_sinput') 
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
  
  
  
  
  
  choices_11 = reactive({c(as.character(unlist(unique(data_sinput[data_sinput$modulcode ==  input$module_sinput , 4]))))})
  
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
    if (filtered == "MC") {
      # Render input for MC type
      # For example:
      checkboxGroupInput("mc_options", label = HTML("Multiple Choice Questions <br> Feldinhalt"), choices = NULL)
    } else if (filtered == "dropdown") {
      # Render dropdown menu for dropdown type
      # For example:
      tagList(
      selectizeInput("fieldname_sinput", label = HTML("Dropdown Questions <br> Field Name"), choices = NULL),
      radioButtons("version_sinput", "Version", choices = c("Yes","No"), selected = "No", inline = TRUE)
      )# End of tagList
      
    } else if (filtered == "Fillin") {
      # Render input for fill-in type
      # For example:
      textInput("fill_in_input", "Fill-in Input", value = "")
    } else {
      # Render default input if type is not recognized
      # For example:
      textOutput("invalid_type_message", "Invalid Type")
    }
  })
  
  
  
  
  # Inputs for MC type questions
  observeEvent(c(input$module_sinput,input$task_name_sinput,input$stage_sinput), {
    
    #choices_1 = reactive({c(unique(data_overall[data_overall$exercise_name == input$task_name_overall, 4]))}) 
    choices_1 = reactive({unique(subset(data_sinput, 
                                        modulcode == input$module_sinput &
                                          exercise_name == input$task_name_sinput & 
                                          stage ==  input$stage_sinput )$feldinhalt)}) 
    
    updateCheckboxGroupInput(session, "mc_options", choices = choices_1(), selected = choices_1())
  })
  
  # Inputs for Dropdown type questions
  observeEvent(c(input$module_sinput,input$task_name_sinput,input$stage_sinput), {
    
    #choices_1 = reactive({c(unique(data_overall[data_overall$exercise_name == input$task_name_overall, 4]))}) 
    choices_1 = reactive({sort(unique(subset(data_sinput, 
                                        modulcode == input$module_sinput &
                                          exercise_name == input$task_name_sinput & 
                                          stage ==  input$stage_sinput )$feldname))}) 
    
    updateSelectizeInput(session, "fieldname_sinput", choices = choices_1())
  })
  

  
  
  
  
  # Create data for Histogram for the OVERALL tab when versions are not selected
  plotly_hist_data <- reactive({
    
    data_overall %>%
      dplyr::filter(exercise_name == input$task_name_overall,
                    stage == input$stage_overall,
                    modulcode %in% input$module_overall) 
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
  
  
  
  # Create data for Histogram for the OVERALL tab when versions are selected
  plotly_hist_data_versions <- reactive({
    
    data_overall %>%
      dplyr::filter(exercise_name == input$task_name_overall,
                    stage == input$stage_overall,
                    modulcode == input$module_overall) %>%
      arrange(master_id) %>%
      group_by(master_id, punkte) %>%
      count() %>%
      ungroup() %>%
      group_by(master_id) %>%
      mutate(total_punkte = sum(n)) %>%
      dplyr::mutate(percentage = round(n*100/total_punkte, 2)) %>%
      ungroup()
    
  })
  
  
  # Create the histogram for the OVERALL tab when versions are selected
  output$plot_overall_version <- renderPlotly({
    
    plot_ly(plotly_hist_data_versions(), x = ~punkte, y = ~percentage, type = 'bar', color = ~master_id, colors = "Blues",
            text = ~paste("Version :", master_id,
                          "<br> Punkte :",  punkte,
                          "<br> Anzahl :",  n , " von ",total_punkte,
                          "<br> Percentage :", round( n * 100 /total_punkte,2), "%"), textposition = "none",
            hoverinfo = "text") %>%
      layout(title = "",
             xaxis = list(title = "Punkte", tickvals = ~punkte),
             yaxis = list(title = "Percentage"),
             barmode = 'group',
             showlegend = TRUE)
    
    
  })
  
  
  

  
  # Create data for the STUDENT's INPUT section forMC type questions
  plotly_bar_data <- reactive({
    
    data_sinput %>%
      dplyr::filter(exercise_name == input$task_name_sinput,
                    stage == input$stage_sinput
      ) %>%
      mutate(last_alpha_pos = max(str_locate_all(master_id, "[[:alpha:]]")[[1]])) %>%
      mutate(master_id = substr(master_id, last_alpha_pos, nchar(master_id))) %>%
      
      dplyr::filter(feldinhalt %in% input$mc_options) %>%
      # has to be changed in the final version (probably)
      dplyr::mutate(right = dplyr::case_when(
        points_individual == 100 ~ 'right',
        .default = 'false' 
      )) %>%
      dplyr::mutate(right = factor(right, levels = c('right', 'false'))) %>%
      dplyr::group_by(feldinhalt, master_id) %>%
      dplyr::add_count(name = 'N') %>%
      dplyr::group_by(feldinhalt,master_id, right, N) %>%
      dplyr::count(name = 'n_i') %>%
      #   dplyr::arrange(feldinhalt, desc(right)) %>%
      dplyr::mutate(percent = n_i/N*100)
  })
  
  
  
  # create the stacked bar plots or STUDENT's INPUT section
  output$plot_sinput <- renderPlotly({
    
    # Function to truncate long titles
    truncate_title <- function(title) {
      if (nchar(title) > 10) {
        if (nchar(title) > 50) {
          title <- paste(substr(title, 1, 20), "...")
        }
        return(list(text = title, standoff = 10, textangle = 45))
      } else {
        return(list(text = title))
      }
    }
    
    
    unique_feldinhalt = unique(plotly_bar_data()$feldinhalt)
    
    
    plotlylist= list()
    
    for( i in 1:length(unique_feldinhalt)){
      
     
      
      plotlylist[[i]] = plotly_bar_data() %>% 
        dplyr::filter(feldinhalt == unique_feldinhalt[i]) %>%
        plot_ly(x = ~master_id, y = ~percent, color = ~right, colors = c('right' = '#008000', 'false' = '#FF0000')) %>%
        add_bars() %>%
        layout(barmode = "stack",
               showlegend = FALSE,
               #xaxis = list(title = paste(unique_feldinhalt[i]), tickangle = 45),
               yaxis = list (title = "Prozent"),
               xaxis = list (title = ""))  
      
      
      
    }
    
    
    subplot(plotlylist, titleX = TRUE, shareY = TRUE) %>%
      layout(barmode = 'stack', showlegend = FALSE,xaxis = list(tickangle = 45)) 
    
    
    
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
  
  # Create the data table for the OVERALL section when version selected
  default_table_overall_versions = reactive({
    
    
    #p = round(plotly_hist_data_versions()$n * 100 / sum(plotly_hist_data_versions()$total_punkte),2)
    result_df=data.frame("Versions" = plotly_hist_data_versions()$master_id, "Punkte" = plotly_hist_data_versions()$punkte, "Anzahl" = plotly_hist_data_versions()$n,  "Percentage"= paste(plotly_hist_data_versions()$percentage, "%", sep=""))
    
  })
  
  
  
  # Present the table for the OVERALL section when version selected
  output$table_output_overall_versions <- renderDT({
    
    req(default_table_overall_versions())
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