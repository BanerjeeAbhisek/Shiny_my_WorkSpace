# Sources the packages used
source(here::here("packages.R"))



# load and modify the data
load(here::here("example_data_shiny.RData"))
load(here::here("results_points_per_stage.RData"))

data <- example_data # later change 

data_overall = points_per_stage


# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("user11", "user2"),
  password = sapply(c("pass11", "pass2"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two") 
)








# Make a common function for the paste
paste_fun = function(task, stage){ return(paste("Aufgabe:", task, " - Stage: ", stage)) }



# Make the ui function
ui <- fluidPage(
  
  
  includeCSS("format.css"),
  
  # logout button
  div(class = "pull-top", 
      p(class = 'own_h' ,'Analyseboard für Deutsch als Zweit- und Fremdsprache'), 
      p(shinyauthr::logoutUI(id = "logout",
                             label = 'Abmelden'))
  ),
  
  # login section
  shinyauthr::loginUI(id = "login", 
                      title = 'Anmelden',
                      user_title = 'Benutzername',
                      pass_title = 'Passwort', 
                      login_title = 'Anmelden'),
  
  # Sidebar to show user info after login
  uiOutput("dashboard_ui"),
  
  
  
  
)#end of fluidpage










# Server Function
server <- function(input, output, session) {
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout to hide
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
  
  
  # Conditionally render the UI based on authentication
  output$dashboard_ui <- renderUI({
    req(credentials()$user_auth)
    
    
    dashboardPage(
      title = 'Analyseboard für Deutsch als Zweit- und Fremdsprache',
      
      # Dashboard header
      dashboardHeader( titleWidth = 230),
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
              #selectizeInput("task_name_sinput", "Task Name", choices = NULL), #sort(unique(data$exercise_name))),
              selectizeInput("task_name_sinput", "Aufgabe", choices = sort(unique(data$exercise_name))),
              # Dropdown menu for Stage
              selectizeInput("stage_sinput", "Stage", choices = NULL),
              # Dropdown menu for Field Name
              selectizeInput("fieldname_sinput", "Inputfeld", choices = NULL),
              # Dropdown menu for variable
              selectizeInput("variable_sinput", "Variable", choices = NULL),
              #Checkbox menu for grouping Variable
              checkboxGroupInput("grouping_variable_sinput", "Variablenwert", choices = NULL)
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
  observeEvent(input$task_name_overall, {
    
    #choices_1 = reactive({c(unique(data_overall[data_overall$exercise_name == input$task_name_overall, 4]))}) 
    choices_1 = reactive({c(as.integer(unlist(unique(data_overall[data_overall$exercise_name ==  input$task_name_overall, 4]))))}) 
    
    updateSelectizeInput(session, "stage_overall", choices = choices_1())
  })
  
  
  # Update choices 
  observeEvent(input$task_name_sinput, {
    
    choices_1 = reactive({as.character(unique(data[data$exercise_name == input$task_name_sinput, 3]))}) 
    
    
    updateSelectizeInput(session, "stage_sinput", choices = choices_1())
  })
  
  # Update choices
  observeEvent(c(input$task_name_sinput, input$stage_sinput), {
    choices_fourth_input <- reactive({ unique(data[data$exercise_name == input$task_name_sinput & as.character(data$stage)== input$stage_sinput , 4])}) 
    
    
    updateSelectizeInput(session, "fieldname_sinput", choices = choices_fourth_input())
  })
  
  # Update choices 
  observeEvent(c(input$task_name_sinput, input$stage_sinput, input$fieldname_sinput), {
    choices_fourth_input <- reactive({ unique(data[data$exercise_name == input$task_name_sinput & as.character(data$stage)== input$stage_sinput & data$feldname== input$fieldname_sinput , 7])  %>% unlist() %>% as.character() %>% sort()}) 
    
    
    updateSelectizeInput(session, "variable_sinput", choices = choices_fourth_input() )
  })
  
  
  
  # Update choices 
  observeEvent(c(input$task_name_sinput, input$stage_sinput, input$fieldname_sinput, input$variable_sinput), {
    choices_grouping_variable <- reactive({
      
      
      unique(subset(example_data, 
                    exercise_name == input$task_name_sinput & 
                      stage ==  input$stage_sinput & 
                      feldname == input$fieldname_sinput & 
                      variablenname == input$variable_sinput)$var_value)  %>% unlist() %>% as.character() %>% sort()
    })
    
    updateCheckboxGroupInput(session, "grouping_variable_sinput", choices =  choices_grouping_variable(), selected = choices_grouping_variable())
    
  })
  
  
  
  
  
  
  # Create data for Histogram for the OVERALL tab when versions are not selected
  plotly_hist_data <- reactive({
    
    data_overall %>%
      dplyr::filter(exercise_name == input$task_name_overall,
                    stage == input$stage_overall) 
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
                    stage == input$stage_overall) %>%
      arrange(master_id) %>%
      group_by(master_id, punkte) %>%
      count() %>%
      ungroup() %>%
      dplyr::mutate(percentage = round(n*100/sum(n),2))
    
  })
  
  
  # Create the histogram for the OVERALL tab when versions are selected
  output$plot_overall_version <- renderPlotly({
    
    plot_ly(plotly_hist_data_versions(), x = ~punkte, y = ~percentage, type = 'bar', color = ~master_id, colors = "Blues",
            text = ~paste("Version :", master_id,
                              "<br> Punkte :",  punkte,
                              "<br> Anzahl :",  n , " von ",sum(n),
                              "<br> Percentage :", round( n * 100 /sum(n),2), "%"), textposition = "none",
            hoverinfo = "text") %>%
      layout(title = "",
             xaxis = list(title = "Punkte", tickvals = ~punkte),
             yaxis = list(title = "Percentage"),
             barmode = 'group',
             showlegend = TRUE)
    
    
  })
  
  
  
  
  # Create data for the stacked bar plots or STUDENT's INPUT section
  plotly_bar_data <- reactive({
    
    example_data %>%
      dplyr::filter(exercise_name == input$task_name_sinput,
                    #variablenname == 'satzAntonym', 
                    stage == input$stage_sinput,
                    feldname == input$fieldname_sinput,
                    variablenname == input$variable_sinput
      ) %>%
      tidyr::unnest(var_value) %>%
      dplyr::filter(var_value %in% input$grouping_variable_sinput) %>%
      # has to be changed in the final version (probably)
      dplyr::mutate(right = dplyr::case_when(
        punkte == 100 ~ 'right',
        .default = 'false' 
      )) %>%
      dplyr::mutate(right = factor(right, levels = c('right', 'false'))) %>%
      dplyr::group_by(var_value) %>%
      dplyr::add_count(name = 'N') %>%
      dplyr::group_by(var_value, right, N) %>%
      dplyr::count(name = 'n_i') %>%
      dplyr::arrange(var_value, desc(right)) %>%
      dplyr::mutate(percent = n_i/N*100)
  })
  
  
  
  # Create Red line which measures the average number of True for all the selected grouping variables
  red_line_1 = reactive({
    
    exd_right <- plotly_bar_data()%>%
      dplyr::filter(right == "right")
    y=exd_right$percent
    mean(y)
  })
  
  
  
  
  
  
  
  
  # create the stacked bar plots or STUDENT's INPUT section
  output$plot_sinput <- renderPlotly({
    
    
    # Adjust length and implemend line brakes for X-axis labels
    new_x <- sapply(plotly_bar_data()$var_value, 
                    FUN = function(x) {
                      if (nchar(x) <= 50) {
                        return(x)
                      } else {
                        truncated_label <- substr(x, 1, 50)  # Truncate to 50 characters
                        truncated_label <- paste0(truncated_label, "...")  # Insert ... after 50th character and <br> after 25th character
                        return(truncated_label)
                      }
                    })
    new_x_1=sapply(new_x, FUN = function(x) {paste(strwrap(x, width = 25), collapse = "<br>")})
    
    
    
    
    # The plot_ly function
    plot_ly(x = new_x_1, y = plotly_bar_data()$percent,  
            color = plotly_bar_data()$right,
            colors = c('right' = '#008000', 'false' = '#FF0000'),text = ~paste("Grouping Variable :", new_x_1,
                                                                               "<br> No of students :",  plotly_bar_data()$n_i,
                                                                               "<br> Proportion of students :", round( plotly_bar_data()$percent/100,2)),
            textposition = "none",
            hoverinfo = "text") %>%
      add_bars() %>%
      layout(barmode = "stack",
             shapes = list(list(type = "line",line = list(color = "black"),
                                #x0 = -0.5, x1 = 12.5,
                                x0 = -0.5, x1 = length(input$grouping_variable_sinput)-0.5,
                                y0 = red_line_1(), y1 = red_line_1())),
             showlegend = FALSE,
             xaxis = list(title = ""),
             yaxis = list (title = "Prozent"))
    
    
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

    p = round(plotly_hist_data_versions()$n * 100 / sum(plotly_hist_data_versions()$n),2)
    result_df=data.frame("Versions" = plotly_hist_data_versions()$master_id, "Punkte" = plotly_hist_data_versions()$punkte, "Anzahl" = plotly_hist_data_versions()$n,  "Percentage"= paste(p, "%", sep=""))
    
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
  
  
  
  
  
  # Create the data table for the STUDENT'S INPUT section
  default_table_sinput = reactive({
    
    
    exd_right <- plotly_bar_data()%>%
      dplyr::filter(right == "right")
    x= exd_right$var_value
    y=exd_right$percent
    z = 100-y
    ni_right=exd_right$n_i
    ni_left=exd_right$N- ni_right
    data.frame("Grouping Variable"=x,"True"=paste(round(y,2), "%", sep = ""),"No_True"= ni_right,"False"=paste(round(z,2), "%", sep = ""),"No_False"=ni_left)
  })
  
  
  
  # Present the table for the STUDENT'S INPUT section
  output$table_output_sinput <- renderDT({
    
    req(default_table_sinput())
    
    datatable(default_table_sinput(), extensions = 'Buttons', options = list(
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
  
  # Create heading for stacked bar plots for STUDENT'S INPUT section
  output$plot_sinput_title <- renderUI({
    
    title_text <-  paste_fun(input$task_name_sinput,input$stage_sinput)   
    h4(title_text)  
  })
  
  # Create heading for data table for STUDENT'S INPUT ssection
  output$table_sinput_title <- renderUI({
    
    title_text <- paste_fun(input$task_name_sinput,input$stage_sinput)   # You can replace this with your dynamic text
    h4(title_text)  # You can adjust the HTML tag and class as needed
  })
  
  
  
  
} # End of server 







# Run the application
shinyApp(ui, server)




























