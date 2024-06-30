######################################################
#####           preamble and functions           #####
######################################################
source("packages.R")
source("functions.R")

# load and modify the data
load("data_overall.RData")
load("data_sinput.RData")
# user base
load("user_base.RData")

# Convert NULL to NA in data_sinput
data_sinput <- data_sinput %>%
  dplyr::mutate(var_value = map(var_value, ~ if(is.null(.)) NA else .))

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
                   by = 'color_value') %>%
  dplyr::mutate(points_individual = as.factor(points_individual))

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
  
  ### ---------- Login page ###
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
  
  ###----- Dashboard UI -----###
  uiOutput("dashboard_ui"),
  
  ###----- footer -----###
  div( #class = "pull-top", 
    #p(class = 'own_h' ,' ')
    includeHTML("footer.html")
  )
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
  
  # Logout Button 
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  ######################################################
  #####     Dashboard UI after successful login    #####
  ######################################################
  
  output$dashboard_ui <- renderUI({
    req(credentials()$user_auth)
    
    
    dashboardPage(
      title = 'Analyseboard für Deutsch als Zweit- und Fremdsprache',
      
      # Dashboard header
      dashboardHeader(title = 'DaZ / DaF', titleWidth = 230),
      # Dashboard Sidebar  (left)
      dashboardSidebar(
        sidebarMenu(
          menuItem("Aufgabe", tabName = "Overall"),
          menuItem("Antwortanalyse", tabName = "Sinput")
        )
      ),
      # dashboard Body
      dashboardBody(
        # Create left and Right margins or the Dashboard Controlbar
        includeCSS("format.css"),
        tabItems(
          tabItem(
            tabName = "Overall",
            # Selection input for Overall
            dashboardControlbar(
              width = 250,  # Adjust the width as needed
              # Dropdown menu for Semester
              checkboxGroupInput("semester", "Semester", choices = unique(data_overall$Semester), selected = unique(data_overall$Semester) ),
              #Checkbox menu for Module
              checkboxGroupInput("module_overall", "Module", choices = sort(unique(data_overall$modulcode)), selected = sort(unique(data_overall$modulcode))),
              # Dropdown menu for Task Name
              selectizeInput("task_name_overall", "Aufgabe", choices = c(' ', sort(unique(data_overall$exercise_name)))),
              # Dropdown menu for Stage
              selectizeInput("stage_overall", "Aufgabenteil", choices = NULL),
              # Radio button for versions
              radioButtons("version_button", "Nach Varianten differenzieren?", choices = c("Yes","No"), selected = "No", inline = TRUE),
              
              conditionalPanel(
                condition = "input.version_button == 'Yes'",
                checkboxGroupInput("master_id_overall", "Master ID", choices = NULL)
              )
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
              # Dropdown menu for Semester
              checkboxGroupInput("semester_sinput", "Semester", choices =unique(data_sinput$Semester), selected = unique(data_sinput$Semester)),
              #Checkbox menu for Module
              checkboxGroupInput("module_sinput", "Module", choices = sort(unique(data_sinput$modulcode)), selected = sort(unique(data_sinput$modulcode))),
              # Dropdown menu for Task Name
              selectizeInput("task_name_sinput", "Aufgabe", choices = c(' ', sort(unique(data_sinput$exercise_name)))),
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
  
  ######################################################
  #####               Update choices               #####
  ######################################################
  
  # exercise choice 
  choices_1 = reactive({c(
    as.character(
      unlist(
        unique(data_overall[data_overall$modulcode %in%  input$module_overall & 
                              data_overall$Semester %in% input$semester , 'exercise_name'])
      )
    ))})
  
  # exercise choice # do we need both?  
  observeEvent(c(input$task_name_overall, input$semester), {
    
    choices_1 = reactive({c(
      as.integer(
        unlist(unique(
          data_overall[data_overall$exercise_name %in% input$task_name_overall & 
                         data_overall$Semester %in% input$semester, 'stage'])) # here false?
      ))}) 
    
    updateSelectizeInput(session, "stage_overall", choices = choices_1())
  })
  
  # Update choices 
  observeEvent(c(input$task_name_overall, input$stage_overall, input$semester), {
    
    choices_1 = reactive({unique(subset(data_overall, 
                                        Semester %in% input$semester &
                                          #modulcode == input$module_sinput &
                                          exercise_name == input$task_name_overall & 
                                          stage ==  input$stage_overall )$master_id)}) 
    
    updateCheckboxGroupInput(session, "master_id_overall", 
                             choices = extract_suffix(choices_1()), 
                             selected = extract_suffix(choices_1()))
  })
  
  # Update choices 
  observeEvent(c(input$module_sinput,input$task_name_sinput, input$semester_sinput), {
    
    choices_1 = reactive({c(as.integer(unlist(unique(data_sinput[data_sinput$modulcode %in% input$module_sinput & data_sinput$exercise_name ==  input$task_name_sinput & data_sinput$Semester %in% input$semester_sinput, 'stage']))))}) 
    
    updateSelectizeInput(session, "stage_sinput", choices = choices_1())
  })
  
  
  # Define reactive expression to filter data based on input choices
  # change filtered to exercise_type
  stage_type <- reactive({
    req(input$module_sinput, input$task_name_sinput, input$stage_sinput, input$semester_sinput)
    
    # getting task type 
    # Filter data based on module_sinput, task_name_sinput, and stage_sinput
    type_stage <- c(as.character(unlist(unique(data_sinput[data_sinput$modulcode %in% input$module_sinput &
                                                             data_sinput$Semester %in% input$semester_sinput &
                                                             data_sinput$exercise_name == input$task_name_sinput &
                                                             data_sinput$stage == input$stage_sinput, 'type']))))[1]
    return(type_stage)
  })
  
  # Inputs for MC type questions - Master ID
  observeEvent(c(input$module_sinput,input$task_name_sinput,input$stage_sinput), {
    
    #choices_1 = reactive({c(unique(data_overall[data_overall$exercise_name == input$task_name_overall, 4]))}) 
    choices_1 = reactive({unique(subset(data_sinput,
                                        !is.na(master_id) &
                                          modulcode %in% input$module_sinput &
                                          exercise_name == input$task_name_sinput & 
                                          stage ==  input$stage_sinput )$master_id)}) 
    
    updateCheckboxGroupInput(session, "mc_options", choices = sort(extract_suffix(choices_1())), selected = extract_suffix(choices_1()))
  })
  
  # Inputs for Dropdown type questions - Fieldname
  observeEvent(c(input$module_sinput,input$task_name_sinput,input$stage_sinput), {
    
    #choices_1 = reactive({c(unique(data_overall[data_overall$exercise_name == input$task_name_overall, 4]))}) 
    choices_1 = reactive({sort(unique(subset(data_sinput, 
                                             modulcode %in% input$module_sinput &
                                               exercise_name == input$task_name_sinput & 
                                               stage ==  input$stage_sinput )$feldname))}) 
    
    updateSelectizeInput(session, "fieldname_sinput_1", choices = choices_1())
  })
  
  
  # Inputs for Dropdown type questions - Master ID
  observeEvent(c(input$module_sinput,input$task_name_sinput,input$stage_sinput), {
    
    choices_1 = reactive({data_overall %>%
        filter(!is.na(master_id),
               modulcode %in% input$module_sinput &
                 exercise_name %in% input$task_name_sinput & 
                 stage %in% input$stage_sinput) %>% 
        pull(master_id) %>%
        unique() %>%
        sort()})          
    
    updateCheckboxGroupInput(session, "master_id_dropdown_sinput", choices = sort(extract_suffix(choices_1())), selected = extract_suffix(choices_1()))
  })
  
  # Inputs for Fill-In type questions - Master ID
  observeEvent(c(input$module_sinput,input$task_name_sinput,input$stage_sinput), {
    choices_1 = reactive({unique(subset(data_sinput,
                                        !is.na(master_id) &
                                          modulcode %in% input$module_sinput &
                                          exercise_name %in% input$task_name_sinput & 
                                          stage %in% input$stage_sinput )$master_id)}) 
    
    updateCheckboxGroupInput(session, "fillin_options", choices = sort(extract_suffix(choices_1())), selected = extract_suffix(choices_1()))
  })
  
  ######################################################
  #####              Overall data tab              #####
  ######################################################
  
  # main data set for Overall tab
  overall_data <- reactive({
    data_overall %>%
      dplyr::filter(exercise_name == input$task_name_overall,
                    stage == input$stage_overall,
                    modulcode %in% input$module_overall,
                    Semester %in% input$semester)
  })
  
  
  
  # Create the histogram for the OVERALL tab when versions are not selected
  output$plot_overall <- renderPlotly({
    
    
    xx=data.frame(table(overall_data()$punkte))
    
    
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
    
    overall_data() %>%
      dplyr::filter(extract_suffix(master_id) %in% input$master_id_overall ) %>%
      mutate(master_id = extract_suffix(master_id)) %>%
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
  
  # Create the data table for the OVERALL section when version not selected
  default_table_overall = reactive({
    
    xx=data.frame(table(overall_data()$punkte))
    x = xx$Var1
    y = xx$Freq
    summ= sum(y)
    z = round(y/summ,2)
    p = z*100
    result_df = data.frame("Punkte" = x, 
                           "Anzahl" = y,
                           "Percentage"= p)
  })
  
  # Present the table for the OVERALL section when version not selected
  output$table_output_overall <- renderDT({
    req(default_table_overall())
  },
  extensions = 'Buttons',
  options = download_button(input$task_name_overall, input$stage_overall),
  class = "display"
  )
  
  
  # Create the data table for the OVERALL section when version selected
  default_table_overall_versions = reactive({
    
    result_df = data.frame("Versions" = plotly_hist_data_versions()$master_id, 
                           "Punkte" = plotly_hist_data_versions()$punkte, 
                           "Anzahl" = plotly_hist_data_versions()$n,  
                           "Percentage"= plotly_hist_data_versions()$percentage)
  })
  
  # Present the table for the OVERALL section when version selected
  output$table_output_overall_versions <- renderDT({
    
    req(default_table_overall_versions())
  },extensions = 'Buttons',
  options =  download_button(input$task_name_overall, input$stage_overall, 
                             options = '_versions'),
  class = "display"
  )
  
  ######################################################
  #####             Antwortanalyse tab             #####
  ######################################################
  # main data set for Analyse tab
  antwortanalyse_data <- reactive({
    data_sinput %>%
      dplyr::filter(Semester %in% input$semester_sinput,
                    exercise_name == input$task_name_sinput,
                    stage == input$stage_sinput,
                    modulcode %in% input$module_sinput)
  })
  
  # Antwortanalyse Data MC TYPE  
  # MC_data
  antwortanalyse_data_MC <- reactive({
    
    antwortanalyse_data() %>%
      dplyr::filter(!is.na(master_id)) %>%
      dplyr::filter(!is.na(feldinhalt)) %>%
      dplyr::filter(extract_suffix(master_id) %in% input$mc_options) %>%
      dplyr::mutate(master_id = extract_suffix(master_id))%>%
      dplyr::group_by(master_id, feldinhalt) %>%
      dplyr::add_count(name = 'N') %>%
      dplyr::group_by(master_id, feldinhalt, points_individual, N, colour) %>%
      dplyr::count(name = 'n_i') %>%
      dplyr::mutate(percent = round(n_i/N*100, 2)) %>%
      dplyr::mutate(feldinhalt_trimmed = truncate_and_wrap(feldinhalt))
  })
  
  # Antwortanalyse Data MC TYPE
  output$plot_sinput_mc <- renderPlotly({
    
    wrap_plot(antwortanalyse_data_MC(), x = 'feldinhalt', type = 'Multiple-Choice')
  })
  
  # Antwortanalyse Data Dropdown TYPE  
  # Dropdown_data
  antwortanalyse_data_dropdown <- reactive({
    antwortanalyse_data() %>%
      dplyr::filter(feldname == input$fieldname_sinput_1) %>%
      dplyr::filter(!is.na(master_id)) %>%
      dplyr::filter(!is.na(points_individual)) %>%    
      dplyr::filter(extract_suffix(master_id) %in% input$master_id_dropdown_sinput) %>%
      dplyr::mutate(master_id = extract_suffix(master_id)) %>%
      dplyr::mutate(var_value = map_chr(var_value, ~ as.array(.x))) %>%
      dplyr::group_by(master_id,var_value) %>%
      dplyr::add_count(name = 'N') %>%
      dplyr::group_by(master_id, var_value, points_individual, N, colour) %>% # changed right
      dplyr::count(name = 'n_i') %>%
      dplyr::mutate(percent = round(n_i/N*100,2)) %>%
      dplyr::mutate(var_value_trimmed = truncate_and_wrap(var_value))
    
  })
  
  
  # Antwortanalyse Data Dropdown TYPE
  output$plot_sinput_dropdown <- renderPlotly({
    
    wrap_plot(antwortanalyse_data_dropdown(), x = 'var_value', type = 'Dropdown')
  })
  
  
  
  
  # Create data for the STUDENT's INPUT section for Fill-In type questions
  # antwortanalyse_data_fillin
  antwortanalyse_data_fillin <- reactive({
    antwortanalyse_data() %>%
      dplyr::filter(!is.na(master_id)) %>%
      dplyr::filter(!is.na(points_individual)) %>%
      dplyr::filter(extract_suffix(master_id) %in% input$fillin_options) %>%
      dplyr::mutate(master_id = extract_suffix(master_id)) %>%
      dplyr::group_by(master_id,feldname) %>%
      dplyr::add_count(name = 'N') %>%
      dplyr::group_by(master_id, feldname, points_individual, N, colour) %>% # changed right
      dplyr::count(name = 'n_i') %>%
      dplyr::mutate(percent = round(n_i/N*100,2)) %>%
      dplyr::mutate(feldname_trimmed = feldname)
  })
  
  
  # create the stacked bar plots or STUDENT's INPUT section for Fill-In type
  output$plot_sinput_fillin <- renderPlotly({
    
    wrap_plot(antwortanalyse_data_fillin(), x = 'feldname', type = 'Fill-In')
  })
  
  
  # Create the data table for the STUDENT'S INPUT section for MC Type
  table_mc_sinput_tab = reactive({
    
    antwortanalyse_data_MC() %>% 
      ungroup() %>%
      dplyr::rename('%' = percent) %>%
      dplyr::rename("Result (in%)" = points_individual) %>%
      dplyr::rename("n" = n_i) %>%
      dplyr::select(-c(colour, feldinhalt_trimmed)) %>%
      dplyr::select(master_id, feldinhalt, `Result (in%)`,n, `%`, N) %>%
      dplyr::filter(!is.na(feldinhalt)) %>%
      dplyr::arrange(master_id, feldinhalt, `Result (in%)`)
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
    
    if (!all(is.na( antwortanalyse_data_dropdown()$var_value))){
    
    antwortanalyse_data_dropdown() %>% 
      ungroup() %>%
      dplyr::rename('%' = percent) %>%
      dplyr::rename("Result (in%)" = points_individual) %>%
      dplyr::rename("n" = n_i) %>%
      dplyr::select(-c(colour,var_value_trimmed)) %>%
      dplyr::mutate(var_value = unlist(var_value)) %>%
      dplyr::select(master_id, var_value, `Result (in%)`, n, `%`, N) %>%
      dplyr::filter(!is.na(var_value)) %>%
      dplyr::arrange(master_id,var_value,`Result (in%)`)
    } else {
      
      antwortanalyse_data_dropdown() %>% 
        ungroup() %>%
        dplyr::rename('%' = percent) %>%
        dplyr::rename("Result (in%)" = points_individual) %>%
        dplyr::rename("n" = n_i) %>%
        dplyr::select(-c(colour,var_value_trimmed,var_value)) %>%
        dplyr::select(master_id, `Result (in%)`, n, `%`, N) %>%
        dplyr::arrange(master_id,`Result (in%)`)
      
    }
      
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
  
  
  # Create the data table for the STUDENT'S INPUT section for Fillin Type
  table_fillin_sinput_tab = reactive({
    antwortanalyse_data_fillin()%>% 
      ungroup() %>%
      dplyr::rename('%' = percent) %>%
      dplyr::rename("Result (in%)" = points_individual) %>%
      dplyr::rename("n" = n_i) %>%
      dplyr::select(-c(colour)) %>%
      dplyr::select(master_id, feldname, `Result (in%)`, n, `%`, N) %>%
      dplyr::arrange(master_id,feldname,`Result (in%)`)
  })
  
  # Present the table for the STUDENT'S INPUT section for Fillin Type
  output$table_fillin_sinput <- renderDT({
    
    req(table_fillin_sinput_tab())
    
    datatable(table_fillin_sinput_tab(), extensions = 'Buttons', options = list(
      paging = FALSE,
      dom = 'Bfrtip',
      buttons = list( 
        list(extend = 'csv', filename = paste(input$task_name_sinput, " - Stage:", input$stage_sinput, 'grouping'), title = paste_fun(input$task_name_sinput,input$stage_sinput)),
        list(extend = 'excel', filename = paste(input$task_name_sinput, " - Stage:", input$stage_sinput, 'grouping'), title = paste_fun(input$task_name_sinput,input$stage_sinput)),
        list(extend = 'copy')
      )
    ))
  }, class = "display")
  
  ##### Conditional Displays for Antwortanalyse tab #####
  
  # Conditional Input Menu
  output$conditional_input <- renderUI({
    
    # Conditional logic to display different input based on the value returned by filtered_data
    if (stage_type() == "Multiple-Choice") {
      # Render input for MC type
      checkboxGroupInput("mc_options", label = HTML("Multiple Choice Questions <br> Master ID"), choices = NULL)
    } else if (stage_type() == "Dropdown") {
      # Render dropdown menu for dropdown type
      tagList(
        selectizeInput("fieldname_sinput_1", label = HTML("Dropdown Questions <br> Field Name"), choices = NULL),
        checkboxGroupInput("master_id_dropdown_sinput", label = "Master ID", choices = NULL)
      )#
      
    } else if (stage_type() == "Fill-In") {
      # Render input for fill-in type
      checkboxGroupInput("fillin_options", label = HTML("Fill-In Questions <br> Master ID"), choices = NULL)
    } else {
      # Render default input if type is not recognized:
      textOutput("invalid_type_message", "Invalid Type")
    }
  })
  
  # Conditional Output
  output$conditional_plot_panel <- renderUI({
    if (stage_type() == "Multiple-Choice") {
      plotlyOutput("plot_sinput_mc")
    } else if (stage_type() == "Dropdown") {
      plotlyOutput("plot_sinput_dropdown")
    } else if (stage_type() == "Fill-In") {
      plotlyOutput("plot_sinput_fillin")  
    } else {
      # Default output if type is not recognized
      plotOutput("INVALID CHOICE")
    }
  })
  
  output$conditional_table_panel <- renderUI({
    if (stage_type() == "Multiple-Choice") {
      DTOutput('table_mc_sinput')
    } else if (stage_type() == "Dropdown") {
      DTOutput('table_dropdown_sinput')
    } else if (stage_type() == "Fill-In"){
      DTOutput("table_fillin_sinput")
    } else {
      # Default output if type is not recognized
      plotOutput("invalid_type_message")
    }
  })
  
  
  # Titles for the 4 tabs
  # Overall panel
  # Create heading for histogram for OVERALL section - versions not selected
  output$plot_overall_title <- renderUI({
    h4(title_fun(default_table_overall(), input$task_name_overall, input$stage_overall, T, T))
  })
  
  # Create heading for data table for OVERALL section - versions not selected
  output$table_overall_title <- renderUI({
    h4(title_fun(default_table_overall(), input$task_name_overall, input$stage_overall, F, F))  
  })
  
  # Create heading for histogram for OVERALL section - versions  selected
  output$plot_overall_title_version <- renderUI({
    h4(title_fun(default_table_overall_versions(), input$task_name_overall, input$stage_overall, T, T))  
  })
  
  # Create heading for data table for OVERALL section - versions  selected
  output$table_overall_title_version <- renderUI({
    h4(title_fun(default_table_overall_versions(), input$task_name_overall, input$stage_overall, F, F))  
  })
  
  # Antwortanalyse
  # Create heading for Plots for SINPUT
  output$plot_sinput_title <- renderUI({
    #    title_text  = paste_fun(input$task_name_sinput,input$stage_sinput) 
    h4(title_fun(antwortanalyse_data(), input$task_name_sinput, input$stage_sinput, F, T))  
  })
  
  # Create heading for table for SINPUT
  output$table_sinput_title <- renderUI({
    #    title_text  = paste_fun(input$task_name_sinput,input$stage_sinput) 
    h4(title_fun(antwortanalyse_data(), input$task_name_sinput, input$stage_sinput, F, F))  
  })
  
} # End of server 

######################################################
#####                   App                      #####
######################################################
shinyApp(ui, server)


