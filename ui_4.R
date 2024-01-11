source(here::here("packages.R"))

# load data 
load(here::here("example_data_shiny.RData"))

data <- example_data

data$variablenname <- ifelse(is.na(data$variablenname), "No variable found", data$variablenname)

ui <- dashboardPage(
  # Dashboard header
  dashboardHeader(title = "Shiny Dashboard", titleWidth = 230),
  
  # Dashboard Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overall", tabName = "Overall"),
      menuItem("Student's Input", tabName = "Sinput")
    )
  ), # end of dashboardSidebar
  
  dashboardBody( 
    tabItems(
      # TAB 1
      tabItem(
        tabName = "Overall",
        dashboardControlbar(
          # Dropdown menu for input 1
          selectizeInput("task_name_overall", "Task Name", choices = unique(data$exercise_name)),
          
          # Dropdown menu for input 2
          selectizeInput("stage_overall", "Stage", choices = NULL)
        ), # end of dashboard controlbar
        box(
          title = "Histogram of Punkte",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,  # Set the width (out of 12 columns)
          height = 200, # Set the height in pixels
          #offset = 500, 
          # Overall punkte plot
          plotlyOutput("plot_overall") 
          # Add more UI elements as needed
        )#End of Box
      ), # end of tab 1
      
      # TAB 2
      tabItem(
        tabName = "Sinput",
        dashboardControlbar(
          # Dropdown menu for input 1
          selectizeInput("task_name_sinput", "Task Name", choices = unique(data$exercise_name)),
          
          # Dropdown menu for input 2
          selectizeInput("stage_sinput", "Stage", choices = NULL),
          
          # Dropdown menu for input 3
          selectizeInput("fieldname_sinput", "Field Name", choices = NULL),
          
          #Dropdown menu for input 4
          checkboxGroupInput("grouping_variable_sinput", "Grouping Variable", choices = NULL)
          
        ), # end of dashboard controlbar
        box(
          title = "Barplots",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,  # Set the width (out of 12 columns)
          height = 200, # Set the height in pixels
          #offset = 500, 
          # Overall punkte plot
          plotlyOutput("plot_sinput") 
          # Add more UI elements as needed
        )#End of Box
      ) # end of tab 2
    ) # end of tabItems
  ) # End of dashboard body
) # End of dashboard page

server <- function(input, output, session) {
  
  
  
  # Update choices for input 2 based on input 1
  observeEvent(input$task_name_overall, {
    # Perform your logic to generate choices based on input 1
    choices_1 = reactive({as.character(sort(unique(data[data$exercise_name == input$task_name_overall, 3])))}) 
    
    # Update choices for input 2
    updateSelectizeInput(session, "stage_overall", choices = choices_1())
  })
  
  # Update choices for input 2 based on input 1
  observeEvent(input$task_name_sinput, {
    # Perform your logic to generate choices based on input 1
    choices_1 = reactive({as.character(unique(data[data$exercise_name == input$task_name_sinput, 3]))}) 
    
    # Update choices for input 2
    updateSelectizeInput(session, "stage_sinput", choices = choices_1())
  })
  
  # Update choices for input 4 based on input 1 and input 2 and input 3
  observeEvent(c(input$task_name_sinput, input$stage_sinput), {
    choices_fourth_input <- reactive({ unique(data[data$exercise_name == input$task_name_sinput & as.character(data$stage)== input$stage_sinput , 4])}) 
    
    # Update choices for input 2
    updateSelectizeInput(session, "fieldname_sinput", choices = choices_fourth_input())
  })
  
  # Update choices for input 4 based on input 1 and input 2 and input 3
  observeEvent(c(input$task_name_sinput, input$stage_sinput, input$fieldname_sinput), {
    choices_grouping_variable <- reactive({
      # Assuming data$variablenname is a vector
      
      unlist(unique(subset(example_data, 
                           exercise_name == input$task_name_sinput & 
                             stage ==  input$stage_sinput & 
                             feldname == input$fieldname_sinput)$var_value))
    })
    # Update choices for input 2
    updateCheckboxGroupInput(session, "grouping_variable_sinput", choices =  choices_grouping_variable(), selected = choices_grouping_variable())
    
  })
  
  
  
  
  
  
  # Create the plotly Histogram from the OVERALL tab..
  plotly_hist_data <- reactive({
    example_data %>%
      dplyr::filter(exercise_name == input$task_name_overall,
                    stage == input$stage_overall) 
  })
  
  output$plot_overall <- renderPlotly({
    # a simple histogram of movie ratings
    hist_data <- hist(plotly_hist_data()$punkte, breaks = seq(0, 100, by = 1), plot = FALSE)
    
    #plotly_data %>%
  #  plot_ly(x = plotly_hist_data()$punkte, type = "bar")
    # Create a bar plot using plot_ly
    plot_ly(x = hist_data$mids, y = hist_data$counts, type = "bar") %>%
      layout(
        xaxis = list(title = "Punkte",
                     tickvals = seq(0, 100, by = 10),  # Set tick values
                     tickmode = "array"),  # Set tick mode to "array"),
        yaxis = list(title = "Anzahl")
      )
    
    
  })
  
  
  
  
  
  
  
  # Create plotly stacked bar plots for the STUDENT#S INPUT menu
  plotly_bar_data <- reactive({
    example_data %>%
      dplyr::filter(exercise_name == input$task_name_sinput,
                    #variablenname == 'satzAntonym', 
                    stage == input$stage_sinput,
                    feldname == input$fieldname_sinput
      ) %>%
      tidyr::unnest(var_value) %>%
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
  
  # red line 
  red_line <- reactive({
    example_data %>%
      dplyr::filter(exercise_name == input$task_name_sinput,
                    #variablenname == 'satzAntonym', 
                    stage == input$stage_sinput,
                    feldname == input$fieldname_sinput
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
  })
  
  output$plot_sinput <- renderPlotly({
    
    # a simple histogram of movie ratings
    
    #plotly_data %>%
    plot_ly(x = plotly_bar_data()$var_value, y = plotly_bar_data()$percent,  
            color = plotly_bar_data()$right,
            colors = c('right' = '#008000', 'false' = '#FF0000')) %>%
      add_bars() %>%
      layout(barmode = "stack",
             shapes = list(list(type = "line",line = list(color = "black"),
                                #x0 = -0.5, x1 = 12.5,
                                x0 = -0.5, x1 = max(plotly_bar_data()$var_value),
                                y0 = red_line(), y1 = red_line())),
             showlegend = FALSE,
             xaxis = list(title = ""),
             yaxis = list (title = "Prozent"))
    
    #    p <- plot_ly(example_data, x = ~punkte, autobinx = F, type = "histogram",
    #                 xbins = list(start = minx, end = maxx, size = 10))
    #    # style the xaxis
    #    layout(p, xaxis = list(title = "Points", range = c(minx, maxx), autorange = F,
    #                           autotick = F, tick0 = minx, dtick = 10))
    
  })
  
  
} 

# Run the application
shinyApp(ui, server)
