source(here::here("packages.R"))

# load data 
load(here::here("example_data_shiny.RData"))

data <- example_data

# data$variablenname <- ifelse(is.na(data$variablenname), "No variable found", data$variablenname)

A = c("Nothing", "Var_values") # What is this? Choice should be nothing, Versions or var_value

minx <- min(example_data$punkte)
maxx <- max(example_data$punkte)



# Idea 
'https://stackoverflow.com/questions/33941232/integrate-plotly-with-shinydashboard'

################################################################################
###################################### UI ######################################
################################################################################
ui <- dashboardPage(
  
  ############# Header
  dashboardHeader(title = "Test"),
  
  ############# Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      # Dropdown menu for input 1
      selectizeInput("task_name", "Task", choices = c('',sort(unique(data$exercise_name))))
    )
  ),
  
  
  ############# Dashboard body
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(
        title = "Distribution of points based on grouping variables variables ",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,  # Set the width (out of 12 columns)
        height = 500, # Set the height in pixels
        offset = 500,
        plotlyOutput("plot1") 
    )
  )
)
)



################################################################################
#################################### Server ####################################
################################################################################

server <- function(input, output, session) {
  
  # subset dataset
  plotly_data <- reactive({
    example_data %>%
    dplyr::filter(exercise_name == input$task_name,
                  variablenname == 'satzAntonym', 
                  stage == 1,
                  feldname == 'dropdown1'
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
    dplyr::filter(exercise_name == input$task_name,
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
  })
  
  
  output$plot1 <- renderPlotly({
    # a simple histogram of movie ratings
    
    #plotly_data %>%
      plot_ly(x = plotly_data()$var_value, y = plotly_data()$percent,  
              color = plotly_data()$right,
              colors = c('right' = '#008000', 'false' = '#FF0000')) %>%
      add_bars() %>%
      layout(barmode = "stack",
             shapes = list(list(type = "line",line = list(color = "black"),
                                x0 = -0.5, x1 = 12.5,
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

################################################################################
################################### Run App ####################################
################################################################################
shinyApp(ui, server)