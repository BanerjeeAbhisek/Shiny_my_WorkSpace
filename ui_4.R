source(here::here("packages.R"))

# load data 
load(here::here("example_data_shiny.RData"))

data <- example_data

data$variablenname <- ifelse(is.na(data$variablenname), "No variable found", data$variablenname)


#A = c("Nothing", "Var_values") # What is this? Choice should be nothing, Versions or var_value






ui <- dashboardPage(
  
  
  
  #Dashboard header
  dashboardHeader(title = "Shiny Dashboard", titleWidth = 230),
  
  
  
  #Dashboard Sidebar
  dashboardSidebar(
   
    # Dropdown menu for input 1
    selectizeInput("task_name", "Task Name", choices = unique(data$exercise_name)),
    
    # Dropdown menu for input 2
    selectizeInput("stage", "Stage", choices = NULL)
    
    
    
  ),
  dashboardBody( 
 
      tabItems(
        # TAB 1
        tabItem(
          tabName = "tab1",
          fluidRow(
            column(width = 4,
                   box(
                     title = "Selection Option",
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     width = 12,
                     height = 200,
                     offset = 500, 
                     selectizeInput("fieldname", "Field Name", choices = NULL)
                   )#end of box
            )# end of column
          )# end of fluid row
        ),# end of tab 1
        
        # TAB 2
        tabItem(
          tabName = "tab1",
          fluidRow(
            column(width = 4,
                   box(
                     title = "Selection Option",
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     width = 12,
                     height = 200,
                     offset = 500, 
                     selectizeInput("fieldname", "Field Name", choices = NULL)
                   ),
                   box(
                     title = "Grouping Variables",
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     width = 12,
                     height = "auto",
                     offset = 500, 
                     checkboxGroupInput("grouping_variable", "Select Grouping Variable", choices = NULL)
                   )#end of box
            )# end of column
          )# end of fluid row
        )# end of tab 2
      )#end of tab itemS
  
    
  ) # End of dashboard body
  
) # End of dashboard page





server <- function(input, output, session) {
  
  
  
  
  
  
  
  
  # Update choices for input 2 based on input 1
  observeEvent(input$task_name, {
    # Perform your logic to generate choices based on input 1
    choices_1 = reactive({as.character(unique(data[data$exercise_name==input$task_name,3]))}) 
    
    # Update choices for input 2
    updateSelectizeInput(session, "stage", choices = choices_1())
  })
  
  
  
  
  # Update choices for input 4 based on input 1 and input 2 and input 3
  observeEvent(c(input$task_name, input$stage), {
    choices_fourth_input <- reactive({ unique(data[data$exercise_name == input$task_name & as.character(data$stage)== input$stage , 4])}) 
    
    # Update choices for input 2
    updateSelectizeInput(session, "fieldname", choices = choices_fourth_input())
  })
  
  
  
  
  
  # Update grouping variable
  #Update choices for input 4 based on input 1 and input 2 and input 3
  observeEvent(c(input$task_name, input$stage, input$fieldname), {
    choices_grouping_variable <- reactive({
      # Assuming data$variablenname is a vector
      
      unlist(unique(subset(example_data, 
                           exercise_name == input$task_name & 
                             stage ==  input$stage & 
                             feldname == input$fieldname)$var_value))
    })
    # Update choices for input 2
    updateCheckboxGroupInput(session, "grouping_variable", choices =  choices_grouping_variable(), selected = choices_grouping_variable())
    
    
    
  })
  
  
  
  
  
  
  
  
}


# Run the application
shinyApp(ui, server)




















