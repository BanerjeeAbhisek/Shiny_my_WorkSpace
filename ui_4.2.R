ui <- dashboardPage(
  
  
  
  #Dashboard header
  dashboardHeader(title = "Shiny Dashboard", titleWidth = 230),
  
  
  
  #Dashboard Sidebar
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Overall", tabName = "Overall"),
      menuItem("Student's Input", tabName = "Sinput")
    )# end of sidebarMenu
    
    
  ), # end of dashboardSidebar
  dashboardBody( 
    
    tabItems(
      # TAB 1
      tabItem(
        tabName = "Overall",
        dashboardControlbar(
          # Dropdown menu for input 1
          selectizeInput("task_name", "Task Name", choices = unique(data$exercise_name)),
          
          # Dropdown menu for input 2
          selectizeInput("stage", "Stage", choices = NULL)
          
        ),# end of dashboard controlbar
        
        observeEvent(input$task_name, {
          # Perform your logic to generate choices based on input 1
          choices_1 = reactive({as.character(unique(data[data$exercise_name==input$task_name,3]))}) 
          
          # Update choices for input 2
          updateSelectizeInput(session, "stage", choices = choices_1())
        })
        
        
        
        
      ),# end of tab 1
      
      # TAB 2
      tabItem(
        tabName = "Sinput",
        dashboardControlbar(
          # Dropdown menu for input 1
          selectizeInput("task_name", "Task Name", choices = unique(data$exercise_name)),
          
          # Dropdown menu for input 2
          selectizeInput("stage", "Stage", choices = NULL),
          
          #Dropdown menu for input 3
          selectizeInput("fieldname", "Field Name", choices = NULL)
          
          
        ),# end of dashboard controlbar
        observeEvent(input$task_name, {
          # Perform your logic to generate choices based on input 1
          choices_1 = reactive({as.character(unique(data[data$exercise_name==input$task_name,3]))}) 
          
          # Update choices for input 2
          updateSelectizeInput(session, "stage", choices = choices_1())
        }),
        # Update choices for input 4 based on input 1 and input 2 and input 3
        observeEvent(c(input$task_name, input$stage), {
          choices_fourth_input <- reactive({ unique(data[data$exercise_name == input$task_name & as.character(data$stage)== input$stage , 4])}) 
          
          # Update choices for input 2
          updateSelectizeInput(session, "fieldname", choices = choices_fourth_input())
        })
        
        
        
      )# end of tab 2
    )#end of tab itemS
    
    
  ) # End of dashboard body
  
) # End of dashboard page










server <- function(input, output, session) {
  
  
  
  
  
  
  
}


# Run the application
shinyApp(ui, server)













