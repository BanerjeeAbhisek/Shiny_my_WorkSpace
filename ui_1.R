source(here::here("packages.R"))

data <- example_data

data$variablenname <- ifelse(is.na(data$variablenname), "No variable found", data$variablenname)


examp_1111 <- subset(example_data, 
                     exercise_name == '04 Semantik' & 
                       stage == 1 & 
                       feldname == 'dropdown1' & 
                       eingereichte_antwort == 'Antonym' & 
                       variablenname == 'satzAntonym')

B <- unique(as.array(unlist(examp_1111$var_value)))





ui <- dashboardPage(
  
  #Dashboard header
  dashboardHeader(title = "Shiny Dashboard", titleWidth = 230),
  
  
  
  #Dashboard Sidebar
  dashboardSidebar(
    tags$style(".main-sidebar {width: 230px;}"),
    # Dropdown menu for input 1
    selectizeInput("task_name", "Task Name", choices = unique(data$exercise_name)),
    
    # Dropdown menu for input 2
    selectizeInput("stage", "Stage", choices = NULL),
    
    # Dropdown menu for input 3
    selectizeInput("version", "Version", choices = NULL),
    
    # Dropdown menu for input 4
    selectizeInput("fieldname", "Field Name", choices = NULL),
    
    # Dropdown menu for input 5
    selectizeInput("variable", "Variable", choices = NULL),
    
    #Action Button
    actionButton("update_button", "Update Plot")
    
    
  ),
  dashboardBody( 
    fluidRow(
      
      column(width = 8,
             
             box(
               title = "Distribution of total points",
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,  # Set the width (out of 12 columns)
               height = 500, # Set the height in pixels
               offset = 500, 
               plotOutput("my_plot_output")  # Plot will be rendered here
             ),
             
             
             box(
               title = "Summary statistics",
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,  # Set the width (out of 12 columns)
               height = 300, # Set the height in pixels
               offset = 500, 
               DTOutput('tbl')  # Summary statistics will be rendered here
               
               
             ) # end of box
             
             
             
      ), # End of column
      
      
      column(width = 4,
             box(
               title = "Grouping Variables",
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,  # Set the width (out of 12 columns)
               height = 700, # Set the height in pixels
               offset = 500, 
               # Dropdown menu with options from array B
               selectInput("grouping_variable", "Select Grouping Variable", choices = B),
               # Add more UI elements as needed
             )#End of Box
             
             
      )# End of column
      
      
      
      
      
    ) # End of fluid row
    
  ) # End of dashboard slidebar
  
) # End of dashboard page





server <- function(input, output, session) {
  
  
  
  
  
  
  
  
  # Update choices for input 2 based on input 1
  observeEvent(input$task_name, {
    # Perform your logic to generate choices based on input 1
    choices_1 = reactive({as.character(unique(data[data$exercise_name==input$task_name,3]))}) 
    
    # Update choices for input 2
    updateSelectizeInput(session, "stage", choices = choices_1())
  })
  
  
  # Update choices for input 3 based on input 1 and input 2
  observeEvent(c(input$task_name, input$stage), {
    choices_third_input <- reactive({as.character( unique(data[data$exercise_name == input$task_name & as.character(data$stage)== input$stage, 3]))}) 
    
    # Update choices for input 2
    updateSelectizeInput(session, "version", choices = choices_third_input())
  })
  
  # Update choices for input 4 based on input 1 and input 2 and input 3
  observeEvent(c(input$task_name, input$stage, input$version), {
    choices_fourth_input <- reactive({ unique(data[data$exercise_name == input$task_name & as.character(data$stage)== input$stage & as.character(data$stage)== input$version, 4])}) 
    
    # Update choices for input 2
    updateSelectizeInput(session, "fieldname", choices = choices_fourth_input())
  })
  
  
  # Update choices for input 4 based on input 1 and input 2 and input 3
  observeEvent(c(input$task_name, input$stage, input$version, input$fieldname), {
    choices_fifth_input <- reactive({
      # Assuming data$variablenname is a vector
      
      unique(data[data$exercise_name == input$task_name & as.character(data$stage)== input$stage & as.character(data$stage)== input$version & data$feldname== input$fieldname, 7])}) 
    
    # Update choices for input 2
    updateSelectizeInput(session, "variable", choices = choices_fifth_input())
  })
  
  
  
  
  # Define output text based on inputs
  iris_plot <- eventReactive(input$update_button, {
    
#    hist(data$punkte,breaks = 5,xlab = "Punkte",ylab = "Proportion",freq = FALSE, main = paste("Plot for",input$variable))
    pkt= examp_1111[examp_1111$var_value == input$grouping_variable,6]
    
    boxplot(pkt, xlab=paste(input$grouping_variable),ylab="Punkt")
    y=as.array(unlist(pkt))
    plot_ly(y = ~y, type = "box" )%>%
      layout(
        xaxis = list(title = paste(input$grouping_variable)),
        yaxis = list(title = "Punkte")
      )
    
  })
  
  output$my_plot_output <- renderPlot({
    req(iris_plot())
  })
  
  
  
  
  
  
  #For the summary statistics (of Punkte)
  
  punkt_summary <- eventReactive(input$update_button, {
  pkt_s= examp_1111[examp_1111$var_value == input$grouping_variable,6]
  stats_summary <- summary(pkt_s)
  stats_table <- as.data.frame(t(stats_summary))
  stats_table
  })# Reactive ends
  
  output$tbl = renderDT(
    stats_table,options = list(stats_table, 
      pageLength = 3)
  )
   
  # Render the table
  output$tbl = renderDT({
    req(punkt_summary())
  }, options = list(pageLength = 3))
  
  
  
  
}


# Run the application
shinyApp(ui, server)
















































































