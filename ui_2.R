source(here::here("packages.R"))

# load data 
load(here::here("example_data_shiny.RData"))

data <- example_data

data$variablenname <- ifelse(is.na(data$variablenname), "No variable found", data$variablenname)


examp_1111 <- subset(example_data, 
                     exercise_name == '04 Semantik' & 
                       stage == 1 & 
                       feldname == 'dropdown1' & 
                       eingereichte_antwort == 'Antonym' & 
                       variablenname == 'satzAntonym')

A = c("No Grouping Variables", "Grouping Variables") # What is this? Choice should be nothing, Versions or var_value

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
               DTOutput('table_output')  # Summary statistics will be rendered here
               
               
             ) # end of box
             
             
             
      ), # End of column
      
      
      column(width = 4,
             
             box(
               title = "Selection Option",
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,  # Set the width (out of 12 columns)
               height = 200, # Set the height in pixels
               offset = 500, 
               # Dropdown menu with options from array B
               selectInput("sort_Option", "Select By", choices = A),
               # Add more UI elements as needed
             ),#End of Box
             
             conditionalPanel(
               condition = "input.sort_Option == 'Grouping Variables'",
               box(
                 title = "Grouping Variables",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 width = 12,  # Set the width (out of 12 columns)
                 height = 500, # Set the height in pixels
                 offset = 500, 
                 # Dropdown menu with options from array B
                 checkboxGroupInput("grouping_variable", "Select Grouping Variable", choices = B),
                 #selectInput("grouping_variable", "Select Grouping Variable", choices = B),
                 # Add more UI elements as needed
               )#End of Box
             )# End of conditionalPanel
             
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
    
    # Initialize an empty list to store individual bar plots
    plot_list <- list()
    for (var_value in input$grouping_variable) {
      
      
      #    hist(data$punkte,breaks = 5,xlab = "Punkte",ylab = "Proportion",freq = FALSE, main = paste("Plot for",input$variable))
      pkt= examp_1111[examp_1111$var_value == var_value,6]
      y=as.array(unlist(pkt))
      # Count occurrences of values less than 100 and 100
      counts <- table(ifelse(pkt < 100, "< 100", "100"))
      m =data.frame(counts)
      y=m$Freq
      x=rownames(counts)
      df= data.frame(x,y)
      bar_plot =  ggplot(df, aes(x=x, y=y)) +
        geom_bar(stat = "identity", fill = c("orange", "steelblue")) +
        labs(x = "Punkte", y = "Count", title = paste(var_value))
      # Create a bar plot
      plot_list[[var_value]] <- bar_plot
      
    }
    #return(plot_list)
    # Calculate the number of rows and columns for the grid
    num_plots <- length(input$grouping_variable)
    num_cols <- min(3, num_plots)  # Set the maximum number of columns per row
    num_rows <- ceiling(num_plots / num_cols)
    grid.arrange(grobs = plot_list, ncol = num_cols, nrow = num_rows)# ncol = length(input$grouping_variable))
  })
  
  
  #})
  
  output$my_plot_output <- renderPlot({
    #grid.arrange(grobs = iris_plot(), ncol = length(input$grouping_variable))
    #do.call(grid.arrange, c(iris_plot(), ncol = length(input$grouping_variable)))
    req(iris_plot())
  })
  
  
  
  
  
  
  #For the summary statistics (of Punkte)
  
  table_summary <- eventReactive(input$update_button, {
    table_list <- data.frame("Grouping_variable"= NA, "No"=NA, "Yes"= NA)
    
    for (var_value in input$grouping_variable) {
      # Subset the data for the current var_value
      pkt = examp_1111[examp_1111$var_value == var_value, 6]
      mod_counts <- table(ifelse(pkt < 100, "< 100", "100"))
      new_row= c(var_value,mod_counts[[1]],mod_counts[[2]])
      table_list = rbind(table_list,new_row)
    }
    
    tbl = table_list[-1,]
    rownames(tbl) <- NULL
    tbl
    
  })
  
  
  
  # Render the table using shiny DT
  output$table_output <- renderDT({
    req(table_summary())
  }, options = list(pageLength = 3))
  
  
  
  
}


# Run the application
shinyApp(ui, server)








































































