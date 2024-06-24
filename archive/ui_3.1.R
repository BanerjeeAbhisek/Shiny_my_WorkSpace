source(here::here("packages.R"))

# load data 
load(here::here("example_data_shiny.RData"))

data <- example_data

data$variablenname <- ifelse(is.na(data$variablenname), "No variable found", data$variablenname)


A = c("Nothing", "Var_values") # What is this? Choice should be nothing, Versions or var_value






ui <- dashboardPage(
  
  
  
  #Dashboard header
  dashboardHeader(title = h2("Title", style = "font-size:20px;"), titleWidth = 230),
  
  
  
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
             conditionalPanel(
               condition = "input.sort_Option == 'Nothing'",
               box(
                 title = "Distribution of total points",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 width = 12,  # Set the width (out of 12 columns)
                 height = 500, # Set the height in pixels
                 offset = 500, 
                 #if (input$sort_Option == 'Nothing') plotOutput("my_plot_output_1") else plotOutput("my_plot_output")
                 plotOutput("my_plot_output_1")  # Plot will be rendered here
               )# End of box
             ),#End of conditional panel,
             conditionalPanel(
               condition = "input.sort_Option == 'Var_values'",
               box(
                 title = "Distribution of points based on grouping variables variables ",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 width = 12,  # Set the width (out of 12 columns)
                 height = 500, # Set the height in pixels
                 offset = 500, 
                 #if (input$sort_Option == 'Nothing') plotOutput("my_plot_output_1") else plotOutput("my_plot_output")
                 plotOutput("my_plot_output")  # Plot will be rendered here
               )# End of box
             ),#End of conditional panel,
             
             conditionalPanel(
               condition = "input.sort_Option == 'Nothing'",
               box(
                 title = "Summary statistics",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 width = 12,  # Set the width (out of 12 columns)
                 height = "auto", # Set the height in pixels
                 offset = 500, 
                 DTOutput('table_output_1')  # Summary statistics will be rendered here
                 
               ) # end of box
             ),#End of conditional panel
             
             conditionalPanel(
               condition = "input.sort_Option == 'Var_values'",
               box(
                 title = "Summary statistics",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 width = 12,  # Set the width (out of 12 columns)
                 height = "auto", # Set the height in pixels
                 offset = 500, 
                 DTOutput('table_output')  # Summary statistics will be rendered here
                 
               ) # end of box
             )#End of conditional panel
             
             
             
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
               condition = "input.sort_Option == 'Var_values'",
               box(
                 title = "Grouping Variables",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 width = 12,  # Set the width (out of 12 columns)
                 height = "auto", # Set the height in pixels
                 offset = 500, 
                 # Dropdown menu with options from array B
                 checkboxGroupInput("grouping_variable", "Select Grouping Variable", choices = NULL),
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
  
  
  # Update grouping variable
  #Update choices for input 4 based on input 1 and input 2 and input 3
  observeEvent(c(input$task_name, input$stage, input$version, input$fieldname, input$variable), {
    choices_grouping_variable <- reactive({
      # Assuming data$variablenname is a vector
      
      unlist(unique(subset(example_data, 
                           exercise_name == input$task_name & 
                             stage ==  input$stage & 
                             feldname == input$fieldname & 
                             #eingereichte_antwort == 'Antonym' & 
                             variablenname == input$variable)$var_value))
    })
    # Update choices for input 2
    updateCheckboxGroupInput(session, "grouping_variable", choices =  choices_grouping_variable(), selected = choices_grouping_variable())
    
    
    
  })
  
  
  # Create the default plot
  default_plot= reactive({
    y=example_data$punkte
    counts <- table(ifelse(y < 50, "< 50", ">= 50"))
    frac=counts / sum(counts) * 100
    m=data.frame(frac)
    y=m$Freq
    red_line= y[2]
    x=rownames(counts)
    tp= "Total Punkt"
    # Create a data frame for plotting
    df <- data.frame(
      x = rep(tp, length(counts)),
      y = y,  # Convert counts to percentages
      Var_val = names(counts)
    )
    
    # Create a stacked bar plot
    ggplot(df, aes(x = x, y = y, fill = Var_val)) +
      geom_bar(stat = "identity", width = 0.5) +
      geom_text(aes(label = Var_val), position = position_stack(vjust = 0.5), color = "white") +
      labs(x = "Punkte", y = "Percentage", title = "") +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      theme_minimal() + theme(legend.position = "none")+
      scale_fill_manual(values = c("orange", "steelblue")) +  # Set bar colors
      scale_color_manual(values = c("white", "white"))+
      geom_hline(yintercept = red_line, color = "red", linetype = "dashed")
    
  })
  
  output$my_plot_output_1 <- renderPlot({
    # Add vertical red line
    req(default_plot())
  })
  
  
  
  # Define output text based on inputs
  iris_plot <- eventReactive(input$update_button, {
    
    # Initialize an empty list to store individual bar plots
    plot_list <- list()
    for (var_value in input$grouping_variable) {
      
      #To create the red line
      y=example_data$punkte
      counts <- table(ifelse(y < 50, "< 50", ">= 50"))
      frac=counts / sum(counts) * 100
      m=data.frame(frac)
      y=m$Freq
      red_line= y[2]
      
      
      #    hist(data$punkte,breaks = 5,xlab = "Punkte",ylab = "Proportion",freq = FALSE, main = paste("Plot for",input$variable))
      examp112=subset(example_data, 
                      exercise_name == input$task_name & 
                        stage ==  input$stage & 
                        feldname == input$fieldname & 
                        #eingereichte_antwort == 'Antonym' & 
                        variablenname == input$variable)
      pkt=examp112[sapply(examp112$var_value, function(sub_list) var_value %in% sub_list),6]
      y=as.array(unlist(pkt))
      counts <- table(ifelse(y < 50, "< 50", ">= 50"))
      frac=counts / sum(counts) * 100
      m=data.frame(frac)
      y=m$Freq
      x=rownames(counts)
      # Create a data frame for plotting
      df <- data.frame(
        x = rep(var_value, length(counts)),
        y = y,  # Convert counts to percentages
        Var_val = names(counts)
      )
      
      # Create a stacked bar plot
      bar_plot <- ggplot(df, aes(x = x, y = y, fill = Var_val)) +
        geom_bar(stat = "identity", width = 0.5) +
        geom_text(aes(label = Var_val), position = position_stack(vjust = 0.5), color = "white") +
        labs(x = "Punkte", y = "Percentage", title = "") +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        theme_minimal() + theme(legend.position = "none")+
        scale_fill_manual(values = c("orange", "steelblue")) +  # Set bar colors
        scale_color_manual(values = c("white", "white")) + # Set text color
        geom_hline(yintercept = red_line, color = "red", linetype = "dashed") 
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
  
  
  
  
  # Create Default table
  default_table = reactive({
    pkt =example_data$punkte
    y=as.array(unlist(pkt))
    counts <- table(ifelse(y < 50, "< 50", ">= 50"))
    frac=counts / sum(counts) * 100
    m_c=data.frame(counts)
    y_c=m_c$Freq
    m_f=data.frame(frac)
    y_f=m_f$Freq
    data.frame("Grouping_variable" = NA, "less_50" = y_c[1], "percent_less_50" =  paste(round(y_f[1],1), "%", sep = ""), "greater_50" = y_c[2], "percent_greater_50" =  paste(round(y_f[2],1), "%", sep = ""))
    
    
  })
  
  output$table_output_1 <- renderDT({
    req(default_table())
  }, options = list(pageLength = 3))
  
  
  
  #For the summary statistics (of Punkte)
  
  table_summary <- eventReactive(input$update_button, {
    # Define the data frame with corrected column names
    table_list= data.frame("variable" = "Total Punkte", "less_50" = NA, "percent_less_50" = NA, "greater_50" = NA, "percent_greater_50" = NA)
    #Iterate through each choice in grouping variables
    for (var_value in input$grouping_variable) {
      
      examp112=subset(example_data, 
                      exercise_name == input$task_name & 
                        stage ==  input$stage & 
                        feldname == input$fieldname & 
                        #eingereichte_antwort == 'Antonym' & 
                        variablenname == input$variable)
      pkt=examp112[sapply(examp112$var_value, function(sub_list) var_value %in% sub_list),6]
      y=as.array(unlist(pkt))
      counts <- table(ifelse(y < 50, "< 50", ">= 50"))
      frac=counts / sum(counts) * 100
      m_c=data.frame(counts)
      y_c=m_c$Freq
      m_f=data.frame(frac)
      y_f=m_f$Freq
      new_row= c(var_value,y_c[1], paste(round(y_f[1],1), "%", sep = ""),y_c[2],paste(round(y_f[2],1), "%", sep = ""))
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




















