# Sources the packages used
source(here::here("packages.R"))









# load and modify the data
load(here::here("example_data_shiny.RData"))

data <- example_data # later change 

data$variablenname <- ifelse(is.na(data$variablenname), "No variable found", data$variablenname)









# Make a common function for the paste
paste_fun = function(task, stage){ return(paste("Task Name:", task, " - Stage: ", stage)) }



# Make the ui function
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
          selectizeInput("task_name_overall", "Task Name", choices = sort(unique(data$exercise_name))),
          
          # Dropdown menu for Stage
          selectizeInput("stage_overall", "Stage", choices = NULL)
          
        ), # end of dashboard controlbar
        fluidRow(
          
          # for the overall histogram plot for Punkte
          column(width = 10,
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
          )#End of column
        ) #End of fluidRow
      ), # end of tab 1
      
      # TAB 2
      tabItem(
        tabName = "Sinput",
        dashboardControlbar(
          width = 250,  # Adjust the width as needed
          # Dropdown menu for Rask Name
          
          selectizeInput("task_name_sinput", "Task Name", choices = NULL), #sort(unique(data$exercise_name))),
          
          # Dropdown menu for Stage
          selectizeInput("stage_sinput", "Stage", choices = NULL),
          
          # Dropdown menu for Field Name
          selectizeInput("fieldname_sinput", "Field Name", choices = NULL),
          
          # Dropdown menu for variable
          selectizeInput("variable_sinput", "Variable", choices = NULL),
          
          #Checkbox menu for grouping Variable
          checkboxGroupInput("grouping_variable_sinput", "Grouping Variable", choices = NULL)
          
          
          
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











# Server Function
server <- function(input, output, session) {
  
  
  
  # Update choices 
  observeEvent(input$task_name_overall, {
    
    choices_1 = reactive({as.character(unique(data[data$exercise_name == input$task_name_overall, 3]))}) 
    
   
    updateSelectizeInput(session, "stage_overall", choices = choices_1())
  })
  
  
  # Update choices
  observeEvent(input$task_name_overall, {
    
    choices_1 = reactive({sort(unique(data$exercise_name))}) 
    
    
    updateSelectizeInput(session, "task_name_sinput", choices = choices_1(), selected = input$task_name_overall )
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
  
  
  
  
  
  
  # Create data for Histogram for the OVERALL tab
  plotly_hist_data <- reactive({
    example_data %>%
      dplyr::filter(exercise_name == input$task_name_overall,
                    stage == input$stage_overall) 
  })
  
  # Create the histogram for the OVERALL tab
  output$plot_overall <- renderPlotly({
    
    xx=data.frame(table(plotly_hist_data()$punkte))
    
    
    # Calculate proportion
    total <- sum(xx$Freq)
    xx$proportion <- round(xx$Freq / total,2)
    
    plot_ly(x = xx$Var1, y = xx$Freq, type = "bar",text = ~paste("Punkte :", xx$Var1,
                                                                 "<br> Anzahl :", xx$Freq,
                                                                 "<br> Proportion :", xx$proportion),
            textposition = "none",
            hoverinfo = "text" ) %>%
      layout(
        xaxis = list(title = "Punkte"),
        yaxis = list(title = "Anzahl"),
        barmode = "group",  # Set bar mode to "group" for side-by-side bars
        bargap = 0.2  # Adjust the bargap to make the bars thinner (you can experiment with different values)
        
      ) 
    
    
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
            colors = c('right' = '#008000', 'false' = '#FF0000')) %>%
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
  
  
  
  
  
  # Create the data table for the OVERALL section
  default_table_overall = reactive({

    xx=data.frame(table(plotly_hist_data()$punkte))
    x = xx$Var1
    y = xx$Freq
    summ= sum(y)
    z = round(y/summ,2)
    result_df=data.frame("Punkte" = x, "Anzahl" = y, "Proportion" = z)
    
  })
  
  
  
  # Present the table for the OVERALL section
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
  
 
  
  
  
  
  
  # Create the data table for the STUDENT'S INPUT section
  default_table_sinput = reactive({
    
    
    exd_right <- plotly_bar_data()%>%
      dplyr::filter(right == "right")
    x= exd_right$var_value
    y=exd_right$percent
    z = 100-y
    data.frame("Grouping Variable"=x,"True"=paste(round(y,2), "%", sep = ""),"False"=paste(round(z,2), "%", sep = ""))
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
 
  
  
  # Create heading for histogram for OVERALL section
  output$plot_overall_title <- renderUI({
    title_text = paste_fun(input$task_name_overall,input$stage_overall)
    h4(title_text)  
  })
  
  # Create heading for data table for OVERALL section
  output$table_overall_title <- renderUI({
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













































# Function to hash password using bcrypt
hash_password <- function(password) {
  hashed_password <- bcrypt::hashpw(password, salt = bcrypt::gensalt())
  return(hashed_password)
}


# Define your secret key
key <- 123

# Encrypt passwords in the user_base tibble
user_base <- user_base %>%
  mutate(encrypted_password = sapply(password, hash_password),
         password = NULL)  # Remove the plain text password column










