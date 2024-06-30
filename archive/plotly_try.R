library(shiny)
library(plotly)

ui <- fluidPage(
  selectInput("choice", "Choose", choices = names(iris), selected = NULL),
  plotlyOutput("graph")
)

server <- function(input, output, session){
  
  output$graph <- renderPlotly({
    plot_ly(iris, x = ~get(input$choice), y = ~Sepal.Length, type = 'scatter', mode = 'markers')
  })
}

shinyApp(ui, server)