library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

shinyApp(
  ui = dashboardPage(
    header = dashboardHeader(),
    sidebar = dashboardSidebar(),
    body = dashboardBody(),
    controlbar = dashboardControlbar(
      skin = "dark",
      controlbarMenu(
        id = "menu"
      )
    ),
    title = "DashboardPage"
  ),
  server = function(input, output) { }
)
