library(shiny)
library(shinydashboard)
library(rgl)
library(ETRep)
library(shinyWidgets)
library(colourpicker)

# source components
source("app1_ui.R")
source("app1_server.R")
source("app2_ui.R")

# main UI
ui <- dashboardPage(
  dashboardHeader(title = "Elliptical Tube Interactive App (ETIA)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("App1: Tube Generator", tabName = "app1", icon = icon("cubes")),
      menuItem("App2: About ETRep", tabName = "app2", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      app1_ui,
      app2_ui
    )
  )
)

# main server
server <- function(input, output, session) {
  app1_server(input, output, session)
}

shinyApp(ui, server)
