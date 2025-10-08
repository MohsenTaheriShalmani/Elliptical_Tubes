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
ui <- tagList(
  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico")
  ),
  
  dashboardPage(
    dashboardHeader(
      title = tags$div(
        tags$img(src = "favicon.ico", height = "20px", style = "margin-right:5px;"),
        "Elliptical Tube Interactive App (ETIA)"
      )
    ),
    
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
      ),
      
      ## ---- Global Footer ----
      tags$hr(),
      tags$div(
        style = "text-align:center; font-size:14px; color:gray; padding:10px;",
        "© 2025 Mohsen Taheri Shalmani. All rights reserved."
      )
    )
  )
)


# main server
server <- function(input, output, session) {
  app1_server(input, output, session)
}

shinyApp(ui, server)
