library(shiny)
library(shinydashboard)
library(rgl)
library(ETRep)
library(shinyWidgets)
library(colourpicker)

# ---- Source modules ----
source("modules/tube_generator/ui_tube_generator.R")
source("modules/tube_generator/server_tube_generator.R")
source("modules/transformation/ui_transformation.R")
source("modules/transformation/server_transformation.R")
source("modules/about_etrep/ui_about_etrep.R")
source("modules/about_etrep/server_about_etrep.R")

# ---- UI ----
ui <- tagList(
  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    
    # ---- GLOBAL SIDEBAR CONTROL (responsive + manual toggle) ----
    tags$script(HTML("
      $(document).on('shiny:connected', function() {

        // Remove any stored sidebar state to start clean
        try { localStorage.removeItem('sidebar-toggle-collapsed'); } catch(e) {}

        let userToggled = false; // Track manual toggle

        // Manual toggle when clicking the hamburger icon
        $(document).on('click', '[data-toggle=\"push-menu\"]', function(e) {
          e.preventDefault();
          $('body').toggleClass('sidebar-collapse');
          userToggled = true; // Mark that user interacted
        });

        // Responsive behavior: auto-collapse only on smaller screens
        function adjustSidebar() {
          const isSmall = $(window).width() < 992;
          const isCollapsed = $('body').hasClass('sidebar-collapse');

          // Only enforce collapse on small screens
          if (isSmall && !isCollapsed) {
            $('body').addClass('sidebar-collapse');
          }
          // On large screens, do NOT override user toggle state
          else if (!isSmall && userToggled === false && isCollapsed) {
            $('body').removeClass('sidebar-collapse');
          }
        }

        // Run once when connected
        adjustSidebar();

        // Adjust only when resizing across thresholds
        let lastWidth = $(window).width();
        $(window).on('resize', function() {
          const currentWidth = $(window).width();
          const crossedBoundary =
            (lastWidth < 992 && currentWidth >= 992) ||
            (lastWidth >= 992 && currentWidth < 992);
          if (crossedBoundary) {
            adjustSidebar();
          }
          lastWidth = currentWidth;
        });
      });
    "))
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
        menuItem("Tube Generator", tabName = "tube_generator", icon = icon("cubes")),
        menuItem("Transformation", tabName = "transformation", icon = icon("arrows-alt")),
        menuItem("About ETRep", tabName = "about_etrep", icon = icon("info-circle"))
      )
    ),
    
    dashboardBody(
      tabItems(
        ui_tube_generator,
        ui_transformation,
        ui_about_etrep
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


# ---- SERVER ----
server <- function(input, output, session) {
  server_tube_generator(input, output, session)
  server_transformation(input, output, session)
  server_about_etrep(input, output, session)
}

shinyApp(ui, server)
