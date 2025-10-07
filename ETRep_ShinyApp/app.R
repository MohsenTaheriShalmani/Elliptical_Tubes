# app.R
library(shiny)
library(rgl)
library(ETRep)
library(shinyWidgets)
library(colourpicker)

ui <- fluidPage(
  titlePanel("Elliptical Tube Generator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("frames", "Number of cross-sections:", value = 10, min = 1),
      numericInput("alpha", "Alpha (degrees in [-pi,pi]):", value = 0),
      numericInput("beta",  "Beta (degrees in [-pi,pi]):",  value = 0),
      numericInput("gamma", "Gamma (degrees in [-pi,pi]):", value = 0),
      
      colourInput("tubeColor", "Tube Color:", value = "orange"),  # color picker
      sliderInput("tubeAlpha", "Transparency (0=transparent, 1=opaque):",
                  min = 0, max = 1, value = 1, step = 0.05),
      
      actionButton("generate", "Generate Tube")
    ),
    
    mainPanel(
      # Container with black border
      tags$div(
        style = "border: 4px solid black; padding: 5px; display: inline-block;",
        rglwidgetOutput("tubePlot", width = "600px", height = "600px")
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$generate, {
    # Create Euler angles
    EulerAngles_alpha <- rep(input$alpha, input$frames)
    EulerAngles_beta  <- rep(input$beta,  input$frames)
    EulerAngles_gamma <- rep(input$gamma, input$frames)
    
    EulerAngles_Matrix <- cbind(
      EulerAngles_alpha,
      EulerAngles_beta,
      EulerAngles_gamma
    )
    
    # Create the tube
    tube <- create_Elliptical_Tube(
      numberOfFrames      = input$frames,
      method              = "basedOnEulerAngles",
      EulerAngles_Matrix  = EulerAngles_Matrix,
      ellipseResolution   = 10,
      ellipseRadii_a      = rep(3, input$frames),
      ellipseRadii_b      = rep(2, input$frames),
      connectionsLengths  = rep(4, input$frames),
      plotting            = FALSE
    )
    
    # Clear previous scene
    rgl.clear()
    
    # Create quadrilateral mesh
    quad_mesh <- tube_Surface_Mesh(
      tube      = tube,
      meshType  = "quadrilateral",
      plotMesh  = FALSE,
      decorate  = TRUE,
      color     = input$tubeColor
    )
    
    # Open new 3D device and draw mesh
    open3d()
    shade3d(quad_mesh, color = input$tubeColor, alpha = input$tubeAlpha)  # mesh with chosen color & alpha
    wire3d(quad_mesh, color = "black", lwd = 2)                           # black wireframe
    
    # Render widget
    output$tubePlot <- renderRglwidget({
      rglwidget()
    })
  })
}

shinyApp(ui, server)