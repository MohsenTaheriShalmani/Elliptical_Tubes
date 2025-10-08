library(shiny)
library(rgl)
library(ETRep)
library(shinyWidgets)
library(colourpicker)

ui <- fluidPage(
  titlePanel("Elliptical Tube Generator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("frames", "Number of cross-sections (min = 2):", value = 15, min = 1),
      numericInput("alpha", "Alpha (degrees in [-3.14, 3.14]):", value = 0.2),
      numericInput("beta",  "Beta (degrees in [-3.14, 3.14]):",  value = 0.2),
      numericInput("gamma", "Gamma (degrees in [-3.14, 3.14]):", value = 0.2),
      
      numericInput("ellipseResolution", "Ellipse Resolution (min = 10):", value = 10, min = 3),
      numericInput("ellipseRadii_a", "Ellipse Radius a (min = 0.1):", value = 3, min = 0.1),
      numericInput("ellipseRadii_b", "Ellipse Radius b (min = 0.1):", value = 2, min = 0.1),
      numericInput("connectionsLengths", "Cross-sectional distance (min = 0.1):", value = 4, min = 0.1),
      
      colourInput("tubeColor", "Tube Color:", value = "orange"),  
      sliderInput("tubeAlpha", "Transparency (0=transparent, 1=opaque):",
                  min = 0, max = 1, value = 1, step = 0.1),
      
      actionButton(
        "generate",
        "Generate Tube",
        style = "
    background-color: #007BFF; 
    color: white; 
    font-weight: bold; 
    font-size: 22px; 
    padding: 12px 30px; 
    border-radius: 8px; 
    border: none; 
    box-shadow: 1px 6px #0056b3; 
    transition: all 0.2s;
  ",
        onclick = "
    this.style.transform='translateY(4px)';
    this.style.boxShadow='0 2px #0056b3';
    setTimeout(()=>{this.style.transform='translateY(0px)'; this.style.boxShadow='0 6px #0056b3';}, 150);
  "
      )
    ),
    
    mainPanel(
      tags$div(
        style = "border: 4px solid black; padding: 5px; display: inline-block;",
        rglwidgetOutput("tubePlot", width = "600px", height = "600px")
      ),
      
      br(), br(),
      
      tags$p(
        style = "font-size:16px; color:gray; font-style:italic;",
        "This Shiny application was developed by ",
        tags$a(
          href = "https://scholar.google.com/citations?user=PuWTeyIAAAAJ&hl=en",
          target = "_blank",
          style = "color:blue; text-decoration:underline;",
          "Mohsen Taheri Shalmani"
        ),
        "."
      ),
      tags$a(
        href = "https://github.com/MohsenTaheriShalmani/Elliptical_Tubes",
        target = "_blank",
        class = "btn btn-primary btn-lg",
        "Visit the GitHub Repository Elliptical_Tubes"
      ),
      br(), br(),
      tags$a(
        href = "https://doi.org/10.1080/10618600.2025.2535600",
        target = "_blank",
        class = "btn btn-primary btn-lg",
        "Visit the Reference Article"
      )
    )
  )
)

server <- function(input, output, session) {
  
  renderTube <- function(frames, alpha, beta, gamma,
                         res, a, b, conn,
                         color, alpha_val) {
    
    EulerAngles_Matrix <- cbind(
      rep(alpha, frames),
      rep(beta,  frames),
      rep(gamma, frames)
    )
    
    tube <- create_Elliptical_Tube(
      numberOfFrames      = frames,
      method              = "basedOnEulerAngles",
      EulerAngles_Matrix  = EulerAngles_Matrix,
      ellipseResolution   = res,
      ellipseRadii_a      = rep(a, frames),
      ellipseRadii_b      = rep(b, frames),
      connectionsLengths  = rep(conn, frames),
      plotting            = FALSE
    )
    
    rgl.clear()
    open3d()
    
    quad_mesh <- tube_Surface_Mesh(
      tube      = tube,
      meshType  = "quadrilateral",
      plotMesh  = FALSE,
      decorate  = TRUE,
      color     = color
    )
    
    shade3d(quad_mesh, color = color, alpha = alpha_val)
    wire3d(quad_mesh, color = "black", lwd = 2)
    
    rglwidget()
  }
  
  # Initial plot with defaults
  output$tubePlot <- renderRglwidget({
    renderTube(
      frames = 15,
      alpha = 0.2, 
      beta = 0.2, 
      gamma = 0.2,
      res = 10,
      a = 3,
      b = 2,
      conn = 4,
      color = "orange",
      alpha_val = 1
    )
  })
  
  # Update when clicking "Generate Tube"
  observeEvent(input$generate, {
    output$tubePlot <- renderRglwidget({
      renderTube(
        frames = input$frames,
        alpha  = input$alpha,
        beta   = input$beta,
        gamma  = input$gamma,
        res    = input$ellipseResolution,
        a      = input$ellipseRadii_a,
        b      = input$ellipseRadii_b,
        conn   = input$connectionsLengths,
        color  = input$tubeColor,
        alpha_val = input$tubeAlpha
      )
    })
  })
}

shinyApp(ui, server)
