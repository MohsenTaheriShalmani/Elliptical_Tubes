app1_ui <- tabItem(
  tabName = "app1",
  fluidPage(
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
          "generate", "Generate Tube",
          style = "background-color: #007BFF; color: white; font-weight: bold; font-size: 22px;"
        ),
        br(), br(),
        
        downloadButton("downloadMesh", "Download Tube Mesh Object *.obj",
                       style = "background-color: #007BFF; color: white; font-weight: bold;")
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
          ), "."
        ),
        tags$a(
          href = "https://github.com/MohsenTaheriShalmani/Elliptical_Tubes",
          target = "_blank",
          class = "btn btn-primary btn-lg",
          "Visit the GitHub Repository Elliptical Tubes"
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
)
