ui_transformation <- tabItem(
  tabName = "transformation",
  fluidPage(
    tags$head(
      tags$style(HTML("
        #tube1Plot, #tube2Plot, #transPlot {
          width: 100% !important;
          height: 70vh !important;
        }

        @media (max-width: 992px) {
          #tube1Plot, #tube2Plot, #transPlot { height: 50vh !important; }
        }

        /* Compact input layout */
        .compact-input .form-group {
          margin-bottom: 5px !important;
        }

        .compact-input input[type='number'] {
          width: 80px !important;
          height: 28px !important;
          padding: 2px 4px !important;
          font-size: 13px !important;
          display: inline-block !important;
        }

        .compact-input label {
          font-size: 13px !important;
          margin-bottom: 2px !important;
        }

        .compact-input .shiny-input-container {
          display: inline-block !important;
          margin-right: 6px !important;
        }
      "))
    ),
    
    titlePanel("Elliptical Tube Transformation"),
    
    # --- 1. Global Settings ---
    fluidRow(
      column(
        width = 12,
        box(
          title = "Global Settings",
          width = 12, solidHeader = TRUE, status = "warning",
          div(class = "compact-input",
              numericInput(
                "frames_global",
                "Number of Cross-Sections (applies to both tubes):",
                value = 15, min = 2
              )
          ),
          helpText("Both tubes share the same number of cross-sections.")
        )
      )
    ),
    
    # --- 2. Tube 1 Parameters ---
    fluidRow(
      column(
        width = 12,
        box(
          title = "Tube 1 Parameters", width = 12, solidHeader = TRUE, status = "primary",
          div(class = "compact-input",
              fluidRow(
                column(6,
                       numericInput("alpha_t1", "Alpha:", value = 0.2),
                       numericInput("beta_t1", "Beta:", value = 0.2),
                       numericInput("gamma_t1", "Gamma:", value = 0.2)
                ),
                column(6,
                       numericInput("a_t1", "Ellipse Radius a:", value = 3),
                       numericInput("b_t1", "Ellipse Radius b:", value = 2),
                       numericInput("conn_t1", "Cross-sectional distance:", value = 4, min = 0.1)
                )
              ),
              fluidRow(
                column(6, colourInput("col_t1", "Tube 1 Color:", value = "orange"))
              )
          )
        )
      )
    ),
    
    # --- 3. Tube 2 Parameters ---
    fluidRow(
      column(
        width = 12,
        box(
          title = "Tube 2 Parameters", width = 12, solidHeader = TRUE, status = "info",
          div(class = "compact-input",
              fluidRow(
                column(6,
                       numericInput("alpha_t2", "Alpha:", value = 0.1),
                       numericInput("beta_t2", "Beta:", value = -0.6),
                       numericInput("gamma_t2", "Gamma:", value = 0.4)
                ),
                column(6,
                       numericInput("a_t2", "Ellipse Radius a:", value = 5),
                       numericInput("b_t2", "Ellipse Radius b:", value = 4),
                       numericInput("conn_t2", "Cross-sectional distance:", value = 4, min = 0.1)
                )
              ),
              fluidRow(
                column(6, colourInput("col_t2", "Tube 2 Color:", value = "blue"))
              )
          )
        )
      )
    ),
    
    # --- 4. Generate Tubes Button ---
    fluidRow(
      column(
        width = 12,
        align = "center",
        actionButton(
          "gen_both", "Generate Tubes",
          class = "btn-primary btn-lg",
          style = "font-weight:bold; width:40%; color:white;"
        )
      )
    ),
    br(),
    
    # --- 5. Tube 1 and Tube 2 Plots ---
    fluidRow(
      column(
        width = 6,
        box(
          title = "Tube 1 Visualization",
          width = 12, solidHeader = TRUE, status = "primary",
          rglwidgetOutput("tube1Plot")
        )
      ),
      column(
        width = 6,
        box(
          title = "Tube 2 Visualization",
          width = 12, solidHeader = TRUE, status = "info",
          rglwidgetOutput("tube2Plot")
        )
      )
    ),
    
    # --- 6. Transformation Visualization ---
    fluidRow(
      column(
        width = 12,
        box(
          title = "Transformation Visualization",
          width = 12, solidHeader = TRUE, status = "success",
          
          div(class = "compact-input",
              numericInput(
                "numSteps",
                "Number of Transformation Steps:",
                value = 10,
                min = 2, max = 20, step = 1
              )
          ),
          br(),
          
          fluidRow(
            column(2, align = "center",
                   actionButton("prevStep", "← Previous", class = "btn-warning",
                                style = "width:100%; font-weight:bold;")),
            column(8, align = "center",
                   actionButton("computeTrans", "Compute Transformation", class = "btn-primary btn-block",
                                style = "width:100%; font-weight:bold;")),
            column(2, align = "center",
                   actionButton("nextStep", "Next →", class = "btn-warning",
                                style = "width:100%; font-weight:bold;"))
          ),
          
          br(),
          tags$div(style = "text-align:center; font-weight:bold;",
                   textOutput("stepInfo")),
          br(),
          rglwidgetOutput("transPlot"),
          br(),
          div(
            align = "center",
            downloadButton(
              "downloadCurrentStep",
              "Download Current Step Mesh (.obj)",
              style = "background-color:#007BFF; color:white; font-weight:bold;"
            )
          )
        )
      )
    )
  )
)
