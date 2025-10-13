# ---- ui_tube_generator.R ----
ui_tube_generator <- tabItem(
  tabName = "tube_generator",
  fluidPage(
    tags$head(
      # ---- Responsive CSS for mobile scaling ----
      tags$style(HTML("
        #tubePlot {
          width: 100% !important;
          height: 70vh !important;
        }

        @media (max-width: 1024px) {
          #tubePlot { height: 60vh !important; }
        }

        @media (max-width: 768px) {
          #tubePlot { height: 50vh !important; }
        }

        @media (max-width: 480px) {
          #tubePlot { height: 45vh !important; }
        }

        /* Container for slider + arrows */
        .slider-container {
          display: flex;
          align-items: center;
          gap: 6px;
        }

        .slider-stepper {
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: center;
          cursor: pointer;
          background-color: #eee;
          border: 1px solid #ccc;
          border-radius: 4px;
          width: 22px;
          height: 36px;
          font-weight: bold;
          line-height: 12px;
          user-select: none;
        }

        .slider-stepper:hover {
          background-color: #ddd;
        }

        .slider-stepper span {
          line-height: 10px;
        }
      ")),
      
      # ---- JS to handle arrow clicks ----
      tags$script(HTML("
        Shiny.addCustomMessageHandler('addSliderArrows', function(sliderIds) {
          sliderIds.forEach(function(id) {
            const el = document.getElementById(id);
            if (!el) return;
            if (el.parentElement.classList.contains('slider-container')) return;

            const container = document.createElement('div');
            container.className = 'slider-container';
            el.parentElement.insertBefore(container, el);
            container.appendChild(el);

            const arrows = document.createElement('div');
            arrows.className = 'slider-stepper';
            arrows.innerHTML = '<span>▲</span><span>▼</span>';
            container.appendChild(arrows);

            const up = arrows.children[0];
            const down = arrows.children[1];

            up.addEventListener('click', () => {
              const current = parseFloat(el.value);
              const step = parseFloat(el.step || 1);
              const max = parseFloat(el.max);
              const newValue = Math.min(current + step, max);
              el.value = newValue;
              el.dispatchEvent(new Event('input', { bubbles: true }));
            });

            down.addEventListener('click', () => {
              const current = parseFloat(el.value);
              const step = parseFloat(el.step || 1);
              const min = parseFloat(el.min);
              const newValue = Math.max(current - step, min);
              el.value = newValue;
              el.dispatchEvent(new Event('input', { bubbles: true }));
            });
          });
        });
      "))
    ),
    
    titlePanel("Elliptical Tube Generator"),
    
    sidebarLayout(
      sidebarPanel(
        sliderInput("frames", "Number of Frames:",
                    min = 2, max = 100, value = 15, step = 1),
        
        sliderInput("alpha", "Alpha:",
                    min = -round(pi/2-0.01,2), max = round(pi/2-0.01,2), value = 0.2, step = 0.01),
        sliderInput("beta", "Beta:",
                    min = -round(pi/2-0.01,2), max = round(pi/2-0.01,2), value = 0.2, step = 0.01),
        sliderInput("gamma", "Gamma:",
                    min = -round(pi-0.01,2), max = round(pi-0.01,2), value = 0.2, step = 0.01),
        
        sliderInput("ellipseResolution", "Ellipse Resolution:",
                    min = 4, max = 20, value = 10, step = 1),
        sliderInput("ellipseRadii_a", "Ellipse Radius a:",
                    min = 0.0001, max = 100, value = 3, step = 0.01),
        sliderInput("ellipseRadii_b", "Ellipse Radius b:",
                    min = 0.0001, max = 100, value = 2, step = 0.01),
        sliderInput("connectionsLengths", "Connection Length:",
                    min = 0.0001, max = 100, value = 4, step = 0.01),
        colourInput("tubeColor", "Tube Color:", value = "orange"),  
        sliderInput("tubeAlpha", "Transparency (0=transparent, 1=opaque):",
                    min = 0, max = 1, value = 1, step = 0.1),
        
        actionButton(
          "generate", "Generate Tube",
          style = "background-color: #007BFF; color: white; font-weight: bold; font-size: 22px;"
        )
      ),
      
      mainPanel(
        fluidRow(
          column(
            width = 12,
            align = "center",
            tags$div(
              style = "
                border: 4px solid black;
                padding: 5px;
                width: 100%;
                max-width: 900px;
                margin: auto;
              ",
              rglwidgetOutput("tubePlot", width = "100%", height = "70vh")
            )
          )
        ),
        
        br(),
        
        # Download button moved outside sidebar
        fluidRow(
          column(
            width = 12,
            align = "center",
            downloadButton(
              "downloadMesh",
              "Download Tube Mesh Object *.obj",
              style = "background-color: #007BFF; color: white; font-weight: bold;"
            )
          )
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
