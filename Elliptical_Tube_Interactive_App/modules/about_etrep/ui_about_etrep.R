# ---- ui_about_etrep.R ----
ui_about_etrep <- tabItem(
  tabName = "about_etrep",
  fluidPage(
    tags$head(
      tags$style(HTML("
        .about-container {
          text-align: justify;
          font-size: 16px;
          line-height: 1.6;
          color: #f0f0f0;
          background-color: #2c3e50;
          padding: 30px;
          border-radius: 15px;
          box-shadow: 0 4px 12px rgba(0, 0, 0, 0.4);
        }

        .about-container h2 {
          text-align: center;
          color: #ffffff;
          margin-bottom: 25px;
          font-weight: bold;
        }

        .about-container p {
          margin-bottom: 15px;
        }

        .about-container a.btn {
          margin-right: 10px;
          margin-top: 10px;
          font-weight: bold;
          border-radius: 8px;
        }

        .logo-container {
          text-align: center;
          margin-bottom: 0px;
        }

        .logo-container img {
          height: 200px;
          border-radius: 12px;
          box-shadow: 0 4px 10px rgba(0, 0, 0, 0);
        }

        blockquote {
          margin: 10px 0 20px 0;
          padding-left: 15px;
          font-style: italic;
        }
      "))
    ),
    
    # ---- App Logo ----
    tags$div(
      class = "logo-container",
      tags$img(src = "ETIA_logo.png", alt = "ETIA Logo")
    ),
    
    # ---- Content ----
    tags$div(
      class = "about-container",
      h2("About ETIA"),
      p("The Elliptical Tube Interactive App (ETIA) is an interactive interface 
         built on the Elliptical Tube Representation (ETRep) R package, 
         designed for modeling and analyzing three-dimensional tube-like 
         structures through skeletal representations."),
      
      p("ETRep provides advanced functionality for generating, transforming, 
         and statistically analyzing elliptical tubes, enabling geometric 
         exploration of tubular objects with high precision. It supports 
         both intrinsic and extrinsic analyses, including the incorporation 
         of geometric constraints such as the Relative Curvature Condition (RCC)."),
      
      p("The ETIA platform allows users to:"),
      tags$ul(
        tags$li("Interactively generate and visualize 3D elliptical tubes."),
        tags$li("Perform smooth transformations between two tube geometries."),
        tags$li("Analyze mean shapes and geometric variability across samples."),
        tags$li("Download 3D mesh representations in standard formats (.obj).")
      ),
      
      p("These capabilities make ETIA a powerful research tool for 
         geometric morphometrics, shape statistics, and biomedical applications 
         such as vessel, airway, and colon morphology analysis."),
      
      hr(style = "border-top: 1px solid #999; margin: 25px 0;"),
      
      h4("Resources and Links"),
      p("To learn more, explore the following resources:"),
      tags$a(
        href = "https://github.com/MohsenTaheriShalmani/Elliptical_Tubes",
        target = "_blank",
        class = "btn btn-primary",
        icon("github"), " View Elliptical Tubes on GitHub"
      ),
      tags$a(
        href = "https://cran.r-project.org/web/packages/ETRep/index.html",
        target = "_blank",
        class = "btn btn-success",
        icon("r-project"), " View ETRep on CRAN"
      ),
      tags$a(
        href = "https://cucsym-mohsen-taheri.shinyapps.io/etrep_shinyapp/",
        target = "_blank",
        class = "btn btn-info",
        icon("play-circle"), " Launch Live Demo"
      ),
      
      hr(style = "border-top: 1px solid #999; margin: 25px 0;"),
      
      h4("Citation"),
      p("If you use ETIA or the ETRep package in your research, please cite:"),
      
      tags$blockquote(
        style = "color:#ddd; border-left: 4px solid #00aced; padding-left: 10px;",
        "Taheri Shalmani, M. (2025). Statistical Shape Analysis of Elliptical Tubes 
         Using Skeletal Representations. ",
        tags$i("Journal of Computational and Graphical Statistics."),
        " ",
        tags$a(
          href = "https://www.tandfonline.com/doi/full/10.1080/10618600.2025.2535600?src=",
          target = "_blank",
          "https://doi.org/10.1080/10618600.2025.2535600"
        )
      ),
      
      tags$blockquote(
        style = "color:#ddd; border-left: 4px solid #00aced; padding-left: 10px;",
        "Taheri Shalmani, M. (2024). PhD thesis, Shape Statistics via Skeletal Structures. 
         University of Stavanger. ",
        tags$a(
          href = "https://uis.brage.unit.no/uis-xmlui/handle/11250/3133161",
          target = "_blank",
          "View Thesis"
        )
      ),
      
      hr(style = "border-top: 1px solid #999; margin: 25px 0;"),
      
      p(style = "text-align:center; color:#bbb; font-size:14px;",
        HTML("&copy; 2025 Mohsen Taheri Shalmani — Developed using the ETRep R package")
      )
    )
  )
)
