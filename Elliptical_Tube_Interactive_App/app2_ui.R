app2_ui <- tabItem(
  tabName = "app2",
  fluidPage(
    titlePanel("About Elliptical Tube Interactive App (ETIA)"),
    fluidRow(
      column(
        width = 10, offset = 1,
        h3("Elliptical Tube Interactive App (ETIA)"),
        p("The Elliptical Tube Interactive App (ETIA) is built on the 
      Elliptical Tube Representation (ETRep) R package, which is designed 
      for modeling and analyzing 3D tube-like structures using skeletal 
      representations. It provides tools for generating elliptical tubes, 
      computing their mean shapes, and incorporating geometric constraints 
      such as the Relative Curvature Condition (RCC)."),
        
        p("ETRep supports the creation of discrete elliptical tubes from 
      user-defined parameters or skeletal curves and offers methods for 
      volumetric and cross-sectional analyses, making it particularly 
      valuable in biomedical research and related applications."),
        tags$a(
          href = "https://github.com/MohsenTaheriShalmani/Elliptical_Tubes",
          target = "_blank",
          class = "btn btn-primary",
          "Visit Elliptical Tubes on GitHub"
        ),
        tags$a(
          href = "https://cran.r-project.org/web/packages/ETRep/index.html",
          target = "_blank",
          class = "btn btn-primary",
          "Visit ETRep on CRAN"
        )
      )
    )
  )
)
