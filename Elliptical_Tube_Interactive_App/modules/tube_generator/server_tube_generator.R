# ---- server_tube_generator.R ----
server_tube_generator <- function(input, output, session) {
  
  quad_mesh_reactive <- reactiveVal(NULL)
  
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
    
    quad_mesh_reactive(quad_mesh)
    
    rglwidget()
  }
  
  # initial tube
  output$tubePlot <- renderRglwidget({
    renderTube(15, 0.2, 0.2, 0.2, 10, 3, 2, 4, "orange", 1)
  })
  
  # regenerate on button click
  observeEvent(input$generate, {
    output$tubePlot <- renderRglwidget({
      renderTube(
        input$frames, input$alpha, input$beta, input$gamma,
        input$ellipseResolution, input$ellipseRadii_a, input$ellipseRadii_b,
        input$connectionsLengths, input$tubeColor, input$tubeAlpha
      )
    })
  })
  
  # download
  # Download handler
  output$downloadMesh <- downloadHandler(
    filename = function() {
      paste0("elliptical_tube_", Sys.Date(), ".obj")  
    },
    content = function(file) {                      
      mesh <- quad_mesh_reactive()
      class(as.mesh3d(mesh))
      if (!is.null(mesh)) {
        writeOBJ(file)      # save the mesh3d as an R object
      } else {
        stop("No tube generated yet!")
      }
    }
  )
}
