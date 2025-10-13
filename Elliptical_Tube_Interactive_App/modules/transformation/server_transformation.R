# ---- server_transformation.R ----
server_transformation <- function(input, output, session) {
  
  tube1_mesh <- reactiveVal(NULL)
  tube2_mesh <- reactiveVal(NULL)
  trans_meshes <- reactiveVal(NULL)
  current_step <- reactiveVal(1)
  
  frozen_tube1 <- reactiveVal(NULL)
  frozen_tube2 <- reactiveVal(NULL)
  
  # ---- Helper to create and render a tube ----
  renderTube <- function(frames, alpha, beta, gamma, a, b, conn, color) {
    
    EulerAngles_Matrix <- cbind(
      rep(alpha, frames),
      rep(beta,  frames),
      rep(gamma, frames)
    )
    
    tube <- create_Elliptical_Tube(
      numberOfFrames      = frames,
      method              = "basedOnEulerAngles",
      EulerAngles_Matrix  = EulerAngles_Matrix,
      ellipseResolution   = 10,
      ellipseRadii_a      = rep(a, frames),
      ellipseRadii_b      = rep(b, frames),
      connectionsLengths  = rep(conn, frames),
      plotting            = FALSE
    )
    
    clear3d()
    open3d()
    
    quad_mesh <- tube_Surface_Mesh(
      tube      = tube,
      meshType  = "quadrilateral",
      plotMesh  = FALSE,
      decorate  = TRUE,
      color     = color
    )
    
    shade3d(quad_mesh, color = color, alpha = 1)
    wire3d(quad_mesh, color = "black", lwd = 2)
    
    list(tube = tube, mesh = quad_mesh)
  }
  
  # ---- Generate Both Tubes ----
  observeEvent(input$gen_both, {
    frames <- input$frames_global
    
    # Tube 1
    output$tube1Plot <- renderRglwidget({
      res1 <- renderTube(
        frames, input$alpha_t1, input$beta_t1, input$gamma_t1,
        input$a_t1, input$b_t1, input$conn_t1, input$col_t1
      )
      tube1_mesh(res1)
      frozen_tube1(res1$tube)
      rglwidget()
    })
    
    # Tube 2
    output$tube2Plot <- renderRglwidget({
      res2 <- renderTube(
        frames, input$alpha_t2, input$beta_t2, input$gamma_t2,
        input$a_t2, input$b_t2, input$conn_t2, input$col_t2
      )
      tube2_mesh(res2)
      frozen_tube2(res2$tube)
      rglwidget()
    })
  })
  
  # ---- Compute Transformation ----
  observeEvent(input$computeTrans, {
    req(frozen_tube1(), frozen_tube2())
    
    tube1 <- frozen_tube1()
    tube2 <- frozen_tube2()
    nsteps <- input$numSteps
    
    trans_list <- nonIntrinsic_Transformation_Elliptical_Tubes(
      tube1,
      tube2,
      type = "sizeAndShapeAnalysis",
      numberOfSteps = nsteps,
      plotting = FALSE
    )
    
    mesh_list <- lapply(seq_len(nsteps), function(i) {
      tube_Surface_Mesh(
        tube      = trans_list[[i]],
        meshType  = "quadrilateral",
        plotMesh  = FALSE,
        decorate  = TRUE,
        color     = "purple"
      )
    })
    
    trans_meshes(mesh_list)
    current_step(1)
    
    # Render first transformation step
    output$transPlot <- renderRglwidget({
      clear3d()
      open3d()
      shade3d(mesh_list[[1]], color = "purple", alpha = 0.8)
      wire3d(mesh_list[[1]], color = "black", lwd = 1)
      rglwidget()
    })
    output$stepInfo <- renderText(paste("Currently showing: Step 1 of", nsteps))
  })
  
  # ---- Step navigation ----
  observeEvent(input$nextStep, {
    req(trans_meshes())
    step <- current_step()
    if (step < length(trans_meshes())) {
      step <- step + 1
      current_step(step)
      output$transPlot <- renderRglwidget({
        clear3d()
        open3d()
        mesh <- trans_meshes()[[step]]
        shade3d(mesh, color = "purple", alpha = 0.8)
        wire3d(mesh, color = "black", lwd = 1)
        rglwidget()
      })
      output$stepInfo <- renderText(paste("Currently showing: Step", step, "of", length(trans_meshes())))
    }
  })
  
  observeEvent(input$prevStep, {
    req(trans_meshes())
    step <- current_step()
    if (step > 1) {
      step <- step - 1
      current_step(step)
      output$transPlot <- renderRglwidget({
        clear3d()
        open3d()
        mesh <- trans_meshes()[[step]]
        shade3d(mesh, color = "purple", alpha = 0.8)
        wire3d(mesh, color = "black", lwd = 1)
        rglwidget()
      })
      output$stepInfo <- renderText(paste("Currently showing: Step", step, "of", length(trans_meshes())))
    }
  })
  
  # ---- Download currently displayed step ----
  output$downloadCurrentStep <- downloadHandler(
    filename = function() {
      paste0("transformation_step_", current_step(), "_", Sys.Date(), ".obj")
    },
    content = function(file) {
      req(trans_meshes())
      mesh <- trans_meshes()[[current_step()]]
      mesh <- as.mesh3d(mesh)
      if (!is.null(mesh)) {
        writeOBJ(file)
      } else {
        stop("No transformation mesh available at this step.")
      }
    }
  )
}
