#' @importFrom rgl plot3d shade3d decorate3d open3d qmesh3d tmesh3d
#' @importFrom grDevices colorRampPalette
#' @importFrom stats cov pf runif var
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom fields image.plot
#' @importFrom truncnorm rtruncnorm
#' @importFrom rotations is.SO3 as.SO3 as.Q4 rot.dist

# Calculating the size of an ETRep based on L1 norm
#' @keywords internal
.ETRepSize <- function(tube) {
  vec<-c(tube$connectionsLengths,tube$ellipseRadii_a,tube$ellipseRadii_b)
  vec_clean <- vec[!is.na(vec)]

  #size based on L_1 norm so the scaling is v/||v||_1
  size<-sum(abs(vec_clean))

  ## Alternatively we can consider the size as the mean(abs(vec_clean))
  ## in this format the ETReps scale by the average arithmetic mean.
  ## Again the scaled shapes belongs to the hyperoctahedron |x_1|+...+|x_d|=d
  ## Aize based on average arithmetic mean so the scaling is v/(||v||_1/d)
  # size<-mean(abs(vec_clean))

  return(size)
}

# Normalization of an elliptical tube by uniform scaling
#' @keywords internal
.scaleETRepToHaveTheSizeAsOne <- function(tube,
                                          plotting=FALSE) {

  size<-.ETRepSize(tube = tube)

  tubeScaled<-create_Elliptical_Tube(numberOfFrames = nrow(tube$spinalPoints3D),
                                     method = "basedOnMaterialFrames",
                                     materialFramesBasedOnParents = tube$materialFramesBasedOnParents,
                                     ellipseRadii_a = tube$ellipseRadii_a/size,
                                     ellipseRadii_b = tube$ellipseRadii_b/size,
                                     connectionsLengths = tube$connectionsLengths/size,
                                     plotting = plotting)
  return(tubeScaled)
}


# Extract the tangent vector as the second element of the material frame
#' @keywords internal
.convert_v_to_TangentVector<- function(v) {

  if(length(v)!=2){stop("v is not a 2-dimensional vector!")}

  x<-v[1]
  y<-v[2]
  z<-abs(sqrt(abs(1-x^2-y^2)))
  unitTangent<-.convertVec2unitVec2(c(z,x,y))
  return(unitTangent)

}

# Calculate the r_project
#' @keywords internal
.calculate_r_project_Length <- function(a, b, theta) {

  t_max<-atan(-b/a*tan(theta))
  r_project_length<-abs(a*cos(t_max)*cos(theta)-b*sin(t_max)*sin(theta))

  return(r_project_length)
}

# calcualting the twisting angle theta based on the given frames
#' @keywords internal
.calculate_theta <- function(materialFrameBasedOnParent,
                             tolerance=1e-7) {

  #reference frame is I
  t_vec<-materialFrameBasedOnParent[1,]
  a_vec<-materialFrameBasedOnParent[2,]
  b_vec<-materialFrameBasedOnParent[3,]

  u2<-t_vec
  u1<-c(-1,0,0)
  psiTemp<-.geodesicDistance(u1,u2)
  if(abs(psiTemp-pi)>tolerance){
    #formula from function .geodesicPathOnUnitSphere()
    n_vec<-1/sin(psiTemp)*(sin(pi/2)*u1+sin(psiTemp-pi/2)*u2)
  }else{
    n_vec<-c(0,1,0)
  }

  theta<-.geodesicDistance(n_vec,a_vec)

  return(theta)
}

# Create a discrete e-tube based on the material frames
#' Create a Discrete Elliptical Tube (ETRep)
#'
#' Constructs a discrete elliptical tube (ETRep) based on specified parameters.
#'
#' @param numberOfFrames Integer, specifies the number of consecutive material frames.
#' @param method String, either "basedOnEulerAngles" or "basedOnMaterialFrames", defines the material frames method.
#' @param EulerAngles_Matrix Matrix of dimensions numberOfFrames x 3 with Euler angles to define material frames.
#' @param materialFramesBasedOnParents Array (3 x 3 x numberOfFrames) with pre-defined material frames.
#' @param ellipseResolution Integer, resolution of elliptical cross-sections (default is 10).
#' @param ellipseRadii_a Numeric vector for the primary radii of cross-sections.
#' @param ellipseRadii_b Numeric vector for the secondary radii of cross-sections.
#' @param connectionsLengths Numeric vector for lengths of spinal connection vectors.
#' @param initialFrame Matrix 3 x 3 as the initial frame
#' @param initialPoint Real vector with three elemets as the initial point
#' @param plotting Logical, enables plotting of the ETRep (default is TRUE).
#' @return List containing tube details (orientation, radii, connection lengths, boundary points, etc.).
#' @references
#' Taheri, M., Pizer, S. M., & Schulz, J. (2024). "The Mean Shape under the Relative Curvature Condition." arXiv.
#' \doi{10.48550/arXiv.2404.01043}
#'
#' Taheri Shalmani, M. (2024). "Shape Statistics via Skeletal Structures." University of Stavanger.
#' \doi{10.13140/RG.2.2.34500.23685}
#'
#' @examples
#' numberOfFrames<-15
#' EulerAngles_alpha<-c(rep(0,numberOfFrames))
#' EulerAngles_beta<-c(rep(-pi/20,numberOfFrames))
#' EulerAngles_gamma<-c(rep(0,numberOfFrames))
#' EulerAngles_Matrix<-cbind(EulerAngles_alpha,
#'                           EulerAngles_beta,
#'                           EulerAngles_gamma)
#' tube <- create_Elliptical_Tube(numberOfFrames = numberOfFrames,
#'                                method = "basedOnEulerAngles",
#'                                EulerAngles_Matrix = EulerAngles_Matrix,
#'                                ellipseResolution = 10,
#'                                ellipseRadii_a = rep(3, numberOfFrames),
#'                                ellipseRadii_b = rep(2, numberOfFrames),
#'                                connectionsLengths = rep(4, numberOfFrames),
#'                                plotting = FALSE)
#' # Plotting
#' \dontrun{
#'  plot_Elliptical_Tube(tube = tube,plot_frames = FALSE,
#'                       plot_skeletal_sheet = TRUE,
#'                       plot_r_project = FALSE,
#'                       plot_r_max = FALSE,add = FALSE)
#'  }
#' @export
create_Elliptical_Tube <- function(numberOfFrames,
                                   method,
                                   materialFramesBasedOnParents=NA,
                                   initialFrame=diag(3),
                                   initialPoint=c(0,0,0),
                                   EulerAngles_Matrix=NA,
                                   ellipseResolution=10,
                                   ellipseRadii_a,
                                   ellipseRadii_b,
                                   connectionsLengths,
                                   plotting=TRUE) {
  
  if(length(connectionsLengths)==(numberOfFrames-1)){
    connectionsLengths<-c(0,connectionsLengths)
  }
  
  #I<-diag(3)
  
  if(method=="basedOnEulerAngles"){
    
    EulerAngles_alpha<-EulerAngles_Matrix[,1]
    EulerAngles_beta<-EulerAngles_Matrix[,2]
    EulerAngles_gamma<-EulerAngles_Matrix[,3]
    
    materialFramesBasedOnParents<-array(NA,dim = c(3,3,numberOfFrames))
    materialFramesBasedOnParents[,,1]<-initialFrame
    for (i in 2:numberOfFrames) {
      materialFramesBasedOnParents[,,i]<-RSpincalc::EA2DCM(EA = c(EulerAngles_alpha[i],EulerAngles_beta[i],EulerAngles_gamma[i]),
                                                           EulerOrder = 'zyx')
    }
  }else if(method=="basedOnMaterialFrames" &
           !any(is.na(materialFramesBasedOnParents &
                      dim(materialFramesBasedOnParents)[3]==numberOfFrames))){
    materialFramesBasedOnParents<-materialFramesBasedOnParents
  }else{
    stop("Please specify the method as basedOnEulerAngles or basedOnMaterialFrames !")
  }
  
  framesCenters<-1:numberOfFrames
  framesParents<-c(1,1:(numberOfFrames-1))
  
  materialFramesGlobalCoordinate<-array(NA,dim = dim(materialFramesBasedOnParents))
  materialFramesGlobalCoordinate[,,1]<-initialFrame
  for (k in 2:numberOfFrames) {
    parent_Index<-framesParents[k]
    child_Index<-framesCenters[k]
    updatedParent<-materialFramesGlobalCoordinate[,,parent_Index]
    materialFramesGlobalCoordinate[,,child_Index]<-
      .rotateFrameToMainAxesAndRotateBack_standard(myFrame = updatedParent,
                                                   vectors_In_I_Axes = materialFramesBasedOnParents[,,child_Index])
  }
  
  # spinal points
  spinalPoints3D<-array(NA,dim = c(numberOfFrames,3))
  spinalPoints3D[1,]<-initialPoint
  for (i in 1:(numberOfFrames-1)) {
    spinalPoints3D[i+1,]<-spinalPoints3D[i,]+
      connectionsLengths[i+1]*materialFramesGlobalCoordinate[1,,i]
  }
  
  ##################################################################
  #Frenet frames
  
  frenetFramesGlobalCoordinate<-array(NA,dim = c(3,3,numberOfFrames))
  frenetFramesGlobalCoordinate[,,1]<-materialFramesGlobalCoordinate[,,1]
  for (i in 2:(numberOfFrames-1)) {
    t_vec<-.convertVec2unitVec2(spinalPoints3D[i+1,]-spinalPoints3D[i,])
    u1<-.convertVec2unitVec2(spinalPoints3D[i-1,]-spinalPoints3D[i,])
    # u1<-c(-1,0,0)
    u2<-t_vec
    psiTemp<-.geodesicDistance(u1,u2)
    if(abs(psiTemp-pi)>10^-5){
      #formula from .geodesicPathOnUnitSphere()
      n_vec<-1/sin(psiTemp)*(sin(pi/2)*u1+sin(psiTemp-pi/2)*u2)
      b_perb_vec<-.convertVec2unitVec2(.myCrossProduct(t_vec,n_vec))
      
      # frenetFramesGlobalCoordinate[,,i]<-as.SO3(rbind(t_vec,n_vec,b_perb_vec))
      frenetFramesGlobalCoordinate[,,i]<-rbind(t_vec,n_vec,b_perb_vec)
      
    }else{
      
      frenetFramesGlobalCoordinate[,,i]<-materialFramesGlobalCoordinate[,,i-1]
    }
  }
  frenetFramesGlobalCoordinate[,,numberOfFrames]<-materialFramesGlobalCoordinate[,,numberOfFrames]
  
  #parent is the local twisting frame
  frenetFramesBasedOnLocalmaterialFrame<-array(NA,dim = c(3,3,numberOfFrames))
  for (i in 1:numberOfFrames) {
    frenetFramesBasedOnLocalmaterialFrame[,,i]<-.rotateFrameToMainAxes_standard(myFrame = materialFramesGlobalCoordinate[,,i],
                                                                                vectors2rotate = frenetFramesGlobalCoordinate[,,i])
  }
  
  #parent is the previous twisting frame
  frenetFramesBasedOnParents<-array(NA,dim = c(3,3,numberOfFrames))
  frenetFramesBasedOnParents[,,1]<-materialFramesGlobalCoordinate[,,1]
  for (i in 2:numberOfFrames) {
    frenetFramesBasedOnParents[,,i]<-.rotateFrameToMainAxes_standard(myFrame = materialFramesGlobalCoordinate[,,i-1],
                                                                     vectors2rotate = frenetFramesGlobalCoordinate[,,i])
  }
  
  #normal vectors
  normalVectorsGlobalCoordinate<-t(frenetFramesGlobalCoordinate[2,,])
  
  # theta is positive and equal to d_g(a_i,n_i)
  theta_angles<-rep(NA,numberOfFrames)
  for (i in 1:numberOfFrames) {
    theta_angles[i]<-.calculate_theta(materialFrameBasedOnParent = materialFramesBasedOnParents[,,i])
  }
  
  phi_angles_bend<-rep(NA,numberOfFrames)
  for (i in 1:numberOfFrames) {
    u1<-materialFramesBasedOnParents[1,,i]
    u2<-c(1,0,0)
    phi_angles_bend[i]<-.geodesicDistance(u1,u2)
  }
  
  psi_angles_roll<-rep(NA,numberOfFrames)
  for (i in 1:numberOfFrames) {
    psi_angles_roll[i]<-RSpincalc::DCM2EA(materialFramesBasedOnParents[,,i],
                                          EulerOrder = 'zyx')[3]
  }
  
  #r_project is the projection of the CS on the normal
  r_project_lengths<-rep(NA,numberOfFrames)
  for (i in 1:numberOfFrames) {
    r_project_lengths[i]<-.calculate_r_project_Length(a = ellipseRadii_a[i],
                                                      b = ellipseRadii_b[i],
                                                      theta = theta_angles[i])
  }
  
  #r_max is the extension of r_project to the intersection with the previous slicing plane
  r_max_lengths<-abs(connectionsLengths/sin(phi_angles_bend))
  r_max_lengths[is.infinite(r_max_lengths) | is.na(r_max_lengths)]<-max(r_project_lengths)+1
  r_max_lengths[1]<-max(r_project_lengths)+1
  r_max_lengths[length(r_max_lengths)]<-max(r_project_lengths)+1
  
  tip_r_ProjectVectors<-array(NA,dim = dim(normalVectorsGlobalCoordinate))
  tip_r_MaxVectors<-array(NA,dim = dim(normalVectorsGlobalCoordinate))
  for (i in 1:numberOfFrames) {
    u<-normalVectorsGlobalCoordinate[i,]
    tip_r_ProjectVectors[i,]<-r_project_lengths[i]*u+spinalPoints3D[i,]
    tip_r_MaxVectors[i,]<-r_max_lengths[i]*u+spinalPoints3D[i,]
  }
  
  ##################################################################
  # cross-sections
  
  ellipseTemplate_2D<-.ellipsoidGenerator_2D_2(center = c(0,0),
                                               a = ellipseRadii_a[1],
                                               b = ellipseRadii_b[1],
                                               n = ellipseResolution,
                                               n2 = 1)
  
  ellipses_2D<-array(NA,c(dim(ellipseTemplate_2D),numberOfFrames))
  for (i in 1:numberOfFrames) {
    ellipses_2D[,,i]<-.ellipsoidGenerator_2D_2(center = c(0,0),
                                               a = ellipseRadii_a[i],
                                               b = ellipseRadii_b[i],
                                               n = ellipseResolution,
                                               n2 = 1)
  }
  
  slicingEllipsoids<-array(NA,dim = c(nrow(ellipseTemplate_2D),3,nrow(spinalPoints3D)))
  for (i in 1:numberOfFrames) {
    
    ellipseIn3DTemp<-cbind(rep(0,nrow(ellipses_2D[,,i])),ellipses_2D[,,i])
    
    #rotate ellipses with rotation matrices materialFramesGlobalCoordinate
    elipseIn3D<-ellipseIn3DTemp%*%materialFramesGlobalCoordinate[,,i]
    # translate
    elipseIn3D<-elipseIn3D+matrix(rep(spinalPoints3D[i,],nrow(elipseIn3D)),ncol = 3,byrow = TRUE)
    
    slicingEllipsoids[,,i]<-elipseIn3D
  }
  
  boundaryPoints<-matrix(base::aperm(slicingEllipsoids, c(1, 3, 2)), ncol = 3)
  
  
  #plot
  numberOfLayers<-20
  skeletalSheetPoints<-array(NA,dim = c(numberOfLayers*2+1,3,dim(slicingEllipsoids)[3]))
  for (i in 1:dim(slicingEllipsoids)[3]) {
    skeletalSheetPoints[,,i]<-.generatePointsBetween2Points(slicingEllipsoids[1,,i],
                                                            slicingEllipsoids[ellipseResolution*2+1,,i],
                                                            numberOfPoints = numberOfLayers*2+1)
  }
  
  #plot
  if(plotting==TRUE){
    
    .etrep_open3d(show_widget = TRUE)   
    
    for (i in 1:dim(slicingEllipsoids)[3]) {
      rgl::plot3d(rbind(slicingEllipsoids[,,i],slicingEllipsoids[1,,i]),type = 'l',col='black',expand = 10,box=FALSE,add = TRUE)
    }
    for (i in 1:dim(slicingEllipsoids)[1]) {
      rgl::plot3d(t(slicingEllipsoids[i,,]),type = 'l',col='blue',expand = 10,box=FALSE,add = TRUE)
    }
    #skeletal sheet
    for (i in 1:dim(skeletalSheetPoints)[1]) {
      rgl::plot3d(t(skeletalSheetPoints[i,,]),type = 'l',lwd=1,col='blue',expand = 10,box=FALSE,add = TRUE)
    }
    for (i in 1:dim(skeletalSheetPoints)[3]) {
      rgl::plot3d(skeletalSheetPoints[,,i],type = 'l',lwd=1,col='blue',expand = 10,box=FALSE,add = TRUE)
    }
    # # critical vectors
    # for (i in 1:dim(slicingEllipsoids)[3]) {
    #   rgl::plot3d(rbind(spinalPoints3D[i,],normalsTips_at_CS_boundaries[i,]),type = 'l',lwd=4,col='orange',expand = 10,box=FALSE,add = TRUE)
    #   # rgl::plot3d(rbind(colMeans(slicingEllipsoids[,,i]),normalsTips_at_CS_boundaries[i,]),type = 'l',lwd=4,col='red',expand = 10,box=FALSE,add = TRUE)
    # }
    for (i in 1:dim(slicingEllipsoids)[3]) {
      rgl::plot3d(rbind(spinalPoints3D[i,],tip_r_ProjectVectors[i,]),type = 'l',lwd=3.2,col='red',expand = 10,box=FALSE,add = TRUE)
      # rgl::plot3d(rbind(spinalPoints3D[i,],tip_r_MaxVectors[i,]),type = 'l',lwd=2,col='orange',expand = 10,box=FALSE,add = TRUE)
    }
    #frames
    matlib::vectors3d(spinalPoints3D+t(materialFramesGlobalCoordinate[1,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="blue", lwd=2)
    matlib::vectors3d(spinalPoints3D+t(materialFramesGlobalCoordinate[2,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="red", lwd=2)
    matlib::vectors3d(spinalPoints3D+t(materialFramesGlobalCoordinate[3,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="green", lwd=2)
    
    #matlib::vectors3d(spinalPoints3D+t(frenetFramesGlobalCoordinate[1,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="black", lwd=2)
    matlib::vectors3d(spinalPoints3D+t(frenetFramesGlobalCoordinate[2,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="black", lwd=2)
    #matlib::vectors3d(spinalPoints3D+t(frenetFramesGlobalCoordinate[3,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="black", lwd=2)
    
    decorate3d()
    
    ## if we are using NULL device (macOS), open browser
    if (getOption("rgl.useNULL", FALSE)) {
      .etrep_show3d()                       # <-- opens the plot in browser
    }
  }
  
  out<-list("spinalPoints3D"=spinalPoints3D,
            "materialFramesBasedOnParents"=materialFramesBasedOnParents,
            "materialFramesGlobalCoordinate"=materialFramesGlobalCoordinate,
            "frenetFramesGlobalCoordinate"=frenetFramesGlobalCoordinate,
            "frenetFramesBasedOnParents"=frenetFramesBasedOnParents,
            "ellipseRadii_a"=ellipseRadii_a,
            "ellipseRadii_b"=ellipseRadii_b,
            "connectionsLengths"=connectionsLengths,
            "theta_angles"=theta_angles,
            "phi_angles_bend"=phi_angles_bend,
            "psi_angles_roll"=psi_angles_roll,
            "r_project_lengths"=r_project_lengths,
            "tip_r_ProjectVectors"=tip_r_ProjectVectors,
            "r_max_lengths"=r_max_lengths,
            "tip_r_MaxVectors"=tip_r_MaxVectors,
            "slicingEllipsoids"=slicingEllipsoids,
            "boundaryPoints"=boundaryPoints,
            "skeletalSheetPoints"=skeletalSheetPoints)
}

#' Create surface mesh of a tube
#' @param tube List containing ETRep details.
#' @param meshType String, either "quadrilateral" or "triangular" definig the type of mesh.
#' @param plotMesh Logical, enables plotting of the mesh (default is TRUE).
#' @param color String, defining the color of the mesh (default is 'blue').
#' @param decorate Logical, enables decorating the plot (default is TRUE).
#' @return An object from rgl::mesh3d class
#' @examples
#' \dontrun{
#' quad_mesh<-tube_Surface_Mesh(tube = ETRep::tube_B, 
#'                              meshType = "quadrilateral", 
#'                              plotMesh = TRUE, 
#'                              decorate = TRUE, 
#'                              color = "orange")
#' # draw wireframe of the mesh
#' rgl::wire3d(quad_mesh, color = "black", lwd = 1)   # add wireframe
#' # Display in browser
#' ETRep:::.etrep_show3d(width = 800, height = 600)
#' 
#' tri_mesh<-tube_Surface_Mesh(tube = ETRep::tube_B, 
#'                             meshType = "triangular", 
#'                             plotMesh = TRUE, 
#'                             decorate = TRUE, 
#'                             color = "green")
#' # draw wireframe of the mesh
#' rgl::wire3d(tri_mesh, color = "black", lwd = 1)   # add wireframe
#' # Display in browser
#' ETRep:::.etrep_show3d(width = 800, height = 600)
#'  }
#' @export
tube_Surface_Mesh <- function(tube,
                              meshType="quadrilateral",
                              plotMesh=TRUE,
                              color='blue',
                              decorate=TRUE) {
  
  slicingEllipsoids<-tube$slicingEllipsoids
  spinePoints<-tube$spinalPoints3D
  
  N <- dim(slicingEllipsoids)[3]  # Number of ellipses
  M <- dim(slicingEllipsoids)[1]  # Number of points per ellipse
  
  # Compute vertices (M * N rows)
  vertices_list <- lapply(1:N, function(j) slicingEllipsoids[, , j])
  vertices <- do.call(rbind, vertices_list)
  vertices <- t(cbind(vertices, rep(1, nrow(vertices)))) 
  
  # Build quad faces
  quads <- matrix(0, nrow = 4, ncol = M * (N - 1))
  face_idx <- 1
  for (j in 1:(N - 1)) {
    for (i in 1:M) {
      i_next <- if (i < M) i + 1 else 1
      v1 <- (j - 1) * M + i
      v2 <- (j - 1) * M + i_next
      v3 <- j * M + i_next
      v4 <- j * M + i
      quads[, face_idx] <- c(v1, v2, v3, v4)
      face_idx <- face_idx + 1
    }
  }
  
  quad_mesh <- rgl::qmesh3d(vertices = vertices,
                            indices = quads)
  
  if(meshType=="quadrilateral"){
    if(plotMesh==TRUE){
      .etrep_open3d(show_widget = TRUE) 
      rgl::shade3d(quad_mesh,color=color)
      if(decorate==TRUE){
        rgl::decorate3d()
      }
      ## if we are using NULL device (macOS), open browser
      if (getOption("rgl.useNULL", FALSE)) {
        .etrep_show3d()                       # <-- opens the plot in browser
      }
    }
    return(quad_mesh)
    
  }else if(meshType=="triangular"){
    
    # Extract quad faces and vertex buffer
    quads <- quad_mesh$ib
    vb <- quad_mesh$vb
    
    n_quads <- ncol(quads)
    triangles <- matrix(0, nrow = 3, ncol = 2 * n_quads)
    
    for (i in 1:n_quads) {
      v1 <- quads[1, i]
      v2 <- quads[2, i]
      v3 <- quads[3, i]
      v4 <- quads[4, i]
      
      # Triangle 1: v1, v2, v3
      triangles[, 2 * i - 1] <- c(v1, v2, v3)
      
      # Triangle 2: v1, v3, v4
      triangles[, 2 * i] <- c(v1, v3, v4)
    }
    
    # Create a triangle mesh
    tri_mesh<-rgl::tmesh3d(vertices = vb, indices = triangles, homogeneous = TRUE)
    
    if(plotMesh==TRUE){
      .etrep_open3d(show_widget = TRUE) 
      rgl::shade3d(tri_mesh,color=color)
      if(decorate==TRUE){
        rgl::decorate3d()
      }
      ## if we are using NULL device (macOS), open browser
      if (getOption("rgl.useNULL", FALSE)) {
        .etrep_show3d()                       # <-- opens the plot in browser
      }
    }
    
    return(tri_mesh)
    
  }else{
    stop("Please choose quadrilateral or triangular for meshType!")
  }
  
}


# Plotting an ETRep
#' Plot an Elliptical Tube (ETRep)
#'
#' Plots a given ETRep with options for boundary, material frames, and projection visualization.
#'
#' @param tube List containing ETRep details.
#' @param plot_boundary Logical, enables plotting of the boundary (default is TRUE).
#' @param plot_r_max Logical, enables plotting of max projection size (default is FALSE).
#' @param plot_r_project Logical, enables plotting of projection along normals (default is TRUE).
#' @param plot_frames Logical, enables plotting of the material frames (default is TRUE).
#' @param plot_spine Logical, enables plotting of the spine.
#' @param plot_normal_vec Logical, enables plotting of the normals.
#' @param plot_skeletal_sheet Logical, enables plotting of the surface skeleton.
#' @param decorate Logical, enables decorate the plot
#' @param add Logical, enables overlay plotting
#' @param frameScaling Numeric, scale factor for frames.
#' @param colSkeletalSheet String, defining the color of the surface skeleton
#' @param colorBoundary String, defining the color of the e-tube
#' @return Graphical output.
#' @examples
#' # Load tube
#' data("colon3D")
#' \dontrun{
#' plot_Elliptical_Tube(tube = colon3D,
#'                      plot_frames = FALSE)
#'  }
#' @export
plot_Elliptical_Tube <- function(tube,
                                 plot_boundary=TRUE,
                                 plot_r_max=FALSE,
                                 plot_r_project=TRUE,
                                 plot_frames=TRUE,
                                 frameScaling=NA,
                                 plot_spine=TRUE,
                                 plot_normal_vec=FALSE,
                                 plot_skeletal_sheet=TRUE,
                                 decorate=TRUE,
                                 colSkeletalSheet="blue",
                                 colorBoundary="blue",
                                 add=FALSE) {

  numberOfFrames<-nrow(tube$spinalPoints3D)

  spinalPoints3D<-tube$spinalPoints3D
  materialFramesBasedOnParents<-tube$materialFramesBasedOnParents
  materialFramesGlobalCoordinate<-tube$materialFramesGlobalCoordinate
  frenetFramesBasedOnParents<-tube$frenetFramesBasedOnParents
  frenetFramesGlobalCoordinate<-tube$frenetFramesGlobalCoordinate
  ellipseRadii_a<-tube$ellipseRadii_a
  ellipseRadii_b<-tube$ellipseRadii_b
  slicingEllipsoids<-tube$slicingEllipsoids
  skeletalSheetPoints<-tube$skeletalSheetPoints
  r_project_lengths<-tube$r_project_lengths
  tip_r_MaxVectors<-tube$tip_r_MaxVectors
  tip_r_ProjectVectors<-tube$tip_r_ProjectVectors
  connectionsLengths<-tube$connectionsLengths


  if(length(connectionsLengths)==(numberOfFrames-1)){
    connectionsLengths<-c(0,connectionsLengths)
  }

  if(is.na(frameScaling)){
    frameScaling<-mean(connectionsLengths)/2
  }


  ## ---------------------------------------------------------
  ## Open device
  ## ---------------------------------------------------------
  if(!add){
    .etrep_open3d(show_widget = FALSE) 
  }
  
  ## ---------------------------------------------------------
  ## Drawing code
  ## ---------------------------------------------------------
  #spine
  if(plot_spine==TRUE){
    rgl::plot3d(spinalPoints3D,type = 'l',expand = 10,box=FALSE,add = TRUE)
  }
  #boundary
  if(plot_boundary==TRUE){
    for (i in 1:dim(slicingEllipsoids)[3]) {
      rgl::plot3d(rbind(slicingEllipsoids[,,i],slicingEllipsoids[1,,i]),type = 'l',col=colorBoundary,expand = 10,box=FALSE,add = TRUE)
    }
    for (i in 1:dim(slicingEllipsoids)[1]) {
      rgl::plot3d(t(slicingEllipsoids[i,,]),type = 'l',col=colorBoundary,expand = 10,box=FALSE,add = TRUE)
    }
  }
  #skeletal sheet
  if(plot_skeletal_sheet==TRUE){
    for (i in 1:dim(skeletalSheetPoints)[1]) {
      rgl::plot3d(t(skeletalSheetPoints[i,,]),type = 'l',lwd=1,col=colSkeletalSheet,expand = 10,box=FALSE,add = TRUE)
    }
    for (i in 1:dim(skeletalSheetPoints)[3]) {
      rgl::plot3d(skeletalSheetPoints[,,i],type = 'l',lwd=1,col=colSkeletalSheet,expand = 10,box=FALSE,add = TRUE)
    }
  }
  # # critical vectors
  # for (i in 1:dim(slicingEllipsoids)[3]) {
  #   rgl::plot3d(rbind(spinalPoints3D[i,],criticalVectorsTip[i,]),type = 'l',lwd=4,col='orange',expand = 10,box=FALSE,add = TRUE)
  #   # rgl::plot3d(rbind(colMeans(slicingEllipsoids[,,i]),criticalVectorsTip[i,]),type = 'l',lwd=4,col='red',expand = 10,box=FALSE,add = TRUE)
  # }

  if(plot_r_project==TRUE){
    for (i in 1:dim(slicingEllipsoids)[3]) {
      rgl::plot3d(rbind(spinalPoints3D[i,],tip_r_ProjectVectors[i,]),type = 'l',lty = 3,lwd=3.2,col='red',expand = 10,box=FALSE,add = TRUE)
      # rgl::plot3d(rbind(spinalPoints3D[i,],tip_r_MaxVectors[i,]),type = 'l',lwd=2,col='orange',expand = 10,box=FALSE,add = TRUE)
    }
  }

  if(plot_r_max==TRUE){
    for (i in 1:dim(slicingEllipsoids)[3]) {
      rgl::plot3d(rbind(spinalPoints3D[i,],tip_r_MaxVectors[i,]),type = 'l',lwd=2,col='orange',expand = 10,box=FALSE,add = TRUE)
    }
  }

  if(plot_frames==TRUE){
    rgl::plot3d(spinalPoints3D,type = 'l',lwd=4,col='darkblue',expand = 10,box=FALSE,add = TRUE)
    #frames
    matlib::vectors3d(spinalPoints3D+frameScaling*t(materialFramesGlobalCoordinate[1,,]),origin = spinalPoints3D,headlength = 0.1*frameScaling,radius = frameScaling/10, col="blue", lwd=frameScaling)
    matlib::vectors3d(spinalPoints3D+frameScaling*t(materialFramesGlobalCoordinate[2,,]),origin = spinalPoints3D,headlength = 0.1*frameScaling,radius = frameScaling/10, col="red", lwd=frameScaling)
    matlib::vectors3d(spinalPoints3D+frameScaling*t(materialFramesGlobalCoordinate[3,,]),origin = spinalPoints3D,headlength = 0.1*frameScaling,radius = frameScaling/10, col="green", lwd=frameScaling)
  }

  if(plot_normal_vec==TRUE){
    #matlib::vectors3d(spinalPoints3D+frameScaling*t(frenetFramesGlobalCoordinate[1,,]),origin = spinalPoints3D,headlength = 0.1*frameScaling,radius = frameScaling/10, col="black", lwd=frameScaling)
    matlib::vectors3d(spinalPoints3D+frameScaling*t(frenetFramesGlobalCoordinate[2,,]),origin = spinalPoints3D,headlength = 0.1*frameScaling,radius = frameScaling/10, col="black", lwd=frameScaling)
    #matlib::vectors3d(spinalPoints3D+frameScaling*t(frenetFramesGlobalCoordinate[3,,]),origin = spinalPoints3D,headlength = 0.1*frameScaling,radius = frameScaling/10, col="black", lwd=frameScaling)
  }
  
  ## ---------------------------------------------------------
  ## If we’re using the NULL device (i.e. macOS), open browser
  ## ---------------------------------------------------------
  if(getOption("rgl.useNULL", FALSE)){
    .etrep_show3d()
  }

}

# Converting a tube to a vector in the high-dimensional space with hyperbolic boundary
#' @keywords internal
.tube2vectorsIn6DHyperbola <- function(tube) {

  materialFramesBasedOnParents<-tube$materialFramesBasedOnParents

  # projection of the tangent vectors to the plane of the previous frames
  tangentVectorsBasedOnParentFrames<-t(materialFramesBasedOnParents[1,,])
  v_vectors<-tangentVectorsBasedOnParentFrames[,2:3]
  psi_angles_roll<-tube$psi_angles_roll
  connectionsLengths<-tube$connectionsLengths
  ellipseRadii_a<-tube$ellipseRadii_a
  ellipseRadii_b<-tube$ellipseRadii_b

  vectorsIn6DHyperbola<-cbind(v_vectors,
                              psi_angles_roll,
                              as.vector(connectionsLengths),
                              as.vector(ellipseRadii_a),
                              as.vector(ellipseRadii_b))

  return(vectorsIn6DHyperbola)
}

# Converting a tube to a vector in the high-dimensional convex-space
#' @keywords internal
.tube2vectorsIn6DCylinder <- function(tube) {

  vectorsIn6DHyperbola_tubes<-.tube2vectorsIn6DHyperbola(tube = tube)

  vectorsIn6DCylinder_tubes<-t(apply(vectorsIn6DHyperbola_tubes,
                                     MARGIN = 1,
                                     FUN = .map6DhyperbolaTo6Dcylinder))

  return(vectorsIn6DCylinder_tubes)
}

# Converting the 6D hyperbola (i.e., space of a cross-section) to a convex 6D cylinder based on the swept skeletal coordinate system
#' @keywords internal
.map6DhyperbolaTo6Dcylinder <- function(v_gamma_x_a_b) {

  #gamma is the role angle

  v_In6DHyperbola<-v_gamma_x_a_b[c(1,2)]
  gamma<-v_gamma_x_a_b[3]
  x<-v_gamma_x_a_b[4]
  a<-v_gamma_x_a_b[5]
  b<-v_gamma_x_a_b[6]

  # calculate tangent vector
  t_vec<-.convert_v_to_TangentVector(v = v_In6DHyperbola)

  # calculate yaw and pitch angles as alpha and beta
  c2s<-.cartesian_to_spherical(v = t_vec)
  alpha<-c2s$phi
  beta<-c2s$theta
  materialFrame<-RSpincalc::EA2DCM(EA = c(alpha, beta, gamma),
                                   EulerOrder = 'zyx')

  #calculate theta
  theta<-.calculate_theta(materialFrameBasedOnParent=materialFrame)

  # calculate r_project length
  r_project<-.calculate_r_project_Length(a = a, b = b, theta = theta)

  # maximum possible value of r inside the unit circle
  v_vec_length<-norm(v_In6DHyperbola,type = '2')
  #i.e., if r_project<x then we can bend the frame up to pi/2 degree
  if(r_project<=x){
    max_v_vec_length<-1
  }else{
    max_v_vec_length<-(x/r_project)
  }

  # maximum possible value of r
  ratio_v_vec_length<-v_vec_length/max_v_vec_length

  if(v_vec_length==0){
    v_In6DCylinder<-c(0,0)
  }else{
    v_In6DCylinder<-ratio_v_vec_length*.convertVec2unitVec(v_In6DHyperbola)
  }

  sweptSkeletalCoordinate<-c(v_In6DCylinder,gamma,x,a,b)

  return(sweptSkeletalCoordinate)

}

# Converting the convex 6D cylinder to 6D hyperbola (i.e., space of a cross-section) based on the swept skeletal coordinate system
#' @keywords internal
.map6DcylinderTo6Dhyperbola <-function(v_gamma_x_a_b_In6DCylinder) {

  v_In6DCylinder<-v_gamma_x_a_b_In6DCylinder[c(1,2)]
  gamma<-v_gamma_x_a_b_In6DCylinder[3]
  x<-v_gamma_x_a_b_In6DCylinder[4]
  a<-v_gamma_x_a_b_In6DCylinder[5]
  b<-v_gamma_x_a_b_In6DCylinder[6]

  # if(a<b){
  #   stop("Radius a must be grater than radius b!")
  # }

  # tangent vector
  t_vec<-.convert_v_to_TangentVector(v = v_In6DCylinder)

  # calculate yaw and pitch angles as alpha and beta
  c2s<-.cartesian_to_spherical(v = t_vec)
  alpha<-c2s$phi
  beta<-c2s$theta
  materialFrame<-RSpincalc::EA2DCM(EA = c(alpha, beta, gamma),
                                   EulerOrder = 'zyx')

  #calculate theta
  theta<-.calculate_theta(materialFrameBasedOnParent=materialFrame)

  #length of the critical vector
  r_project<-.calculate_r_project_Length(a = a,b = b,theta = theta)

  # maximum possible value of r
  # i.e., if r_project<x then we can bend the frame up to pi/2 degree
  if(r_project<=x){
    max_v_vec_length<-1
  }else{
    max_v_vec_length<-(x/r_project)
  }

  v_In6DHyperbola<-v_In6DCylinder*max_v_vec_length

  if(sum(is.nan(v_In6DHyperbola))>0 | anyNA(v_In6DHyperbola)){
    v_In6DHyperbola<-c(0,0)
  }

  v_gamma_x_a_b<-c(v_In6DHyperbola,gamma,x,a,b)

  return(v_gamma_x_a_b)
}

# Convert an elemnt of the high-dimensional non-convex space to an ETRep
#' @keywords internal
.convertMatrixIn6DHyperbola2Tube <- function(matrixIn6DHyperbola) {

  numberOfFrames<-dim(matrixIn6DHyperbola)[1]

  # unit tangents
  unitTangentBasedOnParents<-t(apply(matrixIn6DHyperbola[,c(1,2)],
                                     MARGIN = 1,
                                     FUN = .convert_v_to_TangentVector))

  gammas<-matrixIn6DHyperbola[,3]

  materialFramesBasedOnParents<-array(NA,dim=c(3,3,numberOfFrames))
  for (i in 1:numberOfFrames) {
    # calculate material frames by calculate yaw and pitch angles as alpha and beta
    t_vec<-unitTangentBasedOnParents[i,]
    c2s<-.cartesian_to_spherical(v = t_vec)
    alphaTemp<-c2s$phi
    betaTemp<-c2s$theta
    gammaTemp<-gammas[i]
    materialFramesBasedOnParents[,,i]<-RSpincalc::EA2DCM(EA = c(alphaTemp, betaTemp, gammaTemp),
                                                         EulerOrder = 'zyx')
  }

  #connections' lengths steps
  connectionsLengths<-matrixIn6DHyperbola[,4]

  #radii steps
  ellipseRadii_a<-matrixIn6DHyperbola[,5]
  ellipseRadii_b<-matrixIn6DHyperbola[,6]

  tube<-create_Elliptical_Tube(numberOfFrames = numberOfFrames,
                               method = "basedOnMaterialFrames",
                               materialFramesBasedOnParents = materialFramesBasedOnParents,
                               ellipseRadii_a = ellipseRadii_a,
                               ellipseRadii_b = ellipseRadii_b,
                               connectionsLengths = connectionsLengths,
                               plotting = FALSE)
  return(tube)

}

# convert p-values to color based on spectrum=rev(c("white","lightcyan","cyan","lightblue","darkblue"))
#' @keywords internal
.pvalue_to_color <- function(p_value,
                             range=100,
                             spectrum=rev(c("white","lightcyan","cyan","lightblue","darkblue")),
                             plotSpectrum=FALSE) {

  colfunc <- colorRampPalette(spectrum)
  colors <- colfunc(range)

  if(plotSpectrum==TRUE){
    image.plot(legend.only = TRUE,
               zlim = c(0, 1),
               col = colors,
               legend.lab = "p-value",
               horizontal = FALSE)
  }
  color_index <- round(p_value * (range-1)) + 1  # Scale p-value to range

  return(colors[color_index])
}

# Calculating the intrinsic mean ETRep
#' Calculate Intrinsic Mean of ETReps
#'
#' Computes the intrinsic mean of a set of ETReps. The computation involves transforming the non-convex hypertrumpet space into a convex space, calculating the mean in this transformed space, and mapping the result back to the original hypertrumpet space.
#'
#' @param tubes List of ETReps.
#' @param type String, "ShapeAnalysis" or "sizeAndShapeAnalysis" (default is "sizeAndShapeAnalysis").
#' @param plotting Logical, enables visualization of the mean (default is TRUE).
#' @return List representing the mean ETRep.
#' @references
#' Taheri, M., Pizer, S. M., & Schulz, J. (2024). "The Mean Shape under the Relative Curvature Condition." arXiv.
#' \doi{10.48550/arXiv.2404.01043}
#'
#' Taheri Shalmani, M. (2024). "Shape Statistics via Skeletal Structures." University of Stavanger.
#' \doi{10.13140/RG.2.2.34500.23685}
#'
#' @examples
#' #Example 1
#' # Load tubes
#' data("tube_A")
#' data("tube_B")
#' intrinsic_mean<-
#'   intrinsic_mean_tube(tubes = list(tube_A,tube_B),
#'                       plotting = FALSE)
#' # Plotting
#' \dontrun{
#' plot_Elliptical_Tube(tube = intrinsic_mean,
#'                      plot_frames = FALSE,
#'                      plot_skeletal_sheet = FALSE,
#'                      plot_r_project = FALSE,
#'                      plot_r_max = FALSE,
#'                      add = FALSE)
#'  }
#'
#' #Example 2
#' data("simulatedColons")
#' intrinsic_mean<-
#'   intrinsic_mean_tube(tubes = simulatedColons,
#'                       plotting = FALSE)
#' # Plotting
#' \dontrun{
#' plot_Elliptical_Tube(tube = intrinsic_mean,
#'                      plot_frames = FALSE,
#'                      plot_skeletal_sheet = FALSE,
#'                      plot_r_project = FALSE,
#'                      plot_r_max = FALSE,
#'                      add = FALSE)
#'  }
#' @export
intrinsic_mean_tube <- function(tubes,
                                type="sizeAndShapeAnalysis",
                                plotting=TRUE) {

  numberOftubes<-length(tubes)

  if(type=="sizeAndShapeAnalysis"){

    # sort in a tensor
    vectorsIn6DCylinder_tubes<-array(NA,dim = c(dim(.tube2vectorsIn6DCylinder(tubes[[1]])),numberOftubes))
    for (i in 1:numberOftubes) {
      vectorsIn6DCylinder_tubes[,,i]<-.tube2vectorsIn6DCylinder(tube = tubes[[i]])
    }

    #mean along the third dimension of the array
    meanMatrix_In_6DCylinder<-apply(vectorsIn6DCylinder_tubes, c(1, 2), mean)

    meanMatrixIn6DHyperbola<-t(apply(meanMatrix_In_6DCylinder,
                                     MARGIN = 1,
                                     FUN = .map6DcylinderTo6Dhyperbola))

    #convert mean matrix to a tube
    meantube<-.convertMatrixIn6DHyperbola2Tube(matrixIn6DHyperbola=meanMatrixIn6DHyperbola)


  }else if(type=="shapeAnalysis"){

    scaledTubes<-list()
    for (i in 1:numberOftubes) {
      scaledTubes[[i]]<-.scaleETRepToHaveTheSizeAsOne(tube = tubes[[i]])
    }

    # sort in a tensor
    vectorsIn6DCylinder_tubes<-array(NA,dim = c(dim(.tube2vectorsIn6DCylinder(scaledTubes[[1]])),numberOftubes))
    for (i in 1:numberOftubes) {
      vectorsIn6DCylinder_tubes[,,i]<-.tube2vectorsIn6DCylinder(tube = scaledTubes[[i]])
    }

    #mean along the third dimension of the array
    meanMatrix_In_6DCylinder<-apply(vectorsIn6DCylinder_tubes, c(1, 2), mean)

    meanMatrixIn6DHyperbola<-t(apply(meanMatrix_In_6DCylinder,
                                     MARGIN = 1,
                                     FUN = .map6DcylinderTo6Dhyperbola))

    #convert mean matrix to a tube
    meantube<-.convertMatrixIn6DHyperbola2Tube(matrixIn6DHyperbola=meanMatrixIn6DHyperbola)

  }else{
    stop("Please choose type as sizeAndShapeAnalysis or shapeAnalysis !")
  }

  if(plotting==TRUE){
    plot_Elliptical_Tube(meantube,
                         plot_frames = FALSE)
  }

  return(meantube)

}

# ETRep Euclideanization
#' Convert an ETRep to a Matrix in the Convex Transformed Space.
#'
#' @param tube A list containing the details of the ETRep.
#' @return An n*6 matrix, where n is the number of spinal points, representing the ETRep in the transformed Euclidean convex space.
#' @examples
#' #Example
#' # Load tube
#' data("tube_A")
#' Euclideanized_Tube<- elliptical_Tube_Euclideanization(tube = tube_A)
#'
#' @export
elliptical_Tube_Euclideanization <- function(tube) {

  Euclideanized_Tube<-.tube2vectorsIn6DCylinder(tube = tube)
  colnames(Euclideanized_Tube) <- c("v_1", "v_2", "psi","x","a","b")

  return(Euclideanized_Tube)
}


#' Calculating the intrinsic distance between two ETReps
#' @param tube1 List containing ETRep details.
#' @param tube2 List containing ETRep details.
#' @return Numeric
#' @references
#' Taheri, M., Pizer, S. M., & Schulz, J. (2024). "The Mean Shape under the Relative Curvature Condition." arXiv.
#' \doi{10.48550/arXiv.2404.01043}
#'
#' Taheri Shalmani, M. (2024). "Shape Statistics via Skeletal Structures." University of Stavanger.
#' \doi{10.13140/RG.2.2.34500.23685}
#'
#' @examples
#' # Load tubes
#' data("tube_A")
#' data("tube_B")
#' intrinsic_Distance_Between2tubes(tube1 = tube_A,tube2 = tube_B)
#' @export
intrinsic_Distance_Between2tubes <- function(tube1, tube2) {

  if(dim(tube1$frenetFramesBasedOnParents)[3]!=dim(tube2$frenetFramesBasedOnParents)[3]){
    stop('Number of cross-sections are not the same!')
  }

  vectorsIn6DCylinder_tube1<-.tube2vectorsIn6DCylinder(tube = tube1)
  vectorsIn6DCylinder_tube2<-.tube2vectorsIn6DCylinder(tube = tube2)

  euclideanDistance<-sum(sqrt(rowSums(vectorsIn6DCylinder_tube1-
                                        vectorsIn6DCylinder_tube2)^2))

  return(euclideanDistance)

}

#' Calculating the non-intrinsic distance between two ETReps
#' @param tube1 List containing ETRep details.
#' @param tube2 List containing ETRep details.
#' @return Numeric
#' @references
#' Taheri, M., Pizer, S. M., & Schulz, J. (2024). "The Mean Shape under the Relative Curvature Condition." arXiv.
#' \doi{10.48550/arXiv.2404.01043}
#'
#' Taheri Shalmani, M. (2024). "Shape Statistics via Skeletal Structures." University of Stavanger.
#' \doi{10.13140/RG.2.2.34500.23685}
#'
#' @examples
#' # Load tubes
#' data("tube_A")
#' data("tube_B")
#' intrinsic_Distance_Between2tubes(tube1 = tube_A,tube2 = tube_B)
#' @export
nonIntrinsic_Distance_Between2tubes<- function(tube1,tube2) {

  if(dim(tube1$materialFramesBasedOnParents)[3]!=dim(tube2$materialFramesBasedOnParents)[3]){
    stop('Number of cross-sections are not the same!')
  }

  numberOfFrames<-dim(tube1$materialFramesBasedOnParents)[3]


  distancesMaterialFrames<-rep(NA,numberOfFrames)
  for (i in 1:numberOfFrames) {
    # q1_twist<-as.vector(rotations::as.Q4(rotations::as.SO3(tube1$materialFramesBasedOnParents[,,i])))
    # q2_twist<-as.vector(rotations::as.Q4(rotations::as.SO3(tube2$materialFramesBasedOnParents[,,i])))
    # distancesMaterialFrames[i]<-.geodesicDistance(q1_twist,q2_twist)
    distancesMaterialFrames[i]<-rot.dist(rotations::as.SO3(tube1$materialFramesBasedOnParents[,,i]),
                                         rotations::as.SO3(tube2$materialFramesBasedOnParents[,,i]),
                                         method="intrinsic")
  }

  distancesConnectionsLengths<-abs(log(tube1$connectionsLengths)-log(tube2$connectionsLengths))
  distancesConnectionsLengths<-distancesConnectionsLengths[!is.na(distancesConnectionsLengths)]


  distancesRadii_a<-abs(log(tube1$ellipseRadii_a)-log(tube2$ellipseRadii_a))
  distancesRadii_b<-abs(log(tube1$ellipseRadii_b)-log(tube2$ellipseRadii_b))

  totalDistance<-sqrt(sum(distancesMaterialFrames^2)+
                        sum(distancesConnectionsLengths^2)+
                        sum(distancesRadii_a^2)+
                        sum(distancesRadii_b^2))

  return(totalDistance)
}

# Calculating an intrinsic discrete path between two elements of the 6D non-convex space
#' @keywords internal
.discretePathBetween2PointsIn_6D_Hyperbola <- function(point1,
                                                       point2,
                                                       numberOfpoints=10) {

  point1_SweptCoordinate<-.map6DhyperbolaTo6Dcylinder(v_gamma_x_a_b = point1)
  point2_SweptCoordinate<-.map6DhyperbolaTo6Dcylinder(v_gamma_x_a_b = point2)

  pathPointsIn6DCylinder<-.generatePointsBetween2Points(point1 = point1_SweptCoordinate,
                                                        point2 = point2_SweptCoordinate,
                                                        numberOfPoints = numberOfpoints)

  pathPointsIn_6D_Hyperbola<-array(NA,dim = dim(pathPointsIn6DCylinder))
  for (i in 1:nrow(pathPointsIn6DCylinder)) {
    pathPointsIn_6D_Hyperbola[i,]<-.map6DcylinderTo6Dhyperbola(v_gamma_x_a_b_In6DCylinder =
                                                                 pathPointsIn6DCylinder[i,])
  }

  return(pathPointsIn_6D_Hyperbola)
}

# Transformation between two ETReps using the intrinsic approach
#' Intrinsic Transformation Between Two ETReps
#'
#' Performs an intrinsic transformation from one ETRep to another, preserving essential e-tube properties such as the Relative Curvature Condition (RCC) while avoiding local self-intersections.
#'
#' @param tube1 List containing details of the first ETRep.
#' @param tube2 List containing details of the second ETRep.
#' @param numberOfSteps Integer, number of transformation steps.
#' @param plotting Logical, enables visualization during transformation (default is TRUE).
#' @param colorBoundary String defining the color of the e-tube
#' @param type  String defining the type of analysis as sizeAndShapeAnalysis or shapeAnalysis
#' @return List containing intermediate ETReps.
#' @references
#' Taheri, M., Pizer, S. M., & Schulz, J. (2024). "The Mean Shape under the Relative Curvature Condition." arXiv.
#' \doi{10.48550/arXiv.2404.01043}
#'
#' Taheri Shalmani, M. (2024). "Shape Statistics via Skeletal Structures." University of Stavanger.
#' \doi{10.13140/RG.2.2.34500.23685}
#'
#' @examples
#' \donttest{
#' # Load tubes
#' data("tube_A")
#' data("tube_B")
#' numberOfSteps <- 10
#' transformation_Tubes<-
#'   intrinsic_Transformation_Elliptical_Tubes(
#'     tube1 = tube_A,tube2 = tube_B,
#'     numberOfSteps = numberOfSteps,
#'     plotting = FALSE)
#' # Plotting
#' \dontrun{
#' for (i in 1:length(transformation_Tubes)) {
#'   plot_Elliptical_Tube(tube = transformation_Tubes[[i]],
#'   plot_frames = FALSE,plot_skeletal_sheet = FALSE
#'   ,plot_r_project = FALSE,
#'   plot_r_max = FALSE,
#'   add = FALSE)
#' }
#' ##}
#' }
#' @export
intrinsic_Transformation_Elliptical_Tubes <- function(tube1,
                                                      tube2,
                                                      type="sizeAndShapeAnalysis",
                                                      numberOfSteps=5,
                                                      plotting=TRUE,
                                                      colorBoundary="blue") {

  if(dim(tube1$materialFramesBasedOnParents)[3]!=dim(tube2$materialFramesBasedOnParents)[3]){
    stop('Number of cross-sections are not the same!')
  }

  if(type=="sizeAndShapeAnalysis"){

    numberOfFrames<-dim(tube1$materialFramesBasedOnParents)[3]

    vectorsIn6DHyperbola_tube1<-.tube2vectorsIn6DHyperbola(tube = tube1)
    vectorsIn6DHyperbola_tube2<-.tube2vectorsIn6DHyperbola(tube = tube2)

    pathsBetween2vectorsIn6DHyperbola<-array(NA,dim = c(numberOfSteps,6,numberOfFrames))
    for (i in 1:numberOfFrames) {
      p1<-vectorsIn6DHyperbola_tube1[i,]
      p2<-vectorsIn6DHyperbola_tube2[i,]
      if(norm(p1-p2,type = '2')==0){
        pathsBetween2vectorsIn6DHyperbola[,,i]<-matrix(rep(p1,numberOfSteps),ncol = length(p1),byrow = TRUE)
      }else{
        pathsBetween2vectorsIn6DHyperbola[,,i]<-.discretePathBetween2PointsIn_6D_Hyperbola(
          point1=p1,
          point2=p2,
          numberOfpoints=numberOfSteps)
      }
    }

    unitTangentBasedOnParents4AllSamples<-array(NA,dim = c(numberOfFrames,3,numberOfSteps))
    for (j in 1:numberOfSteps) {
      for (i in 1:numberOfFrames) {
        unitTangentBasedOnParents4AllSamples[i,,j]<-
          .convert_v_to_TangentVector(v =pathsBetween2vectorsIn6DHyperbola[j,c(1,2),i])
      }
    }

    gammaAll<-pathsBetween2vectorsIn6DHyperbola[,3,]
    materialFramesBasedOnParents_Steps<-array(NA,dim=c(dim(tube1$materialFramesBasedOnParents),numberOfSteps))
    materialFramesBasedOnParents_Steps[,,1,]<-diag(3)
    for (k in 1:numberOfSteps) {
      for (i in 2:numberOfFrames) {
        t_vec_temp<-unitTangentBasedOnParents4AllSamples[i,,k]
        gammaTemp<-gammaAll[k,i]

        # calculate material frames by calculate yaw and pitch angles as alpha and beta
        c2s<-.cartesian_to_spherical(v = t_vec_temp)
        alphaTemp<-c2s$phi
        betaTemp<-c2s$theta
        materialFramesBasedOnParents_Steps[,,i,k]<-RSpincalc::EA2DCM(EA = c(alphaTemp, betaTemp, gammaTemp),
                                                                     EulerOrder = 'zyx')
      }
    }
    materialFramesBasedOnParents_Steps[,,numberOfFrames,]<-
      materialFramesBasedOnParents_Steps[,,numberOfFrames-1,]

    #connections' lengths steps
    connectionsLengths_steps<-t(pathsBetween2vectorsIn6DHyperbola[,4,])

    #radii steps
    ellipseRadii_a_steps<-t(pathsBetween2vectorsIn6DHyperbola[,5,])
    ellipseRadii_b_steps<-t(pathsBetween2vectorsIn6DHyperbola[,6,])


    tubes<-list()
    for (j in 1:numberOfSteps) {
      tubes[[j]]<-create_Elliptical_Tube(numberOfFrames = numberOfFrames,
                                         method = "basedOnMaterialFrames",
                                         materialFramesBasedOnParents = materialFramesBasedOnParents_Steps[,,,j],
                                         ellipseRadii_a = ellipseRadii_a_steps[,j],
                                         ellipseRadii_b = ellipseRadii_b_steps[,j],
                                         connectionsLengths = connectionsLengths_steps[,j],
                                         plotting = FALSE)
    }

  }else if(type=="shapeAnalysis"){

    numberOfFrames<-dim(tube1$materialFramesBasedOnParents)[3]

    # NB! scaling does not effect the frames (i.e., the v vectors and roll angles of the scaled tube is the same as in the original tube)
    scaled_tube1<-.scaleETRepToHaveTheSizeAsOne(tube = tube1)
    scaled_tube2<-.scaleETRepToHaveTheSizeAsOne(tube = tube2)

    # insert samples in 6D cylinder
    vectorsIn6DCylinder_tube1<-.tube2vectorsIn6DCylinder(tube = scaled_tube1)
    vectorsIn6DCylinder_tube2<-.tube2vectorsIn6DCylinder(tube = scaled_tube2)

    # path on small cylinder
    v_psiAngleRoll_tube1<-vectorsIn6DCylinder_tube1[,1:3]
    v_psiAngleRoll_tube2<-vectorsIn6DCylinder_tube2[,1:3]

    v_psiAngleRoll_steps<-array(NA,dim = c(numberOfSteps,3,numberOfFrames))
    for (i in 1:numberOfFrames) {
      v_psiAngleRoll_steps[,,i]<-.generatePointsBetween2Points(point1 = v_psiAngleRoll_tube1[i,],
                                                               point2 = v_psiAngleRoll_tube2[i,],
                                                               numberOfPoints = numberOfSteps)
    }

    # path on the hyper-plane n.w=d where d is the space's dimension
    w1<-c(scaled_tube1$connectionsLengths,scaled_tube1$ellipseRadii_a,scaled_tube1$ellipseRadii_b)
    w2<-c(scaled_tube2$connectionsLengths,scaled_tube2$ellipseRadii_a,scaled_tube2$ellipseRadii_b)

    pathBetween_w1_w2<-.generatePointsBetween2Points(w1,w2,numberOfPoints = numberOfSteps)

    connectionsLengths_steps<-t(pathBetween_w1_w2)[1:numberOfFrames,]
    ellipseRadii_a_steps<-t(pathBetween_w1_w2)[(numberOfFrames+1):(2*numberOfFrames),]
    ellipseRadii_b_steps<-t(pathBetween_w1_w2)[(2*numberOfFrames+1):(3*numberOfFrames),]

    matricesIn6DHyperbola<-array(NA,dim = c(dim(vectorsIn6DCylinder_tube1),numberOfSteps))
    for (j in 1:numberOfSteps) {
      tempMatrix_In_6DCylinder<-array(NA,dim = dim(vectorsIn6DCylinder_tube1))
      for (i in 1:numberOfFrames) {
        tempMatrix_In_6DCylinder[i,]<-c(v_psiAngleRoll_steps[j,,i],
                                        connectionsLengths_steps[i,j],
                                        ellipseRadii_a_steps[i,j],
                                        ellipseRadii_b_steps[i,j])
      }
      matricesIn6DHyperbola[,,j]<-t(apply(tempMatrix_In_6DCylinder,
                                          MARGIN = 1,
                                          FUN = .map6DcylinderTo6Dhyperbola))
    }

    tubes<-list()
    for (j in 1:numberOfSteps) {

      tubes[[j]]<-.convertMatrixIn6DHyperbola2Tube(matrixIn6DHyperbola=
                                                     matricesIn6DHyperbola[,,j])
    }


  }else{
    stop("Please choose type as sizeAndShapeAnalysis or shapeAnalysis !")
  }


  #plot tubes
  if(plotting==TRUE){
    for (j in 1:numberOfSteps) {
      plot_Elliptical_Tube(tubes[[j]],
                           plot_boundary = TRUE,
                           plot_frames = TRUE,
                           colorBoundary = colorBoundary,
                           plot_skeletal_sheet = FALSE,
                           plot_r_project = FALSE)
    }
  }

  return(tubes)
}


# Transformation between two ETReps using the non-intrinsic approach
#' Non-Intrinsic Transformation Between Two ETReps
#'
#' Performs a non-intrinsic transformation from one ETRep to another. This approach is inspired by robotic arm transformations and does not account for the Relative Curvature Condition (RCC).
#'
#' @param tube1 List containing details of the first ETRep.
#' @param tube2 List containing details of the second ETRep.
#' @param numberOfSteps Integer, number of transformation steps.
#' @param plotting Logical, enables visualization during transformation (default is TRUE).
#' @param colorBoundary String defining the color of the e-tube
#' @param type  String defining the type of analysis as sizeAndShapeAnalysis or shapeAnalysis
#' @param add Logical, enables overlay plotting
#' @return List containing intermediate ETReps.
#' @references
#' Taheri, M., Pizer, S. M., & Schulz, J. (2024). "The Mean Shape under the Relative Curvature Condition." arXiv.
#' \doi{10.48550/arXiv.2404.01043}
#'
#' Taheri Shalmani, M. (2024). "Shape Statistics via Skeletal Structures." University of Stavanger.
#' \doi{10.13140/RG.2.2.34500.23685}
#'
#' @examples
#' \donttest{
#' # Load tubes
#' data("tube_A")
#' data("tube_B")
#' numberOfSteps <- 10
#' transformation_Tubes<-
#'   nonIntrinsic_Transformation_Elliptical_Tubes(
#'     tube1 = tube_A,tube2 = tube_B,
#'     numberOfSteps = numberOfSteps,
#'     plotting = FALSE)
#' # Plotting
#' \dontrun{
#' for (i in 1:length(transformation_Tubes)) {
#'   plot_Elliptical_Tube(tube = transformation_Tubes[[i]],
#'   plot_frames = FALSE,plot_skeletal_sheet = FALSE
#'   ,plot_r_project = FALSE,
#'   plot_r_max = FALSE,
#'   add = FALSE)
#' }
#'  }
#' }
#' @export
nonIntrinsic_Transformation_Elliptical_Tubes <- function(tube1,
                                                         tube2,
                                                         type="sizeAndShapeAnalysis",
                                                         numberOfSteps=4,
                                                         plotting=TRUE,
                                                         colorBoundary="blue",
                                                         add=FALSE) {

  if(dim(tube1$materialFramesBasedOnParents)[3]!=dim(tube2$materialFramesBasedOnParents)[3]){
    stop('Number of cross-sections are not the same!')
  }
  numberOfFrames<-dim(tube1$materialFramesBasedOnParents)[3]

  materialFramesBasedOnParents_Steps<-array(NA,dim=c(dim(tube1$materialFramesBasedOnParents),numberOfSteps))
  for (i in 1:numberOfFrames) {

    q1_twist<-rotations::as.Q4(rotations::as.SO3(tube1$materialFramesBasedOnParents[,,i]))
    q2_twist<-rotations::as.Q4(rotations::as.SO3(tube2$materialFramesBasedOnParents[,,i]))

    if(norm(as.vector(q1_twist)-as.vector(q2_twist),type = '2')<10^-6){
      for (j in 1:numberOfSteps) {
        materialFramesBasedOnParents_Steps[,,i,j]<-rotations::as.SO3(q2_twist)
      }
    }else{
      q4pointsOnAGeodesic_twist<-.geodesicPathOnUnitSphere(as.vector(q1_twist),
                                                           as.vector(q2_twist),
                                                           numberOfneededPoints = numberOfSteps)
      for (j in 1:numberOfSteps) {
        materialFramesBasedOnParents_Steps[,,i,j]<-rotations::as.SO3(rotations::as.Q4(q4pointsOnAGeodesic_twist[j,]))
      }
    }
  }

  if(type=="sizeAndShapeAnalysis"){

    ellipseRadii_a_steps<-array(NA,dim=c(numberOfFrames,numberOfSteps))
    ellipseRadii_b_steps<-array(NA,dim=c(numberOfFrames,numberOfSteps))
    connectionsLengths_steps<-array(NA,dim=c(numberOfFrames,numberOfSteps))
    for (i in 1:numberOfFrames) {
      ellipseRadii_a_steps[i,]<-seq(from=tube1$ellipseRadii_a[i],
                                    to=tube2$ellipseRadii_a[i],
                                    length.out=numberOfSteps)
      ellipseRadii_b_steps[i,]<-seq(from=tube1$ellipseRadii_b[i],
                                    to=tube2$ellipseRadii_b[i],
                                    length.out=numberOfSteps)
      connectionsLengths_steps[i,]<-seq(from=tube1$connectionsLengths[i],
                                        to=tube2$connectionsLengths[i],
                                        length.out=numberOfSteps)
    }

  }else if(type=="shapeAnalysis"){

    scaled_tube1<-.scaleETRepToHaveTheSizeAsOne(tube = tube1)
    scaled_tube2<-.scaleETRepToHaveTheSizeAsOne(tube = tube2)

    u_1<-c(scaled_tube1$ellipseRadii_a,scaled_tube1$ellipseRadii_b,scaled_tube1$connectionsLengths)
    u_2<-c(scaled_tube2$ellipseRadii_a,scaled_tube2$ellipseRadii_b,scaled_tube2$connectionsLengths)

    geodesicPathBetween_u1_u2<-.geodesicPathOnUnitSphere(point1 = u_1,
                                                         point2 = u_2,
                                                         numberOfneededPoints = numberOfSteps)

    ellipseRadii_a_steps<-t(geodesicPathBetween_u1_u2)[1:numberOfFrames,]
    ellipseRadii_b_steps<-t(geodesicPathBetween_u1_u2)[(numberOfFrames+1):(2*numberOfFrames),]
    connectionsLengths_steps<-t(geodesicPathBetween_u1_u2)[(2*numberOfFrames+1):(3*numberOfFrames),]

  }else{
    stop("Please choose type as sizeAndShapeAnalysis or shapeAnalysis !")
  }

  tubes<-list()
  for (j in 1:numberOfSteps) {

    tubes[[j]]<-create_Elliptical_Tube(numberOfFrames = numberOfFrames,
                                       method = "basedOnMaterialFrames",
                                       materialFramesBasedOnParents = materialFramesBasedOnParents_Steps[,,,j],
                                       ellipseRadii_a = ellipseRadii_a_steps[,j],
                                       ellipseRadii_b = ellipseRadii_b_steps[,j],
                                       connectionsLengths = connectionsLengths_steps[,j],
                                       plotting = FALSE)
  }


  #plot tubes
  if(plotting==TRUE){
    for (j in 1:numberOfSteps) {
      plot_Elliptical_Tube(tube = tubes[[j]],
                           plot_r_project = FALSE,
                           plot_r_max = FALSE,
                           colorBoundary=colorBoundary,
                           add = add)
    }
  }

  return(tubes)


}


# Calculating the mean ETRep using the non-intrinsic approach
#' Compute Non-Intrinsic Mean of ETReps
#'
#' Calculates the non-intrinsic mean of a set of ETReps. This method utilizes a non-intrinsic distance metric based on robotic arm non-intrinsic transformations.
#'
#' @param tubes List of ETReps.
#' @param type String, "ShapeAnalysis" or "sizeAndShapeAnalysis" (default is "sizeAndShapeAnalysis").
#' @param plotting Logical, enables visualization of the mean (default is TRUE).
#' @return List representing the mean ETRep.
#' @examples
#' #Example 1
#' # Load tubes
#' data("tube_A")
#' data("tube_B")
#' nonIntrinsic_mean<-
#'   nonIntrinsic_mean_tube(tubes = list(tube_A,tube_B),
#'                          plotting = FALSE)
#' # Plotting
#' \dontrun{
#' plot_Elliptical_Tube(tube = nonIntrinsic_mean,
#'                      plot_frames = FALSE,
#'                      plot_skeletal_sheet = FALSE,
#'                      plot_r_project = FALSE,
#'                      plot_r_max = FALSE,
#'                      add = FALSE)
#'  }
#'
#' #Example 2
#' data("simulatedColons")
#' nonIntrinsic_mean<-
#'   nonIntrinsic_mean_tube(tubes = simulatedColons,
#'                          plotting = FALSE)
#' # Plotting
#' \dontrun{
#' plot_Elliptical_Tube(tube = nonIntrinsic_mean,
#'                      plot_frames = FALSE,
#'                      plot_skeletal_sheet = FALSE,
#'                      plot_r_project = FALSE,
#'                      plot_r_max = FALSE,
#'                      add = FALSE)
#'  }
#' @export
nonIntrinsic_mean_tube <- function(tubes,
                                   type ="sizeAndShapeAnalysis",
                                   plotting=TRUE) {

  if(type == "shapeAnalysis"){
    message("\n Shape analysis \n")
    for (i in 1:length(tubes)) {
      tubes[[i]]<-.scaleETRepToHaveTheSizeAsOne(tube =tubes[[i]] ,plotting = FALSE)
    }
  }else if(type == "sizeAndShapeAnalysis"){
    message("\n Size-and-shape analysis \n")
  }else{
    stop("Please choose type as sizeAndShapeAnalysis or shapeAnalysis !")
  }


  numberOfSamples<-length(tubes)
  numberOfFrames<-dim(tubes[[1]]$materialFramesBasedOnParents)[3]

  materialFramesBasedOnParents_tubes<-array(NA,dim = c(3,3,numberOfFrames,numberOfSamples))
  for (j in 1:numberOfSamples) {
    materialFramesBasedOnParents_tubes[,,,j]<-tubes[[j]]$materialFramesBasedOnParents
  }

  vectorizedMaterialFramesBasedOnParents<-array(NA,dim = c(numberOfFrames,9,numberOfSamples))
  for (j in 1:numberOfSamples) {
    for (i in 1:numberOfFrames) {
      vectorizedMaterialFramesBasedOnParents[i,,j]<-as.vector(t(materialFramesBasedOnParents_tubes[,,i,j]))
    }
  }

  meanMaterialFramesBasedOnParents<-array(NA, dim = c(3,3,numberOfFrames))
  for (i in 1:numberOfFrames) {
    tempVec<-mean(rotations::as.SO3(t(vectorizedMaterialFramesBasedOnParents[i,,])),type = 'projected')
    # tempVec<-mean(as.SO3(t(vectorizedMaterialFramesBasedOnParents[i,,])),type = 'geometric')
    meanMaterialFramesBasedOnParents[,,i]<-matrix(tempVec,nrow = 3,byrow = TRUE)
  }


  ellipseRadii_a_samples<-array(NA,dim = c(numberOfFrames,numberOfSamples))
  ellipseRadii_b_samples<-array(NA,dim = c(numberOfFrames,numberOfSamples))
  for (j in 1:numberOfSamples) {
    ellipseRadii_a_samples[,j]<-tubes[[j]]$ellipseRadii_a
    ellipseRadii_b_samples[,j]<-tubes[[j]]$ellipseRadii_b
  }

  mean_ellipseRadii_a<-rowMeans(ellipseRadii_a_samples)
  mean_ellipseRadii_b<-rowMeans(ellipseRadii_b_samples)

  connectionsLengths_samples<-array(NA,dim = c(numberOfFrames,numberOfSamples))
  for (j in 1:numberOfSamples) {
    connectionsLengths_samples[,j]<-tubes[[j]]$connectionsLengths
  }

  mean_connectionsLengths<-rowMeans(connectionsLengths_samples)

  meanTube<-create_Elliptical_Tube(numberOfFrames = numberOfFrames,
                                   method = "basedOnMaterialFrames",
                                   materialFramesBasedOnParents = meanMaterialFramesBasedOnParents,
                                   ellipseResolution = 10,
                                   ellipseRadii_a = mean_ellipseRadii_a,
                                   ellipseRadii_b = mean_ellipseRadii_b,
                                   connectionsLengths = mean_connectionsLengths,
                                   plotting = plotting)

}


# ETRep simulation
#' Simulate Random Elliptical Tubes (ETReps)
#'
#' Generates random samples of ETReps based on a reference tube with added variation.
#'
#' @param referenceTube List containing ETRep information as the reference.
#' @param numberOfSimulation Integer, number of random samples.
#' @param sd_v Standard deviations for various parameters.
#' @param sd_psi Standard deviations for various parameters.
#' @param sd_x Standard deviations for various parameters.
#' @param sd_a Standard deviations for various parameters.
#' @param sd_b Standard deviations for various parameters.
#' @param rangeSdScale Numeric range for random scaling.
#' @param plotting Logical, enables visualization of samples (default is FALSE).
#' @return List of random ETReps.
#' @references
#' Taheri, M., Pizer, S. M., & Schulz, J. (2024). "The Mean Shape under the Relative Curvature Condition." arXiv.
#' \doi{10.48550/arXiv.2404.01043}
#'
#' Taheri Shalmani, M. (2024). "Shape Statistics via Skeletal Structures." University of Stavanger.
#' \doi{10.13140/RG.2.2.34500.23685}
#'
#' @examples
#' \donttest{
#' # Load tube
#' data("colon3D")
#' #Set Parameters
#' sd_v<-sd_psi<-1e-03
#' sd_x<-sd_a<-sd_b<-1e-04
#' numberOfSimulation<-4
#' random_Tubes<-
#'   simulate_etube(referenceTube = colon3D,
#'                  numberOfSimulation = numberOfSimulation,
#'                  sd_v = sd_v,
#'                  sd_psi = sd_psi,
#'                  sd_x = sd_x,
#'                  sd_a = sd_a,
#'                  sd_b = sd_b,
#'                  rangeSdScale = c(1, 2),
#'                  plotting = FALSE)
#' # Plotting
#' \dontrun{
#' plot_Elliptical_Tube(random_Tubes[[1]], add = FALSE)
#' plot_Elliptical_Tube(random_Tubes[[2]], add = TRUE)
#' plot_Elliptical_Tube(random_Tubes[[3]], add = TRUE)
#' plot_Elliptical_Tube(random_Tubes[[4]], add = TRUE)
#'  }
#' }
#' @export
simulate_etube <- function(referenceTube,
                           numberOfSimulation,
                           sd_v=10^-10,
                           sd_psi=10^-10,
                           sd_x=10^-10,
                           sd_a=10^-10,
                           sd_b=10^-10,
                           rangeSdScale=c(1,2),
                           plotting=TRUE) {


  referenceTube_scaled<-.scaleETRepToHaveTheSizeAsOne(referenceTube)

  tubeIn6DCylinder<-.tube2vectorsIn6DCylinder(referenceTube_scaled)
  tubeIn6DCylinder[is.infinite(tubeIn6DCylinder)]<-0

  numberOfFrames<-nrow(tubeIn6DCylinder)

  simulatedTubes<-list()
  for (j in 1:numberOfSimulation) {
    tubeSimulatedTempIn6DCylinder<-array(NA,dim = dim(tubeIn6DCylinder))
    for (i in 1:numberOfFrames) {

      w1<-rtruncnorm(n = 1,a = -1,b = 1,mean = tubeIn6DCylinder[i,1],sd = sd_v)
      w2<-rtruncnorm(n = 1,a = -1,b = 1,mean = tubeIn6DCylinder[i,2],sd = sd_v)
      w3<-rtruncnorm(n = 1,a = -1,b = 1,mean = tubeIn6DCylinder[i,3],sd = sd_psi)
      w4<-rtruncnorm(n = 1,a = 10^-6,b = 1,mean = tubeIn6DCylinder[i,4],sd = sd_x)
      w5<-rtruncnorm(n = 1,a = 10^-6,b = 1,mean = tubeIn6DCylinder[i,5],sd = sd_a)
      w6<-rtruncnorm(n = 1,a = 10^-6,b = 1,mean = tubeIn6DCylinder[i,6],sd = sd_b)

      #ensure validity
      if(norm(c(w1,w2),type = "2")>1){
        temp<-.convertVec2unitVec(c(w1,w2))*0.9999
        w1<-temp[1]
        w1<-temp[2]
      }

      tubeSimulatedTempIn6DCylinder[i,]<-c(w1,w2,w3,w4,w5,w6)
    }
    tubeSimulatedTempIn6DCylinder[1,1:3]<-0

    matrixIn6DHyperbola<-t(apply(tubeSimulatedTempIn6DCylinder,
                                 MARGIN = 1,
                                 FUN = .map6DcylinderTo6Dhyperbola))

    kappa_total_scale<-runif(n = 1,min = rangeSdScale[1],max = rangeSdScale[2])

    matrixIn6DHyperbola[,3:6]<-matrixIn6DHyperbola[,3:6]*kappa_total_scale

    #convert mean matrix to a tube
    simulatedTubes[[j]]<-.convertMatrixIn6DHyperbola2Tube(matrixIn6DHyperbola=matrixIn6DHyperbola)
  }

  if(plotting==TRUE){
    color_spectrum <- colorRampPalette(c("lightblue","darkblue"))
    colors <- color_spectrum(numberOfSimulation)
    .etrep_open3d(show_widget = TRUE) 
    for (j in 1:numberOfSimulation) {
      plot_Elliptical_Tube(simulatedTubes[[j]],
                           colorBoundary = colors[j],
                           plot_frames = FALSE,
                           plot_skeletal_sheet = FALSE,
                           add = TRUE)
    }
  }

  return(simulatedTubes)

}


# Plot the boundary points of an ETRep (For Procrustes analysis)
#' @keywords internal
.plotProcTube <- function(boundaryPoints,
                          numberOfEllispePoints,
                          colorBoundary="blue",
                          colorSpine="black") {

  m<-nrow(boundaryPoints)
  d<-numberOfEllispePoints
  rows_per_matrix <- m / d
  list_of_rows<-split(1:m, cut(seq_along(1:m), rows_per_matrix, labels = FALSE))


  centroids<-c()
  for (i in 1:length(list_of_rows)) {
    centroids<-rbind(centroids,
                     colMeans(boundaryPoints[list_of_rows[[i]],]))
  }
  ellipses<-list()
  for (i in 1:length(list_of_rows)) {
    ellipses[[i]]<-boundaryPoints[c(list_of_rows[[i]],list_of_rows[[i]][1]),]
  }

  .etrep_open3d(show_widget = TRUE)
  for (i in 1:length(list_of_rows)) {
    rgl::plot3d(ellipses[[i]],type = 'l',col=colorBoundary,expand = 10,box=FALSE,add = TRUE)
  }
  for (i in 1:d) {
    ith_Points <- do.call(rbind, lapply(ellipses, function(x) x[i, ]))
    rgl::plot3d(ith_Points ,type = 'l',col=colorBoundary,expand = 10,box=FALSE,add = TRUE)
  }
  rgl::plot3d(centroids,type = 'l',col=colorSpine,lwd=3,expand = 10,box=FALSE,add = TRUE)
  #decorate3d()
  ## if we are using NULL device (macOS), open browser
  if (getOption("rgl.useNULL", FALSE)) {
    .etrep_show3d()                       # <-- opens the plot in browser
  }
}


# Identifying cross-sections of an e-tube with non-local intersection
#' @keywords internal
.tubeCrossSetionsIndicesWith_NonLocal_SelfIntersections <- function(tube) {

  criticalElipses_index<-c()
  intersectionPoints<-c()
  pb <- txtProgressBar(min = 0, max = nrow(tube$spinalPoints3D), style = 3) #progress bar
  for (i in 1:nrow(tube$spinalPoints3D)) {
    setTxtProgressBar(pb, i) #create progress bar
    for (j in i:nrow(tube$spinalPoints3D)) {
      if(i==j){
        next
      }
      ellipse1<-tube$slicingEllipsoids[,,i]
      ellipse2<-tube$slicingEllipsoids[,,j]

      intersectionPointsTemp<-.intersectionPointsBetween2Ellipses_In3D(ellipse1 = ellipse1,ellipse2 = ellipse2,plotting = FALSE)

      if(!anyNA(intersectionPointsTemp) & !is.null(intersectionPointsTemp)){
        intersectionPoints<-rbind(intersectionPoints,intersectionPointsTemp)
        criticalElipses_index<-c(criticalElipses_index,i,j)
      }
    }
  }

  result<-list("criticalElipses_index"=sort(unique(criticalElipses_index)),
               "intersectionPoints"=intersectionPoints)
  return(result)
}

# Non-intrinsic transformation between two ETReps by cross-sectional adjustment
#' @keywords internal
.nonIntrinsic_Transformation_Elliptical_Tubes_Without_SelfIntersection <- function(tube1,
                                                                                   tube2,
                                                                                   numberOfSteps=8,
                                                                                   scalingFactor=0.9,
                                                                                   removeNonLocalSingularity=TRUE,
                                                                                   plotting=TRUE) {

  tubes<-nonIntrinsic_Transformation_Elliptical_Tubes(tube1 = tube1,
                                                      tube2 = tube2,
                                                      numberOfSteps = numberOfSteps,
                                                      plotting = FALSE)

  #remove local self intersection
  for (i in 1:length(tubes)) {
    tubeTemp<-tubes[[i]]

    while(any(tubeTemp$r_max_lengths<tubeTemp$r_project_lengths)){
      criticalCrossSections<-which(tubeTemp$r_max_lengths<tubeTemp$r_project_lengths)
      tubeTemp$ellipseRadii_a[criticalCrossSections]<-scalingFactor*tubeTemp$ellipseRadii_a[criticalCrossSections]
      tubeTemp$ellipseRadii_b[criticalCrossSections]<-scalingFactor*tubeTemp$ellipseRadii_b[criticalCrossSections]

      tubeTemp<-create_Elliptical_Tube(numberOfFrames = dim(tubeTemp$materialFramesBasedOnParents)[3],
                                       method = "basedOnMaterialFrames",
                                       materialFramesBasedOnParents = tubeTemp$materialFramesBasedOnParents,
                                       ellipseRadii_a = tubeTemp$ellipseRadii_a,
                                       ellipseRadii_b = tubeTemp$ellipseRadii_b,
                                       connectionsLengths = tubeTemp$connectionsLengths,
                                       plotting = FALSE)
    }
    #plot_Elliptical_Tube(tube = tubeTemp)

    if(removeNonLocalSingularity==TRUE){
      #remove non-local intersections
      indicesOfCriticalNonLocalIntersections<-.tubeCrossSetionsIndicesWith_NonLocal_SelfIntersections(tube = tubeTemp)$criticalElipses_index
      while(length(indicesOfCriticalNonLocalIntersections)>=1){
        message("Number of nonlocal intersection is: ",length(indicesOfCriticalNonLocalIntersections),"\n")
        tubeTemp$ellipseRadii_a[indicesOfCriticalNonLocalIntersections]<-scalingFactor*tubeTemp$ellipseRadii_a[indicesOfCriticalNonLocalIntersections]
        tubeTemp$ellipseRadii_b[indicesOfCriticalNonLocalIntersections]<-scalingFactor*tubeTemp$ellipseRadii_b[indicesOfCriticalNonLocalIntersections]

        tubeTemp<-create_Elliptical_Tube(numberOfFrames = dim(tubeTemp$materialFramesBasedOnParents)[3],
                                         method = "basedOnMaterialFrames",
                                         materialFramesBasedOnParents = tubeTemp$materialFramesBasedOnParents,
                                         ellipseRadii_a = tubeTemp$ellipseRadii_a,
                                         ellipseRadii_b = tubeTemp$ellipseRadii_b,
                                         connectionsLengths = tubeTemp$connectionsLengths,
                                         plotting = FALSE)
        indicesOfCriticalNonLocalIntersections<-.tubeCrossSetionsIndicesWith_NonLocal_SelfIntersections(tube = tubeTemp)$criticalElipses_index
      }
    }
    plot_Elliptical_Tube(tube = tubeTemp,
                         plot_boundary = TRUE,plot_r_max = FALSE,plot_r_project = FALSE,
                         plot_frames = TRUE,
                         plot_normal_vec = FALSE,plot_skeletal_sheet = FALSE,
                         decorate = FALSE)

    tubes[[i]]<-tubeTemp

  }


  #plot tubes
  if(plotting==TRUE){
    for (i in 1:length(tubes)) {
      plot_Elliptical_Tube(tube = tubes[[i]],
                           plot_boundary = TRUE,plot_r_max = FALSE,plot_r_project = FALSE,
                           plot_frames = TRUE,
                           plot_normal_vec = FALSE,plot_skeletal_sheet = FALSE,
                           decorate = FALSE)
    }
  }

  return(tubes)

}

# Check the legality of an ETRep
#' Check the Legality of an Elliptical Tube (ETRep)
#'
#' Checks the validity of a given ETRep based on the Relative Curvature Condition (RCC) and principal radii such that forall i a_i>b_i.
#'
#' @param tube List containing ETRep details.
#' @return Logical value: TRUE if valid, FALSE otherwise.
#' @references
#' Taheri, M., Pizer, S. M., & Schulz, J. (2024). "The Mean Shape under the Relative Curvature Condition." arXiv.
#' \doi{10.48550/arXiv.2404.01043}
#'
#' Taheri Shalmani, M. (2024). "Shape Statistics via Skeletal Structures." University of Stavanger.
#' \doi{10.13140/RG.2.2.34500.23685}
#'
#' @examples
#' # Load tube
#' data("colon3D")
#' check_Tube_Legality(tube = colon3D)
#' @export
check_Tube_Legality <- function(tube) {
  numberOfFrames<-nrow(tube$spinalPoints3D)
  criticalIndices_RCC<-which(tube$r_project_lengths>tube$r_max_lengths)
  criticalIndices_Radii<-which(tube$ellipseRadii_a<tube$ellipseRadii_b)
  if(length(criticalIndices_Radii)>0){
    message("The tube is not valid as it is not elliptical!\n Critical cross-sections that a<b are: ",paste(criticalIndices_Radii,collapse = " , "))
    return(FALSE)
  }else if(length(criticalIndices_RCC)>0){
    message("The tube is not valid as it violates the RCC! \n Critical cross-sections are:",paste(criticalIndices_RCC,collapse = " , "))
    return(FALSE)
  }else{
    message("The tube is a valid elliptical tube and it satisfies the RCC! \n")
    return(TRUE)
  }
}


# Fitting radial spokes based on parallel slicing regarding radial distance analysis of Supplementary Materials
#' @keywords internal
.fitRadialVectorsModelBasedOnParallelSlicing <- function(PDM,
                                                         polyMatrix,
                                                         nunmberOfSlices,
                                                         numberOFRadialSpokes,
                                                         plotting=TRUE,
                                                         colorRadialVectors="blue") {

  verts <- rbind(t(base::as.matrix(PDM)),1)
  trgls <- base::as.matrix(t(polyMatrix))
  tmesh <- rgl::tmesh3d(verts, trgls)
  tmesh <- Rvcg::vcgUpdateNormals(tmesh)
  # shade3d(tmesh, col="white",alpha=0.2)  #surface mesh

  #remeshing to increase the number of triangles by reducing the voxelSize
  remeshedMesh<-Rvcg::vcgUniformRemesh(tmesh,voxelSize = 0.5)
  tmeshSmooth<-Rvcg::vcgUpdateNormals(remeshedMesh)

  pointsTest<-Morpho::vert2points(tmeshSmooth)
  slicesAlongXaxis<-seq(min(pointsTest[,2]),max(pointsTest[,2]),length.out=nunmberOfSlices)

  # we use asymmetric circles to avoid antipodal vectors
  tempCircle<-.sphereGenerator_2D(center = c(0,0),r = 1,
                                  n = numberOFRadialSpokes+1,asymmetric = FALSE)

  slicesCentroids<-c()
  radialSpokesTails<-c()
  radialSpokesTips<-c()
  for (i in 1:(nunmberOfSlices-1)) {
    tempIndices<-which(pointsTest[,2]>=slicesAlongXaxis[i] & pointsTest[,2]<=slicesAlongXaxis[i+1])
    centroidTemp<-colMeans(pointsTest[tempIndices,])
    slicesCentroids<-rbind(slicesCentroids,centroidTemp)

    tempRadialSpokesTails<-matrix(rep(centroidTemp,nrow(tempCircle)),ncol = 3,byrow = TRUE)

    circleIn3D<-tempRadialSpokesTails + cbind(tempCircle[,1],rep(0,nrow(tempCircle)),tempCircle[,2])

    circleMesh3D<-rgl::as.mesh3d(circleIn3D)

    normalsTemp<-circleIn3D-tempRadialSpokesTails

    circleMesh3D$normals<-rbind(apply(normalsTemp,FUN = .convertVec2unitVec,MARGIN = 1),
                                rep(1,nrow(circleIn3D)))


    intersections<-Rvcg::vcgRaySearch(circleMesh3D,mesh = tmesh)
    tempRadialSpokesTips<-Morpho::vert2points(intersections)

    radialSpokesTails<-rbind(radialSpokesTails,tempRadialSpokesTails)
    radialSpokesTips<-rbind(radialSpokesTips,tempRadialSpokesTips)

  }

  if(plotting==TRUE){
    .etrep_open3d(show_widget = TRUE)
    shade3d(tmesh,col="white",alpha=0.2)
    rgl::plot3d(slicesCentroids,type="s",radius = 0.2,col = colorRadialVectors,expand = 10,box=FALSE,add = TRUE)
    rgl::plot3d(slicesCentroids,type="l",lwd=4,col = colorRadialVectors,expand = 10,box=FALSE,add = TRUE)

    #plot slicing-planes
    rangeSquare<-10
    square<-rbind(c(-rangeSquare,0,-rangeSquare),c(-rangeSquare,0,rangeSquare),c(rangeSquare,0,rangeSquare),c(rangeSquare,0,-rangeSquare))
    for (i in 1:nrow(slicesCentroids)) {
      slicingPlaneTemp<-square+matrix(rep(slicesCentroids[i,],4),ncol = 3,byrow = TRUE)
      rgl::plot3d(rbind(slicingPlaneTemp,slicingPlaneTemp[1,]),type="l",lwd=2,col = "grey",expand = 10,box=FALSE,add = TRUE)
    }

    matlib::vectors3d(radialSpokesTips,
                      origin = radialSpokesTails,headlength = 0.1,radius = 1/10, col=colorRadialVectors, lwd=2)

    decorate3d()
    
    ## if we are using NULL device (macOS), open browser
    if (getOption("rgl.useNULL", FALSE)) {
      .etrep_show3d()                       # <-- opens the plot in browser
    }
    
  }

  result<-list("tmesh"=tmesh,
               "radialSpokesTails"=radialSpokesTails,
               "radialSpokesTips"=radialSpokesTips,
               "slicesCentroids"=slicesCentroids)

  return(result)
}


#' Data
#'
#' A tube with 204 elliptical cross-sections.
#'
#' @format A list containing the information of an e-tube with 204 elliptical cross-sections
#' @source Generated and stored in the package's `data/` folder.
"tube_A"

#' Data
#'
#' A tube with 204 elliptical cross-sections.
#'
#' @format A list containing the information of an e-tube with 204 elliptical cross-sections
#' @source Generated and stored in the package's `data/` folder.
"tube_B"

#' Data
#'
#' A colon sample as an elliptical tube.
#'
#' @format A list containing the information of an e-tube
#' @source Generated and stored in the package's `data/` folder.
"colon3D"

#' Data
#'
#' Simulated samples of e-tubes, modeled after a reference structure resembling a colon.
#'
#' @format Five simulated samples of elliptical tubes, modeled after a reference structure resembling a colon.
#' @source Generated and stored in the package's `data/` folder.
"simulatedColons"


