# Calculating the size of an ETRep
ETRepSize <- function(tube) {
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

# Normalization by uniform scaling
scaleETRepToHaveTheSizeAsOne <- function(tube, 
                                         plotting=FALSE) {
  
  size<-ETRepSize(tube = tube)
  
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
convert_v_to_TangentVector<- function(v) {
  
  if(length(v)!=2){stop("v is not a 2-dimensional vector!")}
  
  x<-v[1]
  y<-v[2]
  z<-abs(sqrt(abs(1-x^2-y^2)))
  unitTangent<-convertVec2unitVec2(c(z,x,y))
  return(unitTangent)
  
}

# Calculate the r_project refering to Equation (1) of the paper
calculate_r_project_Length <- function(a, b, theta) {
  
  t_max<-atan(-b/a*tan(theta))
  r_project_length<-abs(a*cos(t_max)*cos(theta)-b*sin(t_max)*sin(theta))
  
  return(r_project_length)
}

# calcualting the twisting angle theta
calculate_theta <- function(materialFrameBasedOnParent,
                            tolerance=1e-7) {
  
  #reference frame is I
  t_vec<-materialFrameBasedOnParent[1,]
  a_vec<-materialFrameBasedOnParent[2,]
  b_vec<-materialFrameBasedOnParent[3,]
  
  u2<-t_vec
  u1<-c(-1,0,0)
  psiTemp<-geodesicDistance(u1,u2)
  if(abs(psiTemp-pi)>tolerance){
    #formula from function geodesicPathOnUnitSphere()
    n_vec<-1/sin(psiTemp)*(sin(pi/2)*u1+sin(psiTemp-pi/2)*u2)  
  }else{
    n_vec<-c(0,1,0)
  }
  
  theta<-geodesicDistance(n_vec,a_vec)
  
  return(theta)
}

# Create a discrete e-tube based on the material frames
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
                                   plotting=TRUE,
                                   add=FALSE) {
  
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
      materialFramesBasedOnParents[,,i]<-EA2DCM(EA = c(EulerAngles_alpha[i],EulerAngles_beta[i],EulerAngles_gamma[i]),
                                                EulerOrder = 'zyx')
    } 
  }else if(method=="basedOnMaterialFrames" & 
           !any(is.na(materialFramesBasedOnParents &
                      dim(materialFramesBasedOnParents)[3]==numberOfFrames))){
    materialFramesBasedOnParents<-materialFramesBasedOnParents
  }else{
    stop("Please specify the method as basedOnEulerAngles or basedOnMaterialFrames !")
  }
  
  # open3d()
  # vectors3d(1.5*diag(3))
  # vectors3d(t(materialFramesBasedOnParents[1,,]),col="blue")
  # vectors3d(t(materialFramesBasedOnParents[2,,]),col="red")
  # vectors3d(t(materialFramesBasedOnParents[3,,]),col="green")
  
  framesCenters<-1:numberOfFrames
  framesParents<-c(1,1:(numberOfFrames-1))
  
  materialFramesGlobalCoordinate<-array(NA,dim = dim(materialFramesBasedOnParents))
  materialFramesGlobalCoordinate[,,1]<-initialFrame
  for (k in 2:numberOfFrames) {
    parent_Index<-framesParents[k]
    child_Index<-framesCenters[k]
    updatedParent<-materialFramesGlobalCoordinate[,,parent_Index]
    materialFramesGlobalCoordinate[,,child_Index]<-
      rotateFrameToMainAxesAndRotateBack_standard(myFrame = updatedParent,
                                                  vectors_In_I_Axes = materialFramesBasedOnParents[,,child_Index])
  }
  
  # spinal points
  spinalPoints3D<-array(NA,dim = c(numberOfFrames,3))
  spinalPoints3D[1,]<-initialPoint
  for (i in 1:(numberOfFrames-1)) {
    spinalPoints3D[i+1,]<-spinalPoints3D[i,]+
      connectionsLengths[i+1]*materialFramesGlobalCoordinate[1,,i]
  }
  
  # open3d()
  # vectors3d(spinalPoints3D+t(materialFramesGlobalCoordinate[1,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="blue", lwd=2)
  # vectors3d(spinalPoints3D+t(materialFramesGlobalCoordinate[2,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="red", lwd=2)
  # vectors3d(spinalPoints3D+t(materialFramesGlobalCoordinate[3,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="green", lwd=2)
  # decorate3d()
  
  ##################################################################
  ##################################################################
  #Frenet frames
  
  frenetFramesGlobalCoordinate<-array(NA,dim = c(3,3,numberOfFrames))
  frenetFramesGlobalCoordinate[,,1]<-materialFramesGlobalCoordinate[,,1]
  for (i in 2:(numberOfFrames-1)) {
    t_vec<-convertVec2unitVec2(spinalPoints3D[i+1,]-spinalPoints3D[i,])
    u1<-convertVec2unitVec2(spinalPoints3D[i-1,]-spinalPoints3D[i,])
    # u1<-c(-1,0,0)
    u2<-t_vec
    psiTemp<-geodesicDistance(u1,u2)
    if(abs(psiTemp-pi)>10^-5){
      #formula from geodesicPathOnUnitSphere()
      n_vec<-1/sin(psiTemp)*(sin(pi/2)*u1+sin(psiTemp-pi/2)*u2)
      b_perb_vec<-convertVec2unitVec2(myCrossProduct(t_vec,n_vec))
      
      # frenetFramesGlobalCoordinate[,,i]<-as.SO3(rbind(t_vec,n_vec,b_perb_vec))
      frenetFramesGlobalCoordinate[,,i]<-rbind(t_vec,n_vec,b_perb_vec)
      
    }else{
      
      frenetFramesGlobalCoordinate[,,i]<-materialFramesGlobalCoordinate[,,i-1]
    }
  }
  frenetFramesGlobalCoordinate[,,numberOfFrames]<-materialFramesGlobalCoordinate[,,numberOfFrames]
  
  # open3d()
  # vectors3d(spinalPoints3D+t(frenetFramesGlobalCoordinate[1,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="blue", lwd=2)
  # vectors3d(spinalPoints3D+t(frenetFramesGlobalCoordinate[2,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="red", lwd=2)
  # vectors3d(spinalPoints3D+t(frenetFramesGlobalCoordinate[3,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="green", lwd=2)
  # decorate3d()
  
  #parent is the local twisting frame
  frenetFramesBasedOnLocalmaterialFrame<-array(NA,dim = c(3,3,numberOfFrames))
  for (i in 1:numberOfFrames) {
    frenetFramesBasedOnLocalmaterialFrame[,,i]<-rotateFrameToMainAxes_standard(myFrame = materialFramesGlobalCoordinate[,,i],
                                                                               vectors2rotate = frenetFramesGlobalCoordinate[,,i])
  }
  
  #parent is the previous twisting frame
  frenetFramesBasedOnParents<-array(NA,dim = c(3,3,numberOfFrames))
  frenetFramesBasedOnParents[,,1]<-materialFramesGlobalCoordinate[,,1]
  for (i in 2:numberOfFrames) {
    frenetFramesBasedOnParents[,,i]<-rotateFrameToMainAxes_standard(myFrame = materialFramesGlobalCoordinate[,,i-1],
                                                                    vectors2rotate = frenetFramesGlobalCoordinate[,,i])
  }
  
  #normal vectors
  normalVectorsGlobalCoordinate<-t(frenetFramesGlobalCoordinate[2,,])
  
  # theta is positive and equal to d_g(a_i,n_i)
  theta_angles<-rep(NA,numberOfFrames)
  for (i in 1:numberOfFrames) {
    theta_angles[i]<-calculate_theta(materialFrameBasedOnParent = materialFramesBasedOnParents[,,i])
  }
  
  phi_angles_bend<-rep(NA,numberOfFrames)
  for (i in 1:numberOfFrames) {
    u1<-materialFramesBasedOnParents[1,,i]
    u2<-c(1,0,0)
    phi_angles_bend[i]<-geodesicDistance(u1,u2)
  }
  
  psi_angles_roll<-rep(NA,numberOfFrames)
  for (i in 1:numberOfFrames) {
    psi_angles_roll[i]<-DCM2EA(materialFramesBasedOnParents[,,i],
                               EulerOrder = 'zyx')[3]
  }
  
  r_project_lengths<-rep(NA,numberOfFrames) #r_project is the projection of the CS on the normal
  for (i in 1:numberOfFrames) {
    r_project_lengths[i]<-calculate_r_project_Length(a = ellipseRadii_a[i],
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
  ##################################################################
  # cross-sections
  
  ellipseTemplate_2D<-ellipsoidGenerator_2D_2(center = c(0,0),
                                              a = ellipseRadii_a[1],
                                              b = ellipseRadii_b[1],
                                              n = ellipseResolution,
                                              n2 = 1) 
  
  ellipses_2D<-array(NA,c(dim(ellipseTemplate_2D),numberOfFrames))
  for (i in 1:numberOfFrames) {
    ellipses_2D[,,i]<-ellipsoidGenerator_2D_2(center = c(0,0),
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
    
    # plot3d(rbind(elipseIn3D,elipseIn3D[1,]),type = 'l',col='blue',expand = 10,box=FALSE,add = TRUE)
    # plot3d(rbind(spinalPoints3D[i,],elipseIn3D[1,]),type = 'l',col='blue',expand = 10,box=FALSE,add = TRUE)
    
    slicingEllipsoids[,,i]<-elipseIn3D
  }
  
  # normalsTips_at_CS_boundaries<-array(NA,dim = dim(normalVectorsGlobalCoordinate))
  # for (i in 1:numberOfFrames) {
  #   r_temp<-ellipse_Radius_InPolarCoordinate(a = ellipseRadii_a[i],
  #                                            b = ellipseRadii_b[i],
  #                                            theta = theta_angles[i])
  #   normalsTips_at_CS_boundaries[i,]<-spinalPoints3D[i,]+r_temp
  # }
  
  boundaryPoints<-matrix(aperm(slicingEllipsoids, c(1, 3, 2)), ncol = 3)
  
  
  #plot
  numberOfLayers<-20
  skeletalSheetPoints<-array(NA,dim = c(numberOfLayers*2+1,3,dim(slicingEllipsoids)[3]))
  for (i in 1:dim(slicingEllipsoids)[3]) {
    skeletalSheetPoints[,,i]<-generatePointsBetween2Points(slicingEllipsoids[1,,i],
                                                           slicingEllipsoids[ellipseResolution*2+1,,i],
                                                           numberOfPoints = numberOfLayers*2+1)
  }
  
  #plot
  if(plotting==TRUE){
    if(add==FALSE){
      open3d() 
    }
    for (i in 1:dim(slicingEllipsoids)[3]) {
      plot3d(rbind(slicingEllipsoids[,,i],slicingEllipsoids[1,,i]),type = 'l',col='black',expand = 10,box=FALSE,add = TRUE)
    }
    for (i in 1:dim(slicingEllipsoids)[1]) {
      plot3d(t(slicingEllipsoids[i,,]),type = 'l',col='blue',expand = 10,box=FALSE,add = TRUE)
    }
    #skeletal sheet
    for (i in 1:dim(skeletalSheetPoints)[1]) {
      plot3d(t(skeletalSheetPoints[i,,]),type = 'l',lwd=1,col='blue',expand = 10,box=FALSE,add = TRUE)
    }
    for (i in 1:dim(skeletalSheetPoints)[3]) {
      plot3d(skeletalSheetPoints[,,i],type = 'l',lwd=1,col='blue',expand = 10,box=FALSE,add = TRUE)
    }
    # # critical vectors
    # for (i in 1:dim(slicingEllipsoids)[3]) {
    #   plot3d(rbind(spinalPoints3D[i,],normalsTips_at_CS_boundaries[i,]),type = 'l',lwd=4,col='orange',expand = 10,box=FALSE,add = TRUE)
    #   # plot3d(rbind(colMeans(slicingEllipsoids[,,i]),normalsTips_at_CS_boundaries[i,]),type = 'l',lwd=4,col='red',expand = 10,box=FALSE,add = TRUE)
    # }
    for (i in 1:dim(slicingEllipsoids)[3]) {
      plot3d(rbind(spinalPoints3D[i,],tip_r_ProjectVectors[i,]),type = 'l',lwd=3.2,col='red',expand = 10,box=FALSE,add = TRUE)
      # plot3d(rbind(spinalPoints3D[i,],tip_r_MaxVectors[i,]),type = 'l',lwd=2,col='orange',expand = 10,box=FALSE,add = TRUE)
    }
    #frames
    vectors3d(spinalPoints3D+t(materialFramesGlobalCoordinate[1,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="blue", lwd=2) 
    vectors3d(spinalPoints3D+t(materialFramesGlobalCoordinate[2,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="red", lwd=2) 
    vectors3d(spinalPoints3D+t(materialFramesGlobalCoordinate[3,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="green", lwd=2) 
    
    #vectors3d(spinalPoints3D+t(frenetFramesGlobalCoordinate[1,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="black", lwd=2) 
    vectors3d(spinalPoints3D+t(frenetFramesGlobalCoordinate[2,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="black", lwd=2) 
    #vectors3d(spinalPoints3D+t(frenetFramesGlobalCoordinate[3,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="black", lwd=2) 
    
    decorate3d() 
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
# # example
# numberOfFrames<-50
# ellipseResolution<-4
# alpha, beta and gamma, angles are yaw, pitch and roll angles respectively
# EulerAngles_alpha<-rep(pi/10,numberOfFrames)
# EulerAngles_beta<-rep(pi/20,numberOfFrames)
# EulerAngles_gamma<-rep(pi/30,numberOfFrames)
# ellipseRadii_a<-runif(numberOfFrames,min = 2,max = 3)
# ellipseRadii_b<-runif(numberOfFrames,min = 1,max = 2)
# connectionsLengths<-runif(numberOfFrames-1,min = 3,max = 5)
# tube_test<-create_Elliptical_Tube(numberOfFrames=numberOfFrames,
#                        EulerAngles_alpha=EulerAngles_alpha,
#                        EulerAngles_beta=EulerAngles_beta,
#                        EulerAngles_gamma=EulerAngles_gamma,
#                        method = "basedOnEulerAngles",
#                        ellipseResolution=ellipseResolution,
#                        ellipseRadii_a=ellipseRadii_a,
#                        ellipseRadii_b=ellipseRadii_b,
#                        connectionsLengths=connectionsLengths,
#                        plotting=TRUE,
#                        add=FALSE)


# Converting p-values to color based on the spectrum
pvalue_to_color <- function(p_value,
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


# Plotting an ETRep
plot_Elliptical_Tube <- function(e_tube,
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
                                 userMatrix4plotOrientation=NA,
                                 plotSignificantFeatures=FALSE,
                                 p_values_v=NULL,
                                 p_values_psi=NULL,
                                 p_values_x=NULL,
                                 p_values_a=NULL,
                                 p_values_b=NULL,
                                 lwdSignificant=4,
                                 add=FALSE,
                                 mainTitle="") {
  
  numberOfFrames<-nrow(e_tube$spinalPoints3D)
  
  spinalPoints3D<-e_tube$spinalPoints3D
  materialFramesBasedOnParents<-e_tube$materialFramesBasedOnParents
  materialFramesGlobalCoordinate<-e_tube$materialFramesGlobalCoordinate
  frenetFramesBasedOnParents<-e_tube$frenetFramesBasedOnParents
  frenetFramesGlobalCoordinate<-e_tube$frenetFramesGlobalCoordinate
  ellipseRadii_a<-e_tube$ellipseRadii_a
  ellipseRadii_b<-e_tube$ellipseRadii_b
  slicingEllipsoids<-e_tube$slicingEllipsoids
  skeletalSheetPoints<-e_tube$skeletalSheetPoints
  r_project_lengths<-e_tube$r_project_lengths
  tip_r_MaxVectors<-e_tube$tip_r_MaxVectors
  tip_r_ProjectVectors<-e_tube$tip_r_ProjectVectors
  connectionsLengths<-e_tube$connectionsLengths
  
  
  if(length(connectionsLengths)==(numberOfFrames-1)){
    connectionsLengths<-c(0,connectionsLengths)
  }
  
  if(is.na(frameScaling)){
    frameScaling<-mean(connectionsLengths)/2
  }
  
  
  #plot
  if(add==FALSE){
    open3d() 
  }
  #spine
  if(plot_spine==TRUE){
    plot3d(spinalPoints3D,type = 'l',expand = 10,box=FALSE,add = TRUE)
  }
  #boundary
  if(plot_boundary==TRUE){
    for (i in 1:dim(slicingEllipsoids)[3]) {
      plot3d(rbind(slicingEllipsoids[,,i],slicingEllipsoids[1,,i]),type = 'l',col=colorBoundary,expand = 10,box=FALSE,add = TRUE)
    }
    for (i in 1:dim(slicingEllipsoids)[1]) {
      plot3d(t(slicingEllipsoids[i,,]),type = 'l',col=colorBoundary,expand = 10,box=FALSE,add = TRUE)
    } 
  }
  #skeletal sheet
  if(plot_skeletal_sheet==TRUE){
    for (i in 1:dim(skeletalSheetPoints)[1]) {
      plot3d(t(skeletalSheetPoints[i,,]),type = 'l',lwd=1,col=colSkeletalSheet,expand = 10,box=FALSE,add = TRUE)
    }
    for (i in 1:dim(skeletalSheetPoints)[3]) {
      plot3d(skeletalSheetPoints[,,i],type = 'l',lwd=1,col=colSkeletalSheet,expand = 10,box=FALSE,add = TRUE)
    } 
  }
  # # critical vectors
  # for (i in 1:dim(slicingEllipsoids)[3]) {
  #   plot3d(rbind(spinalPoints3D[i,],criticalVectorsTip[i,]),type = 'l',lwd=4,col='orange',expand = 10,box=FALSE,add = TRUE)
  #   # plot3d(rbind(colMeans(slicingEllipsoids[,,i]),criticalVectorsTip[i,]),type = 'l',lwd=4,col='red',expand = 10,box=FALSE,add = TRUE)
  # }
  
  if(plot_r_project==TRUE){
    for (i in 1:dim(slicingEllipsoids)[3]) {
      plot3d(rbind(spinalPoints3D[i,],tip_r_ProjectVectors[i,]),type = 'l',lty = 3,lwd=3.2,col='red',expand = 10,box=FALSE,add = TRUE)
      # plot3d(rbind(spinalPoints3D[i,],tip_r_MaxVectors[i,]),type = 'l',lwd=2,col='orange',expand = 10,box=FALSE,add = TRUE)
    }
  }
  
  if(plot_r_max==TRUE){
    for (i in 1:dim(slicingEllipsoids)[3]) {
      plot3d(rbind(spinalPoints3D[i,],tip_r_MaxVectors[i,]),type = 'l',lwd=2,col='orange',expand = 10,box=FALSE,add = TRUE)
    } 
  }
  
  if(plot_frames==TRUE){
    plot3d(spinalPoints3D,type = 'l',lwd=4,col='darkblue',expand = 10,box=FALSE,add = TRUE)
    #frames
    vectors3d(spinalPoints3D+frameScaling*t(materialFramesGlobalCoordinate[1,,]),origin = spinalPoints3D,headlength = 0.1*frameScaling,radius = frameScaling/10, col="blue", lwd=frameScaling) 
    vectors3d(spinalPoints3D+frameScaling*t(materialFramesGlobalCoordinate[2,,]),origin = spinalPoints3D,headlength = 0.1*frameScaling,radius = frameScaling/10, col="red", lwd=frameScaling) 
    vectors3d(spinalPoints3D+frameScaling*t(materialFramesGlobalCoordinate[3,,]),origin = spinalPoints3D,headlength = 0.1*frameScaling,radius = frameScaling/10, col="green", lwd=frameScaling) 
  }
  
  if(plot_normal_vec==TRUE){
    #vectors3d(spinalPoints3D+frameScaling*t(frenetFramesGlobalCoordinate[1,,]),origin = spinalPoints3D,headlength = 0.1*frameScaling,radius = frameScaling/10, col="black", lwd=frameScaling) 
    vectors3d(spinalPoints3D+frameScaling*t(frenetFramesGlobalCoordinate[2,,]),origin = spinalPoints3D,headlength = 0.1*frameScaling,radius = frameScaling/10, col="black", lwd=frameScaling) 
    #vectors3d(spinalPoints3D+frameScaling*t(frenetFramesGlobalCoordinate[3,,]),origin = spinalPoints3D,headlength = 0.1*frameScaling,radius = frameScaling/10, col="black", lwd=frameScaling) 
  }
  
  if(decorate==TRUE){
    decorate3d(main = mainTitle)  
  }
  
  if(plotSignificantFeatures==TRUE){
    
    if(!is.null(p_values_a)){
      colors<-pvalue_to_color(p_values_a)
      for (i in 1:numberOfFrames) {
        plot3d(rbind(slicingEllipsoids[,,i],slicingEllipsoids[1,,i]),type = 'l',
               lwd=lwdSignificant,col=colors[i],expand = 10,box=FALSE,add = TRUE)
      } 
    }
    if(!is.null(p_values_b)){
      colors<-pvalue_to_color(p_values_b)
      for (i in 1:numberOfFrames) {
        plot3d(rbind(slicingEllipsoids[,,i],slicingEllipsoids[1,,i]),type = 'l',
               lwd=lwdSignificant,col=colors[i],expand = 10,box=FALSE,add = TRUE)
      } 
    }
    if(!is.null(p_values_x)){
      colors<-pvalue_to_color(p_values_x)
      for (i in 2:numberOfFrames) {
        plot3d(rbind(spinalPoints3D[i-1,],
                     spinalPoints3D[i,]),type = 'l',
               lwd=lwdSignificant,col=colors[i],expand = 10,box=FALSE,add = TRUE)
      }
    }
    
    if(!is.null(p_values_psi)){
      colors<-pvalue_to_color(p_values_psi)
      for (i in 1:numberOfFrames) {
        plot3d(rbind(slicingEllipsoids[,,i],slicingEllipsoids[1,,i]),type = 'l',
               lwd=lwdSignificant,col=colors[i],expand = 10,box=FALSE,add = TRUE)
      } 
    }
    
    if(!is.null(p_values_v)){
      colors<-pvalue_to_color(p_values_v)
      for (i in 1:numberOfFrames) {
        plot3d(rbind(slicingEllipsoids[,,i],slicingEllipsoids[1,,i]),type = 'l',
               lwd=lwdSignificant,col=colors[i],expand = 10,box=FALSE,add = TRUE)
      } 
    }
    decorate3d(main = mainTitle)
  }
  
  if(!anyNA(userMatrix4plotOrientation)){
    par3d(userMatrix=userMatrix4plotOrientation)
  }
}

# Converting a tube to a vector in the high-dimensional space with hyperbolic boundary
tube2vectorsIn6DHyperbola <- function(tube) {
  
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
tube2vectorsIn6DCylinder <- function(tube) {
  
  vectorsIn6DHyperbola_tubes<-tube2vectorsIn6DHyperbola(tube = tube)
  
  vectorsIn6DCylinder_tubes<-t(apply(vectorsIn6DHyperbola_tubes, 
                                     MARGIN = 1,
                                     FUN = map6DhyperbolaTo6Dcylinder))
  
  return(vectorsIn6DCylinder_tubes)
}

# Converting the 6D hyperbola (i.e., space of a cross-section) to a convex 6D cylinder based on the swept skeletal coordinate system
map6DhyperbolaTo6Dcylinder <- function(v_gamma_x_a_b) {
  
  #gamma is the role angle
  
  v_In6DHyperbola<-v_gamma_x_a_b[c(1,2)]
  gamma<-v_gamma_x_a_b[3]
  x<-v_gamma_x_a_b[4]
  a<-v_gamma_x_a_b[5]
  b<-v_gamma_x_a_b[6]
  
  # calculate tangent vector
  t_vec<-convert_v_to_TangentVector(v = v_In6DHyperbola)
  
  # calculate yaw and pitch angles as alpha and beta
  c2s<-cartesian_to_spherical(v = t_vec)
  alpha<-c2s$phi
  beta<-c2s$theta
  materialFrame<-EA2DCM(EA = c(alpha, beta, gamma),
                        EulerOrder = 'zyx')
  
  #calculate theta
  theta<-calculate_theta(materialFrameBasedOnParent=materialFrame)
  
  # calculate r_project length
  r_project<-calculate_r_project_Length(a = a, b = b, theta = theta)
  
  # maximum possible value of r inside the unit circle
  v_vec_length<-norm(v_In6DHyperbola,type = '2')
  if(r_project<=x){ #i.e., if r_project<x then we can bend the frame up to pi/2 degree
    max_v_vec_length<-1
  }else{
    max_v_vec_length<-(x/r_project)
  }
  
  # maximum possible value of r
  ratio_v_vec_length<-v_vec_length/max_v_vec_length
  
  if(v_vec_length==0){
    v_In6DCylinder<-c(0,0)
  }else{
    v_In6DCylinder<-ratio_v_vec_length*convertVec2unitVec(v_In6DHyperbola) 
  }
  
  sweptSkeletalCoordinate<-c(v_In6DCylinder,gamma,x,a,b)
  
  return(sweptSkeletalCoordinate)
  
}

# Converting the convex 6D cylinder to 6D hyperbola (i.e., space of a cross-section) based on the swept skeletal coordinate system
map6DcylinderTo6Dhyperbola <-function(v_gamma_x_a_b_In6DCylinder) {
  
  v_In6DCylinder<-v_gamma_x_a_b_In6DCylinder[c(1,2)]
  gamma<-v_gamma_x_a_b_In6DCylinder[3]
  x<-v_gamma_x_a_b_In6DCylinder[4]
  a<-v_gamma_x_a_b_In6DCylinder[5]
  b<-v_gamma_x_a_b_In6DCylinder[6]
  
  # if(a<b){
  #   stop("Radius a must be grater than radius b!")
  # }
  
  # tangent vector
  t_vec<-convert_v_to_TangentVector(v = v_In6DCylinder)
  
  # calculate yaw and pitch angles as alpha and beta
  c2s<-cartesian_to_spherical(v = t_vec)
  alpha<-c2s$phi
  beta<-c2s$theta
  materialFrame<-EA2DCM(EA = c(alpha, beta, gamma),
                        EulerOrder = 'zyx')
  
  #calculate theta
  theta<-calculate_theta(materialFrameBasedOnParent=materialFrame)
  
  #length of the critical vector
  r_project<-calculate_r_project_Length(a = a,b = b,theta = theta)
  
  # maximum possible value of r
  if(r_project<=x){ #i.e., if r_project<x then we can bend the frame up to pi/2 degree
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
convertMatrixIn6DHyperbola2Tube <- function(matrixIn6DHyperbola) {
  
  numberOfFrames<-dim(matrixIn6DHyperbola)[1]
  
  # unit tangents
  unitTangentBasedOnParents<-t(apply(matrixIn6DHyperbola[,c(1,2)],
                                     MARGIN = 1,
                                     FUN = convert_v_to_TangentVector))
  
  gammas<-matrixIn6DHyperbola[,3]
  
  materialFramesBasedOnParents<-array(NA,dim=c(3,3,numberOfFrames))
  for (i in 1:numberOfFrames) {
    # calculate material frames by calculate yaw and pitch angles as alpha and beta
    t_vec<-unitTangentBasedOnParents[i,]
    c2s<-cartesian_to_spherical(v = t_vec)
    alphaTemp<-c2s$phi
    betaTemp<-c2s$theta
    gammaTemp<-gammas[i]
    materialFramesBasedOnParents[,,i]<-EA2DCM(EA = c(alphaTemp, betaTemp, gammaTemp),
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
                               plotting = FALSE,
                               add = FALSE)
  return(tube)
  
}

# Calculating the intrinsic mean ETRep
intrinsic_mean_tube <- function(tubes,
                                type="sizeAndShapeAnalysis",
                                plotting=TRUE) {
  
  numberOftubes<-length(tubes)
  
  if(type=="sizeAndShapeAnalysis"){
    
    # sort in a tensor
    vectorsIn6DCylinder_tubes<-array(NA,dim = c(dim(tube2vectorsIn6DCylinder(tubes[[1]])),numberOftubes))
    for (i in 1:numberOftubes) {
      vectorsIn6DCylinder_tubes[,,i]<-tube2vectorsIn6DCylinder(tube = tubes[[i]])
    }
    
    #mean along the third dimension of the array
    meanMatrix_In_6DCylinder<-apply(vectorsIn6DCylinder_tubes, c(1, 2), mean)
    
    meanMatrixIn6DHyperbola<-t(apply(meanMatrix_In_6DCylinder,
                                     MARGIN = 1,
                                     FUN = map6DcylinderTo6Dhyperbola))
    
    #convert mean matrix to a tube
    meantube<-convertMatrixIn6DHyperbola2Tube(matrixIn6DHyperbola=meanMatrixIn6DHyperbola)
    
    
  }else if(type=="shapeAnalysis"){
    
    scaledTubes<-list()
    for (i in 1:numberOftubes) {
      scaledTubes[[i]]<-scaleETRepToHaveTheSizeAsOne(tube = tubes[[i]])
    }
    
    # sort in a tensor
    vectorsIn6DCylinder_tubes<-array(NA,dim = c(dim(tube2vectorsIn6DCylinder(scaledTubes[[1]])),numberOftubes))
    for (i in 1:numberOftubes) {
      vectorsIn6DCylinder_tubes[,,i]<-tube2vectorsIn6DCylinder(tube = scaledTubes[[i]])
    }
    
    #mean along the third dimension of the array
    meanMatrix_In_6DCylinder<-apply(vectorsIn6DCylinder_tubes, c(1, 2), mean)
    
    meanMatrixIn6DHyperbola<-t(apply(meanMatrix_In_6DCylinder,
                                     MARGIN = 1,
                                     FUN = map6DcylinderTo6Dhyperbola))
    
    #convert mean matrix to a tube
    meantube<-convertMatrixIn6DHyperbola2Tube(matrixIn6DHyperbola=meanMatrixIn6DHyperbola)
    
  }else{
    stop("Please choose type as sizeAndShapeAnalysis or shapeAnalysis !")
  }
  
  if(plotting==TRUE){
    plot_Elliptical_Tube(meantube,
                         plot_frames = FALSE)
  }
  
  return(meantube)
  
}

# Calculating the intrinsic distance between two ETReps
intrinsic_Distance_Between2tubes <- function(tube1,tube2) {
  
  if(dim(tube1$frenetFramesBasedOnParents)[3]!=dim(tube2$frenetFramesBasedOnParents)[3]){
    stop('Number of cross-sections are not the same!')
  }
  
  vectorsIn6DCylinder_tube1<-tube2vectorsIn6DCylinder(tube = tube1)
  vectorsIn6DCylinder_tube2<-tube2vectorsIn6DCylinder(tube = tube2)
  
  euclideanDistance<-sum(sqrt(rowSums(vectorsIn6DCylinder_tube1-
                                        vectorsIn6DCylinder_tube2)^2))
  
  return(euclideanDistance)
  
}

# Calculating the non-intrinsic distance between two ETReps
nonIntrinsic_Distance_Between2tubes<- function(tube1,tube2) {
  
  if(dim(tube1$materialFramesBasedOnParents)[3]!=dim(tube2$materialFramesBasedOnParents)[3]){
    stop('Number of cross-sections are not the same!')
  }
  
  numberOfFrames<-dim(tube1$materialFramesBasedOnParents)[3]
  
  
  distancesMaterialFrames<-rep(NA,numberOfFrames)
  for (i in 1:numberOfFrames) {
    # q1_twist<-as.vector(as.Q4(as.SO3(tube1$materialFramesBasedOnParents[,,i])))
    # q2_twist<-as.vector(as.Q4(as.SO3(tube2$materialFramesBasedOnParents[,,i])))
    # distancesMaterialFrames[i]<-geodesicDistance(q1_twist,q2_twist)
    distancesMaterialFrames[i]<-rot.dist(as.SO3(tube1$materialFramesBasedOnParents[,,i]),
                                         as.SO3(tube2$materialFramesBasedOnParents[,,i]),
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
discretePathBetween2PointsIn_6D_Hyperbola <- function(point1,
                                                      point2,
                                                      numberOfpoints=10) {
  
  point1_SweptCoordinate<-map6DhyperbolaTo6Dcylinder(v_gamma_x_a_b = point1)
  point2_SweptCoordinate<-map6DhyperbolaTo6Dcylinder(v_gamma_x_a_b = point2)
  
  pathPointsIn6DCylinder<-generatePointsBetween2Points(point1 = point1_SweptCoordinate,
                                                       point2 = point2_SweptCoordinate,
                                                       numberOfPoints = numberOfpoints)
  
  pathPointsIn_6D_Hyperbola<-array(NA,dim = dim(pathPointsIn6DCylinder))
  for (i in 1:nrow(pathPointsIn6DCylinder)) {
    pathPointsIn_6D_Hyperbola[i,]<-map6DcylinderTo6Dhyperbola(v_gamma_x_a_b_In6DCylinder = 
                                                                pathPointsIn6DCylinder[i,])
  }
  
  return(pathPointsIn_6D_Hyperbola)
}

# Transformation between two ETReps based on the intrinsic approach
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
    
    vectorsIn6DHyperbola_tube1<-tube2vectorsIn6DHyperbola(tube = tube1)
    vectorsIn6DHyperbola_tube2<-tube2vectorsIn6DHyperbola(tube = tube2)
    
    pathsBetween2vectorsIn6DHyperbola<-array(NA,dim = c(numberOfSteps,6,numberOfFrames))
    for (i in 1:numberOfFrames) {
      p1<-vectorsIn6DHyperbola_tube1[i,]
      p2<-vectorsIn6DHyperbola_tube2[i,]
      if(norm(p1-p2,type = '2')==0){
        pathsBetween2vectorsIn6DHyperbola[,,i]<-matrix(rep(p1,numberOfSteps),ncol = length(p1),byrow = TRUE)
      }else{
        pathsBetween2vectorsIn6DHyperbola[,,i]<-discretePathBetween2PointsIn_6D_Hyperbola(
          point1=p1,
          point2=p2,
          numberOfpoints=numberOfSteps)
      }
    }
    
    unitTangentBasedOnParents4AllSamples<-array(NA,dim = c(numberOfFrames,3,numberOfSteps))
    for (j in 1:numberOfSteps) {
      for (i in 1:numberOfFrames) {
        unitTangentBasedOnParents4AllSamples[i,,j]<-
          convert_v_to_TangentVector(v =pathsBetween2vectorsIn6DHyperbola[j,c(1,2),i])
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
        c2s<-cartesian_to_spherical(v = t_vec_temp)
        alphaTemp<-c2s$phi
        betaTemp<-c2s$theta
        materialFramesBasedOnParents_Steps[,,i,k]<-EA2DCM(EA = c(alphaTemp, betaTemp, gammaTemp),
                                                          EulerOrder = 'zyx')
      }
    }
    materialFramesBasedOnParents_Steps[,,numberOfFrames,]<-
      materialFramesBasedOnParents_Steps[,,numberOfFrames-1,]
    
    # # #plot frames of a step
    # step<-2
    # vectors3d(t(materialFramesBasedOnParents_Steps[1,,,step]),color="blue")
    # vectors3d(t(materialFramesBasedOnParents_Steps[2,,,step]),color="red")
    # vectors3d(t(materialFramesBasedOnParents_Steps[3,,,step]),color="green")
    
    #connections' lengths steps
    connectionsLengths_steps<-t(pathsBetween2vectorsIn6DHyperbola[,4,])
    # connectionsLengths_steps<-connectionsLengths_stepsTemp[2:nrow(connectionsLengths_stepsTemp),]
    
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
                                         plotting = FALSE,
                                         add = FALSE)
    }
    
  }else if(type=="shapeAnalysis"){
    
    numberOfFrames<-dim(tube1$materialFramesBasedOnParents)[3]
    
    # NB! scaling does not effect the frames (i.e., the v vectors and roll angles of the scaled tube is the same as in the original tube) 
    scaled_tube1<-scaleETRepToHaveTheSizeAsOne(tube = tube1)
    scaled_tube2<-scaleETRepToHaveTheSizeAsOne(tube = tube2)
    
    # insert samples in 6D cylinder
    vectorsIn6DCylinder_tube1<-tube2vectorsIn6DCylinder(tube = scaled_tube1)
    vectorsIn6DCylinder_tube2<-tube2vectorsIn6DCylinder(tube = scaled_tube2)
    
    # path on small cylinder
    v_psiAngleRoll_tube1<-vectorsIn6DCylinder_tube1[,1:3]
    v_psiAngleRoll_tube2<-vectorsIn6DCylinder_tube2[,1:3]
    
    v_psiAngleRoll_steps<-array(NA,dim = c(numberOfSteps,3,numberOfFrames))
    for (i in 1:numberOfFrames) {
      v_psiAngleRoll_steps[,,i]<-generatePointsBetween2Points(point1 = v_psiAngleRoll_tube1[i,],
                                                              point2 = v_psiAngleRoll_tube2[i,],
                                                              numberOfPoints = numberOfSteps)
    }
    
    # path on the hyper-plane n.w=d where d is the space's dimension
    w1<-c(scaled_tube1$connectionsLengths,scaled_tube1$ellipseRadii_a,scaled_tube1$ellipseRadii_b)
    w2<-c(scaled_tube2$connectionsLengths,scaled_tube2$ellipseRadii_a,scaled_tube2$ellipseRadii_b)
    
    pathBetween_w1_w2<-generatePointsBetween2Points(w1,w2,numberOfPoints = numberOfSteps)
    
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
                                          FUN = map6DcylinderTo6Dhyperbola))
    }
    
    tubes<-list()
    for (j in 1:numberOfSteps) {
      
      tubes[[j]]<-convertMatrixIn6DHyperbola2Tube(matrixIn6DHyperbola=
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

# Transformation between two ETReps based on the non-intrinsic approach
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
    
    q1_twist<-as.Q4(as.SO3(tube1$materialFramesBasedOnParents[,,i]))
    q2_twist<-as.Q4(as.SO3(tube2$materialFramesBasedOnParents[,,i]))
    
    if(norm(as.vector(q1_twist)-as.vector(q2_twist),type = '2')<10^-6){
      for (j in 1:numberOfSteps) {
        materialFramesBasedOnParents_Steps[,,i,j]<-as.SO3(q2_twist) 
      }
    }else{
      q4pointsOnAGeodesic_twist<-geodesicPathOnUnitSphere(as.vector(q1_twist),
                                                          as.vector(q2_twist),
                                                          numberOfneededPoints = numberOfSteps)
      for (j in 1:numberOfSteps) {
        materialFramesBasedOnParents_Steps[,,i,j]<-as.SO3(as.Q4(q4pointsOnAGeodesic_twist[j,])) 
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
    
    scaled_tube1<-scaleETRepToHaveTheSizeAsOne(tube = tube1)
    scaled_tube2<-scaleETRepToHaveTheSizeAsOne(tube = tube2)
    
    u_1<-c(scaled_tube1$ellipseRadii_a,scaled_tube1$ellipseRadii_b,scaled_tube1$connectionsLengths)
    u_2<-c(scaled_tube2$ellipseRadii_a,scaled_tube2$ellipseRadii_b,scaled_tube2$connectionsLengths)
    
    geodesicPathBetween_u1_u2<-geodesicPathOnUnitSphere(point1 = u_1,
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
                                       plotting = FALSE,
                                       add = TRUE)
  }
  
  
  #plot tubes
  if(plotting==TRUE){
    for (j in 1:numberOfSteps) {
      plot_Elliptical_Tube(e_tube = tubes[[j]],
                           plot_r_project = FALSE,
                           plot_r_max = FALSE,
                           colorBoundary=colorBoundary,
                           add = add)
    } 
  }
  
  return(tubes)
  
  
}
# nonIntrinsic_Transformation_Elliptical_Tubes(tube1 = e_Tubes_simulated[[1]],
#                                           tube2 = e_Tubes_simulated[[7]],
#                                           numberOfSteps = 4,
#                                           plotting = TRUE,
#                                           add = FALSE)


# Calculating the mean ETRep based on the non-intrinsic approach
nonIntrinsic_mean_tube <- function(tubes,
                                   type ="sizeAndShapeAnalysis",
                                   plotting=TRUE) {
  
  if(type == "shapeAnalysis"){
    cat("\n shapeAnalysis \n")
    for (i in 1:length(tubes)) {
      tubes[[i]]<-scaleETRepToHaveTheSizeAsOne(tube =tubes[[i]] ,plotting = FALSE)
    }
  }else if(type == "sizeAndShapeAnalysis"){
    cat("\n sizeAndShapeAnalysis \n")
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
    tempVec<-mean(as.SO3(t(vectorizedMaterialFramesBasedOnParents[i,,])),type = 'projected')
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
simulate_etube <- function(referenceTube,
                           numberOfSimulation,
                           sd_v=10^-10,
                           sd_psi=10^-10,
                           sd_x=10^-10,
                           sd_a=10^-10,
                           sd_b=10^-10,
                           rangeSdScale=c(1,2),
                           plotting=TRUE) {
  
  
  referenceTube_scaled<-scaleETRepToHaveTheSizeAsOne(referenceTube)
  
  tubeIn6DCylinder<-tube2vectorsIn6DCylinder(referenceTube_scaled)
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
        temp<-convertVec2unitVec(c(w1,w2))*0.9999
        w1<-temp[1]
        w1<-temp[2]
      }
      
      tubeSimulatedTempIn6DCylinder[i,]<-c(w1,w2,w3,w4,w5,w6)
    }
    tubeSimulatedTempIn6DCylinder[1,1:3]<-0
    
    matrixIn6DHyperbola<-t(apply(tubeSimulatedTempIn6DCylinder,
                                 MARGIN = 1,
                                 FUN = map6DcylinderTo6Dhyperbola))
    
    kappa_total_scale<-runif(n = 1,min = rangeSdScale[1],max = rangeSdScale[2])
    
    matrixIn6DHyperbola[,3:6]<-matrixIn6DHyperbola[,3:6]*kappa_total_scale
    
    #convert mean matrix to a tube
    simulatedTubes[[j]]<-convertMatrixIn6DHyperbola2Tube(matrixIn6DHyperbola=matrixIn6DHyperbola)
  }
  
  if(plotting==TRUE){
    color_spectrum <- colorRampPalette(c("lightblue","darkblue"))
    colors <- color_spectrum(numberOfSimulation)
    open3d()
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
plotProcTube <- function(boundaryPoints,
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
  
  #open3d()
  for (i in 1:length(list_of_rows)) {
    plot3d(ellipses[[i]],type = 'l',col=colorBoundary,expand = 10,box=FALSE,add = TRUE)
  }
  for (i in 1:d) {
    ith_Points <- do.call(rbind, lapply(ellipses, function(x) x[i, ]))
    plot3d(ith_Points ,type = 'l',col=colorBoundary,expand = 10,box=FALSE,add = TRUE)
  }
  plot3d(centroids,type = 'l',col=colorSpine,lwd=3,expand = 10,box=FALSE,add = TRUE)
  #decorate3d()
}


# Identifying cross-sections of an e-tube with non-local intersection
tubeCrossSetionsIndicesWith_NonLocal_SelfIntersections <- function(tube) {
  
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
      
      intersectionPointsTemp<-intersectionPointsBetween2Ellipses_In3D(ellipse1 = ellipse1,ellipse2 = ellipse2,plotting = FALSE)
      
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
# test1<-tubeCrossSetionsIndicesWith_NonLocal_SelfIntersections(tube = nonIntrinsicTubes[[2]])
# test1$criticalElipses_index
# plot_Elliptical_Tube(nonIntrinsicTubes[[2]])
# plot3d(test1$intersectionPoints,type = 's',radius = 0.2,col='black',expand = 10,box=FALSE,add = TRUE)

# Non-intrinsic transformation between two ETReps by cross-sectional adjustment
nonIntrinsic_Transformation_Elliptical_Tubes_Without_SelfIntersection <- function(tube1,
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
    #plot_Elliptical_Tube(e_tube = tubeTemp)
    
    if(removeNonLocalSingularity==TRUE){
      #remove non-local intersections
      indicesOfCriticalNonLocalIntersections<-tubeCrossSetionsIndicesWith_NonLocal_SelfIntersections(tube = tubeTemp)$criticalElipses_index
      while(length(indicesOfCriticalNonLocalIntersections)>=1){
        cat("Number of nonlocal intersection",length(indicesOfCriticalNonLocalIntersections),"\n")
        tubeTemp$ellipseRadii_a[indicesOfCriticalNonLocalIntersections]<-scalingFactor*tubeTemp$ellipseRadii_a[indicesOfCriticalNonLocalIntersections]
        tubeTemp$ellipseRadii_b[indicesOfCriticalNonLocalIntersections]<-scalingFactor*tubeTemp$ellipseRadii_b[indicesOfCriticalNonLocalIntersections]
        
        tubeTemp<-create_Elliptical_Tube(numberOfFrames = dim(tubeTemp$materialFramesBasedOnParents)[3],
                                         method = "basedOnMaterialFrames",
                                         materialFramesBasedOnParents = tubeTemp$materialFramesBasedOnParents,
                                         ellipseRadii_a = tubeTemp$ellipseRadii_a,
                                         ellipseRadii_b = tubeTemp$ellipseRadii_b,
                                         connectionsLengths = tubeTemp$connectionsLengths,
                                         plotting = FALSE)
        indicesOfCriticalNonLocalIntersections<-tubeCrossSetionsIndicesWith_NonLocal_SelfIntersections(tube = tubeTemp)$criticalElipses_index
      }
    }
    plot_Elliptical_Tube(e_tube = tubeTemp,
                         plot_boundary = TRUE,plot_r_max = FALSE,plot_r_project = FALSE,
                         plot_frames = TRUE,
                         plot_normal_vec = FALSE,plot_skeletal_sheet = FALSE,
                         decorate = FALSE)
    
    tubes[[i]]<-tubeTemp
    
  }
  
  
  #plot tubes
  if(plotting==TRUE){
    for (i in 1:length(tubes)) {
      plot_Elliptical_Tube(e_tube = tubes[[i]],
                           plot_boundary = TRUE,plot_r_max = FALSE,plot_r_project = FALSE,
                           plot_frames = TRUE,
                           plot_normal_vec = FALSE,plot_skeletal_sheet = FALSE,
                           decorate = FALSE)
    }
  }
  
  return(tubes)
  
}

# Check the legality of an ETRep based on the RCC
check_Tube_Legality <- function(tube) {
  numberOfFrames<-nrow(tube$spinalPoints3D)
  criticalIndices_RCC<-which(tube$r_project_lengths>tube$r_max_lengths)
  criticalIndices_Radii<-which(tube$ellipseRadii_a<tube$ellipseRadii_b)
  if(length(criticalIndices_Radii)>0){
    cat("The tube is not valid as it is not elliptical! \n")
    cat("Critical cross-sections that a<b are:",criticalIndices_Radii,"\n")
    return(FALSE)
  }else if(length(criticalIndices_RCC)>0){
    cat("The tube is not valid as it violates the RCC! \n")
    cat("Critical cross-sections are:",criticalIndices_RCC,"\n")
    return(FALSE)
  }else{
    cat("The tube is a valid elliptical tube and it satisfies the RCC! \n")
    return(TRUE)
  }
}


# Fitting radial spokes based on parallel slicing regarding radial distance analysis of Supplementary Materials
fitRadialVectorsModelBasedOnParallelSlicing <- function(PDM,
                                                        polyMatrix,
                                                        nunmberOfSlices,
                                                        numberOFRadialSpokes,
                                                        plotting=TRUE,
                                                        colorRadialVectors="blue") {
  
  verts <- rbind(t(as.matrix(PDM)),1)
  trgls <- as.matrix(t(polyMatrix))
  tmesh <- tmesh3d(verts, trgls)
  tmesh <- vcgUpdateNormals(tmesh)
  # shade3d(tmesh, col="white",alpha=0.2)  #surface mesh
  
  #remeshing to increase the number of triangles by reducing the voxelSize
  remeshedMesh<-vcgUniformRemesh(tmesh,voxelSize = 0.5)
  # print(dim(tmesh$vb))
  # print(dim(remeshedMesh$vb))
  tmeshSmooth<-vcgUpdateNormals(remeshedMesh)
  
  pointsTest<-vert2points(tmeshSmooth)
  slicesAlongXaxis<-seq(min(pointsTest[,2]),max(pointsTest[,2]),length.out=nunmberOfSlices)
  
  # we use asymmetric circles to avoid antipodal vectors
  tempCircle<-sphereGenerator_2D(center = c(0,0),r = 1,
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
    
    circleMesh3D<-as.mesh3d(circleIn3D)
    
    normalsTemp<-circleIn3D-tempRadialSpokesTails
    
    circleMesh3D$normals<-rbind(apply(normalsTemp,FUN = convertVec2unitVec,MARGIN = 1),
                                rep(1,nrow(circleIn3D)))
    
    
    intersections<-vcgRaySearch(circleMesh3D,mesh = tmesh)
    tempRadialSpokesTips<-vert2points(intersections)
    
    radialSpokesTails<-rbind(radialSpokesTails,tempRadialSpokesTails)
    radialSpokesTips<-rbind(radialSpokesTips,tempRadialSpokesTips)
    
  }
  
  if(plotting==TRUE){
    open3d()
    shade3d(tmesh,col="white",alpha=0.2)
    plot3d(slicesCentroids,type="s",radius = 0.2,col = colorRadialVectors,expand = 10,box=FALSE,add = TRUE)
    plot3d(slicesCentroids,type="l",lwd=4,col = colorRadialVectors,expand = 10,box=FALSE,add = TRUE)
    
    #plot slicing-planes
    rangeSquare<-10
    square<-rbind(c(-rangeSquare,0,-rangeSquare),c(-rangeSquare,0,rangeSquare),c(rangeSquare,0,rangeSquare),c(rangeSquare,0,-rangeSquare))
    for (i in 1:nrow(slicesCentroids)) {
      slicingPlaneTemp<-square+matrix(rep(slicesCentroids[i,],4),ncol = 3,byrow = TRUE)
      plot3d(rbind(slicingPlaneTemp,slicingPlaneTemp[1,]),type="l",lwd=2,col = "grey",expand = 10,box=FALSE,add = TRUE)
    }
    
    vectors3d(radialSpokesTips,
              origin = radialSpokesTails,headlength = 0.1,radius = 1/10, col=colorRadialVectors, lwd=2)
    
    decorate3d()
  }
  
  result<-list("tmesh"=tmesh, 
               "radialSpokesTails"=radialSpokesTails,
               "radialSpokesTips"=radialSpokesTips,
               "slicesCentroids"=slicesCentroids)
  
  return(result)
}


