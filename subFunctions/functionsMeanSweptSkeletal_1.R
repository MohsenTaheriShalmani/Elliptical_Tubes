# r project
calculate_r_project_Length <- function(a,b,theta) {
  
  t_max<-atan(-b/a*tan(theta))
  r_project_length<-abs(a*cos(t_max)*cos(theta)-b*sin(t_max)*sin(theta))
  
  return(r_project_length)
}


#NB! tangent vectors provides the Frenet frames automatically
create_Elliptical_Tube<- function(numberOfFrames,
                                  method,
                                  twistingFramesBasedOnParents=NA,
                                  EulerAngles_alpha=0,
                                  EulerAngles_beta=0,
                                  EulerAngles_gamma=0,
                                  ellipseResolution=4,
                                  ellipseRadii_a,
                                  ellipseRadii_b,
                                  connectionsLengths,
                                  plotting=TRUE,
                                  add=FALSE) {
  
  if(length(connectionsLengths)==(numberOfFrames-1)){
    connectionsLengths<-c(0,connectionsLengths)
  }
  
  I<-diag(3)
  
  if(method=="basedOnEulerAngles"){
    # I_tilde<-matrix(c(0,0,1,1,0,0,0,1,0),nrow = 3,byrow = T)
    twistingFramesBasedOnParents<-array(NA,dim = c(3,3,numberOfFrames))
    twistingFramesBasedOnParents[,,1]<-I
    for (i in 2:numberOfFrames) {
      R1<-EA2DCM(c(EulerAngles_alpha[i],EulerAngles_beta[i],EulerAngles_gamma[i]))
      twistingFramesBasedOnParents[,,i]<-I%*%R1
    } 
    
  }else if(method=="basedOnTwistingFrames" & 
           !any(is.na(twistingFramesBasedOnParents &
                      dim(twistingFramesBasedOnParents)[3]==numberOfFrames))){
    twistingFramesBasedOnParents<-twistingFramesBasedOnParents
  }else{
    stop("Please specify the method!")
  }
  
  # open3d()
  # vectors3d(1.5*diag(3))
  # vectors3d(t(twistingFramesBasedOnParents[1,,]),col="blue")
  # vectors3d(t(twistingFramesBasedOnParents[2,,]),col="red")
  # vectors3d(t(twistingFramesBasedOnParents[3,,]),col="green")

  framesCenters<-1:numberOfFrames
  framesParents<-c(1,1:(numberOfFrames-1))
  
  twistingFramesGlobalCoordinate<-array(NA,dim = dim(twistingFramesBasedOnParents))
  twistingFramesGlobalCoordinate[,,1]<-I
  for (k in 2:numberOfFrames) {
    parent_Index<-framesParents[k]
    child_Index<-framesCenters[k]
    updatedParent<-twistingFramesGlobalCoordinate[,,parent_Index]
    twistingFramesGlobalCoordinate[,,child_Index]<-
      rotateFrameToMainAxesAndRotateBack_standard(myFrame = updatedParent,
                                                  vectorsInMainAxes = twistingFramesBasedOnParents[,,child_Index])
  }
  
  # spinal points
  spinalPoints3D<-array(NA,dim = c(numberOfFrames,3))
  spinalPoints3D[1,]<-c(0,0,0)
  for (i in 1:(numberOfFrames-1)) {
    spinalPoints3D[i+1,]<-spinalPoints3D[i,]+
      connectionsLengths[i+1]*twistingFramesGlobalCoordinate[1,,i]
  }
  
  # open3d()
  # vectors3d(spinalPoints3D+t(twistingFramesGlobalCoordinate[1,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="blue", lwd=2)
  # vectors3d(spinalPoints3D+t(twistingFramesGlobalCoordinate[2,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="red", lwd=2)
  # vectors3d(spinalPoints3D+t(twistingFramesGlobalCoordinate[3,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="green", lwd=2)
  # decorate3d()

  ##################################################################
  ##################################################################
  #Frenet frames
  
  frenetFramesGlobalCoordinate<-array(NA,dim = c(3,3,numberOfFrames))
  frenetFramesGlobalCoordinate[,,1]<-twistingFramesGlobalCoordinate[,,1]
  for (i in 2:(numberOfFrames-1)) {
    b_vec<-convertVec2unitVec2(spinalPoints3D[i+1,]-spinalPoints3D[i,])
    u1<-convertVec2unitVec2(spinalPoints3D[i-1,]-spinalPoints3D[i,])
    # u1<-c(-1,0,0)
    u2<-b_vec
    thetaTemp<-geodesicDistance(u1,u2)
    if(abs(thetaTemp-pi)>10^-5){
      #formula from geodesicPathOnUnitSphere()
      N_vec<-1/sin(thetaTemp)*(sin(pi/2)*u1+sin(thetaTemp-pi/2)*u2)  
      b_perb_vec<-convertVec2unitVec2(myCrossProduct(b_vec,N_vec))
      
      # frenetFramesGlobalCoordinate[,,i]<-as.SO3(rbind(b_vec,N_vec,b_perb_vec))
      frenetFramesGlobalCoordinate[,,i]<-rbind(b_vec,N_vec,b_perb_vec)
      
    }else{
      
      frenetFramesGlobalCoordinate[,,i]<-twistingFramesGlobalCoordinate[,,i-1]
    }
  }
  frenetFramesGlobalCoordinate[,,numberOfFrames]<-twistingFramesGlobalCoordinate[,,numberOfFrames]
  
  # open3d()
  # vectors3d(spinalPoints3D+t(frenetFramesGlobalCoordinate[1,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="blue", lwd=2)
  # vectors3d(spinalPoints3D+t(frenetFramesGlobalCoordinate[2,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="red", lwd=2)
  # vectors3d(spinalPoints3D+t(frenetFramesGlobalCoordinate[3,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="green", lwd=2)
  # decorate3d()

  
  #parent is the local twisting frame
  frenetFramesBasedOnLocalTwistingFrame<-array(NA,dim = c(3,3,numberOfFrames))
  for (i in 1:numberOfFrames) {
    frenetFramesBasedOnLocalTwistingFrame[,,i]<-rotateFrameToMainAxes_standard(myFrame = twistingFramesGlobalCoordinate[,,i],
                                                                    vectors2rotate = frenetFramesGlobalCoordinate[,,i])
  }
  
  #parent is the previous twisting frame
  frenetFramesBasedOnParents<-array(NA,dim = c(3,3,numberOfFrames))
  frenetFramesBasedOnParents[,,1]<-twistingFramesGlobalCoordinate[,,1]
  for (i in 2:numberOfFrames) {
    frenetFramesBasedOnParents[,,i]<-rotateFrameToMainAxes_standard(myFrame = twistingFramesGlobalCoordinate[,,i-1],
                                                                    vectors2rotate = frenetFramesGlobalCoordinate[,,i])
  }
  
  ##################################################################
  ##################################################################
  # cross-sections and critical vector
  
  criticalVectors_2D<-t(frenetFramesBasedOnLocalTwistingFrame[2,2:3,])
  
  
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
  
  
  tipOfCriticalVectors2D<-array(NA,dim = c(numberOfFrames,2))
  for (i in 1:numberOfFrames) {
    # tipOfCriticalVectors2D[i,]<-cutAndStretch_OneSpokeToMesh_2D(rayOrigin = c(0,0),
    #                                                             rayDirection = criticalVectors_2D[i,],
    #                                                             Mesh2D = rbind(ellipses_2D[,,i],ellipses_2D[,,i][1,])) 
    
    a_temp<-ellipseRadii_a[i]
    b_temp<-ellipseRadii_b[i]
    t_temp<-geodesicDistance(c(1,0),criticalVectors_2D[i,])
    
    r_temp<-ellipse_Radius_InPolarCoordinate(a = a_temp,b = b_temp,theta = t_temp)
    
    tipOfCriticalVectors2D[i,]<-r_temp*convertVec2unitVec2(criticalVectors_2D[i,])
    
  }
  # CS_number<-50
  # plot(rbind(ellipses_2D[,,CS_number],ellipses_2D[,,CS_number][1,]),type = 'l',xlim = c(-ellipseRadii_a[CS_number],ellipseRadii_a[CS_number]),ylim = c(-ellipseRadii_a[CS_number],ellipseRadii_a[CS_number]),xlab = '',ylab = '')
  # lines(rbind(c(0,0),tipOfCriticalVectors2D[CS_number,]))
  
  
  slicingEllipsoids<-array(NA,dim = c(nrow(ellipseTemplate_2D),3,nrow(spinalPoints3D)))
  criticalVectorsTip<-array(NA,dim = dim(spinalPoints3D))
  I<-diag(3)
  for (i in 1:nrow(spinalPoints3D)) {

    ellipseIn3DTemp<-cbind(rep(0,nrow(ellipses_2D[,,i])),ellipses_2D[,,i])
    criticalVector3DTemp<-c(0,tipOfCriticalVectors2D[i,])
    
    # rotate ellipse
    R1<-rotMat(I[1,],twistingFramesGlobalCoordinate[1,,i])
    if(anyNA(R1)){
      R1<-I
    }
    rotatedFrame1<-I%*%t(R1)
    R2<-rotMat(rotatedFrame1[2,],twistingFramesGlobalCoordinate[2,,i])
    if(anyNA(R2)){
      R2<-I
    }
    
    elipseIn3D<-ellipseIn3DTemp%*%t(R2%*%R1)
    criticalVector3D<-criticalVector3DTemp%*%t(R2%*%R1)
    
    # translate
    elipseIn3D<-elipseIn3D+matrix(rep(spinalPoints3D[i,],nrow(elipseIn3D)),ncol = 3,byrow = TRUE)
    criticalVector3D<-criticalVector3D+spinalPoints3D[i,]
    
    # plot3d(rbind(elipseIn3D,elipseIn3D[1,]),type = 'l',col='blue',expand = 10,box=FALSE,add = TRUE)
    # plot3d(rbind(spinalPoints3D[i,],elipseIn3D[1,]),type = 'l',col='blue',expand = 10,box=FALSE,add = TRUE)
    # plot3d(rbind(spinalPoints3D[i,],criticalVector3D),type = 'l',col='red',expand = 10,box=FALSE,add = TRUE)
    
    slicingEllipsoids[,,i]<-elipseIn3D
    criticalVectorsTip[i,]<-criticalVector3D
    
  }
  
  # theta_angles<-rep(NA,numberOfFrames)
  # for (i in 1:numberOfFrames) {
  #   u1<-convertVec2unitVec2(tipOfCriticalVectors2D[i,]-c(0,0))
  #   u2<-c(1,0)
  #   theta_angles[i]<-clockwiseAngle(u2,u1)
  # }
  
  
  # projection of the tangent vectors to the plane of the previous frames
  tangentVectorsBasedOnParentFrames<-t(twistingFramesBasedOnParents[1,,])
  projectedVectors<-tangentVectorsBasedOnParentFrames[,2:3]
  # twisting vector
  twisting_UnitVectors<-t(apply(t(frenetFramesBasedOnLocalTwistingFrame[2,2:3,]),
                                MARGIN = 1,convertVec2unitVec2))
  
  theta_angles<-rep(NA,numberOfFrames)
  for (i in 1:numberOfFrames) {
    theta_angles[i]<-clockwiseAngle(c(1,0),twisting_UnitVectors[i,]) 
  }
  
  phi_angles_bend<-rep(NA,numberOfFrames)
  for (i in 1:numberOfFrames) {
    u1<-twistingFramesBasedOnParents[1,,i]
    u2<-c(1,0,0)
    phi_angles_bend[i]<-geodesicDistance(u1,u2)
  }
  
  r_project_lengths<-rep(NA,numberOfFrames)
  for (i in 1:numberOfFrames) {
    r_project_lengths[i]<-calculate_r_project_Length(a = ellipseRadii_a[i],
                                                     b = ellipseRadii_b[i],
                                                     theta = theta_angles[i]) 
  }
  
  r_max_lengths<-abs(connectionsLengths/sin(phi_angles_bend))
  r_max_lengths[1]<-r_project_lengths[1]
  
  tip_r_ProjectVectors<-array(NA,dim = dim(criticalVectorsTip))
  tip_r_MaxVectors<-array(NA,dim = dim(criticalVectorsTip))
  for (i in 1:numberOfFrames) {
    u<-convertVec2unitVec2(criticalVectorsTip[i,]-spinalPoints3D[i,])
    tip_r_ProjectVectors[i,]<-r_project_lengths[i]*u+spinalPoints3D[i,]
    tip_r_MaxVectors[i,]<-r_max_lengths[i]*u+spinalPoints3D[i,]
  }
  
  
  numberOfLayers<-20
  skeletalSheetPoints<-array(NA,dim = c(numberOfLayers*2+1,3,dim(slicingEllipsoids)[3]))
  for (i in 1:dim(slicingEllipsoids)[3]) {
    skeletalSheetPoints[,,i]<-generatePointsBetween2Points(slicingEllipsoids[1,,i],
                                                           slicingEllipsoids[9,,i],
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
    #   plot3d(rbind(spinalPoints3D[i,],criticalVectorsTip[i,]),type = 'l',lwd=4,col='orange',expand = 10,box=FALSE,add = TRUE)
    #   # plot3d(rbind(colMeans(slicingEllipsoids[,,i]),criticalVectorsTip[i,]),type = 'l',lwd=4,col='red',expand = 10,box=FALSE,add = TRUE)
    # }
    for (i in 1:dim(slicingEllipsoids)[3]) {
      plot3d(rbind(spinalPoints3D[i,],tip_r_ProjectVectors[i,]),type = 'l',lwd=3.2,col='red',expand = 10,box=FALSE,add = TRUE)
      # plot3d(rbind(spinalPoints3D[i,],tip_r_MaxVectors[i,]),type = 'l',lwd=2,col='orange',expand = 10,box=FALSE,add = TRUE)
    }
    #frames
    vectors3d(spinalPoints3D+t(twistingFramesGlobalCoordinate[1,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="blue", lwd=2) 
    vectors3d(spinalPoints3D+t(twistingFramesGlobalCoordinate[2,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="red", lwd=2) 
    vectors3d(spinalPoints3D+t(twistingFramesGlobalCoordinate[3,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="green", lwd=2) 
    
    #vectors3d(spinalPoints3D+t(frenetFramesGlobalCoordinate[1,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="black", lwd=2) 
    vectors3d(spinalPoints3D+t(frenetFramesGlobalCoordinate[2,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="black", lwd=2) 
    #vectors3d(spinalPoints3D+t(frenetFramesGlobalCoordinate[3,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="black", lwd=2) 
    
    
    decorate3d() 
  }
  
  out<-list("spinalPoints3D"=spinalPoints3D,
            "twistingFramesBasedOnParents"=twistingFramesBasedOnParents,
            "twistingFramesGlobalCoordinate"=twistingFramesGlobalCoordinate,
            "frenetFramesGlobalCoordinate"=frenetFramesGlobalCoordinate,
            "frenetFramesBasedOnParents"=frenetFramesBasedOnParents,
            "ellipseRadii_a"=ellipseRadii_a,
            "ellipseRadii_b"=ellipseRadii_b,
            "connectionsLengths"=connectionsLengths,
            "theta_angles"=theta_angles,
            "phi_angles_bend"=phi_angles_bend,
            "r_project_lengths"=r_project_lengths,
            "tip_r_ProjectVectors"=tip_r_ProjectVectors,
            "r_max_lengths"=r_max_lengths,
            "tip_r_MaxVectors"=tip_r_MaxVectors,
            "slicingEllipsoids"=slicingEllipsoids,
            "skeletalSheetPoints"=skeletalSheetPoints)
}

# numberOfFrames<-12
# ellipseResolution<-4
# 
# #NB! do not choose alpha beta gamma as exactly 0 !!!!
# EulerAngles_alpha<-rep(pi/100,numberOfFrames)
# EulerAngles_beta<-rep(pi/10,numberOfFrames)
# #EulerAngles_gamma<-rep(pi/4,numberOfFrames)
# EulerAngles_gamma<-rep(pi/100,numberOfFrames)
# 
# ellipseRadii_a<-runif(numberOfFrames,min = 2,max = 3)
# ellipseRadii_b<-runif(numberOfFrames,min = 1,max = 2)
# connectionsLengths<-runif(numberOfFrames-1,min = 3,max = 5)
# 
# 
# create_Elliptical_Tube(numberOfFrames=numberOfFrames,
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


plot_Elliptical_Tube<- function(e_tube,
                                plot_boundary=TRUE,
                                plot_r_max=FALSE,
                                plot_r_project=TRUE,
                                plot_frames=TRUE,
                                plot_normal_vec=FALSE,
                                plot_skeletal_sheet=TRUE,
                                decorate=TRUE,
                                colSkeletalSheet="blue",
                                colorBoundary="blue",
                                userMatrix4plotOrientation=NA,
                                plotSignificantFeatures=FALSE,
                                sig_Radii_a_indices=NULL,
                                sig_Radii_b_indices=NULL,
                                sig_Conncetions_indices=NULL,
                                sig_Theta_indices=NULL,
                                sig_V1_V2_indices=NULL,
                                lwdSignificant=4,
                                colorSignificant="red",
                                add=FALSE) {
  
  numberOfFrames<-nrow(e_tube$spinalPoints3D)
  
  spinalPoints3D<-e_tube$spinalPoints3D
  twistingFramesBasedOnParents<-e_tube$twistingFramesBasedOnParents
  twistingFramesGlobalCoordinate<-e_tube$twistingFramesGlobalCoordinate
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
  
  #plot
  if(add==FALSE){
    open3d() 
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
      plot3d(rbind(spinalPoints3D[i,],tip_r_ProjectVectors[i,]),type = 'l',lwd=3.2,col='red',expand = 10,box=FALSE,add = TRUE)
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
    vectors3d(spinalPoints3D+t(twistingFramesGlobalCoordinate[1,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="blue", lwd=2) 
    vectors3d(spinalPoints3D+t(twistingFramesGlobalCoordinate[2,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="red", lwd=2) 
    vectors3d(spinalPoints3D+t(twistingFramesGlobalCoordinate[3,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="green", lwd=2) 
  }
  
  if(plot_normal_vec==TRUE){
    #vectors3d(spinalPoints3D+t(frenetFramesGlobalCoordinate[1,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="black", lwd=2) 
    vectors3d(spinalPoints3D+t(frenetFramesGlobalCoordinate[2,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="black", lwd=2) 
    #vectors3d(spinalPoints3D+t(frenetFramesGlobalCoordinate[3,,]),origin = spinalPoints3D,headlength = 0.1,radius = 1/10, col="black", lwd=2) 
    
  }
  
  if(decorate==TRUE){
    decorate3d()  
  }
  
  if(plotSignificantFeatures==TRUE){
    
    sig_Radii_a_indices
    sig_Radii_b_indices
    sig_Conncetions_indices
    sig_Theta_indices
    sig_V1_V2_indices
    
    if(!is.null(sig_Radii_a_indices)){
      for (i in sig_Radii_a_indices) {
        plot3d(rbind(slicingEllipsoids[,,i],slicingEllipsoids[1,,i]),type = 'l',
               lwd=lwdSignificant,col=colorSignificant,expand = 10,box=FALSE,add = TRUE)
      } 
    }
    if(!is.null(sig_Radii_b_indices)){
      for (i in sig_Radii_b_indices) {
        plot3d(rbind(slicingEllipsoids[,,i],slicingEllipsoids[1,,i]),type = 'l',
               lwd=lwdSignificant,col=colorSignificant,expand = 10,box=FALSE,add = TRUE)
      }
    }
    if(!is.null(sig_Conncetions_indices)){
      for (i in sig_Conncetions_indices) {
        if(i==1){
          next
        }
        plot3d(rbind(spinalPoints3D[i-1,],
                     spinalPoints3D[i,]),type = 'l',
               lwd=lwdSignificant,col=colorSignificant,expand = 10,box=FALSE,add = TRUE)
      }
    }
    
    if(!is.null(sig_Theta_indices)){
      for (i in sig_Theta_indices) {
        plot3d(rbind(slicingEllipsoids[,,i],slicingEllipsoids[1,,i]),type = 'l',
               lwd=lwdSignificant,col=colorSignificant,expand = 10,box=FALSE,add = TRUE)
      }
    }
    
    if(!is.null(sig_V1_V2_indices)){
      for (i in sig_V1_V2_indices) {
        plot3d(rbind(slicingEllipsoids[,,i],slicingEllipsoids[1,,i]),type = 'l',
               lwd=lwdSignificant,col=colorSignificant,expand = 10,box=FALSE,add = TRUE)
      } 
    }
    
  }
  
  
  if(!anyNA(userMatrix4plotOrientation)){
    par3d(userMatrix=userMatrix4plotOrientation)
  }
  
  
}



extrinsic_Transformation_Elliptical_Tubes <- function(tube1,
                                                      tube2,
                                                      numberOfSteps=4,
                                                      plotting=TRUE,
                                                      colorBoundary="blue",
                                                      add=FALSE) {
  
  twistingFramesBasedOnParents<-tube1$twistingFramesBasedOnParents
  
  if(dim(tube1$twistingFramesBasedOnParents)[3]!=dim(tube2$twistingFramesBasedOnParents)[3]){
    stop('Number of cross-sections are not the same!')
  }
  
  numberOfFrames<-dim(tube1$twistingFramesBasedOnParents)[3]
  twistingFramesBasedOnParents_Steps<-array(NA,dim=c(dim(tube1$twistingFramesBasedOnParents),numberOfSteps))
  for (i in 1:numberOfFrames) {
    
    q1_twist<-as.Q4(as.SO3(tube1$twistingFramesBasedOnParents[,,i]))
    q2_twist<-as.Q4(as.SO3(tube2$twistingFramesBasedOnParents[,,i]))
    
    if(norm(as.vector(q1_twist)-as.vector(q2_twist),type = '2')<0.000001){
      for (j in 1:numberOfSteps) {
        twistingFramesBasedOnParents_Steps[,,i,j]<-as.SO3(q2_twist) 
      }
    }else{
      q4pointsOnAGeodesic_twist<-geodesicPathOnUnitSphere(as.vector(q1_twist),
                                                          as.vector(q2_twist),
                                                          numberOfneededPoints = numberOfSteps)
      for (j in 1:numberOfSteps) {
        twistingFramesBasedOnParents_Steps[,,i,j]<-as.SO3(as.Q4(q4pointsOnAGeodesic_twist[j,])) 
      }
    }
  }
  
  
  ellipseRadii_a_steps<-array(NA,dim=c(numberOfFrames,numberOfSteps))
  ellipseRadii_b_steps<-array(NA,dim=c(numberOfFrames,numberOfSteps))
  for (i in 1:numberOfFrames) {
    ellipseRadii_a_steps[i,]<-seq(from=tube1$ellipseRadii_a[i],
                                  to=tube2$ellipseRadii_a[i],
                                  length.out=numberOfSteps)
    ellipseRadii_b_steps[i,]<-seq(from=tube1$ellipseRadii_b[i],
                                  to=tube2$ellipseRadii_b[i],
                                  length.out=numberOfSteps)
  }
  
  connectionsLengths_steps<-array(NA,dim=c(numberOfFrames,numberOfSteps))
  for (i in 1:numberOfFrames) {
    connectionsLengths_steps[i,]<-seq(from=tube1$connectionsLengths[i],
                                      to=tube2$connectionsLengths[i],
                                      length.out=numberOfSteps)
  }
  
  
  
  tubes<-list()
  for (j in 1:numberOfSteps) {
    
    tubes[[j]]<-create_Elliptical_Tube(numberOfFrames = numberOfFrames,
                                       method = "basedOnTwistingFrames",
                                       twistingFramesBasedOnParents = twistingFramesBasedOnParents_Steps[,,,j],
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
# extrinsic_Transformation_Elliptical_Tubes(tube1 = e_Tubes_simulated[[1]],
#                                           tube2 = e_Tubes_simulated[[7]],
#                                           numberOfSteps = 4,
#                                           plotting = TRUE,
#                                           add = FALSE)



scalingEllipseBasedOn_R_max <- function(ellipse_radius_a,
                                        ellipse_radius_b,
                                        R_project_length,
                                        R_max,
                                        theta) {

  
  a<-ellipse_radius_a
  b<-ellipse_radius_b
  r<-R_project_length
  r_max<-R_max
  
  if(r<r_max){
    return(c(a,b))
  }else{

    #eccentricity
    e<-sqrt(1-(b/a)^2)
    
    # theta is for the polar coordinate of the ellipse i.e., 
    # the angle between the third element of the Frenet frame and the twisting frame
    
    b2<-r_max*sqrt(1-(e*cos(theta))^2)
    a2<-sqrt(b2^2/(1-e^2))
    
    if(a< a2 & b< b2){
      return(c(a,b))
    }else{
      return(c(a2,b2)) 
    }
    
  }
 
}


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
}
# test1<-tubeCrossSetionsIndicesWith_NonLocal_SelfIntersections(tube = extrinsicTubes[[2]])
# test1$criticalElipses_index
# plot_Elliptical_Tube(extrinsicTubes[[2]])
# plot3d(test1$intersectionPoints,type = 's',radius = 0.2,col='black',expand = 10,box=FALSE,add = TRUE)

semiExtrinsic_Transformation_Elliptical_Tubes <- function(tube1,
                                                          tube2,
                                                          numberOfSteps=numberOfSteps,
                                                          removeNonLocalSingularity=FALSE,
                                                          scalingFactor=0.9,
                                                          plotting=TRUE,
                                                          colorBoundary = "blue",
                                                          add=FALSE) {
  
  
  extrinsic_tubes<-extrinsic_Transformation_Elliptical_Tubes(tube1 = tube1,
                                                             tube2 = tube2,
                                                             numberOfSteps = numberOfSteps,
                                                             plotting = FALSE)
  
  numberOfFrames<-nrow(extrinsic_tubes[[1]]$spinalPoints3D)
  
  #updating ellipse radii based on the length of RC vectors
  ellipseRadii_a_steps_updated<-array(NA,dim=c(numberOfFrames,numberOfSteps))
  ellipseRadii_b_steps_updated<-array(NA,dim=c(numberOfFrames,numberOfSteps))
  for (k in 1:numberOfSteps) {
    for (j in 1:numberOfFrames) {
      
      newRadii<-scalingEllipseBasedOn_R_max(ellipse_radius_a = extrinsic_tubes[[k]]$ellipseRadii_a[j],
                                            ellipse_radius_b = extrinsic_tubes[[k]]$ellipseRadii_b[j],
                                            R_project_length = extrinsic_tubes[[k]]$r_project_lengths[j],
                                            R_max=extrinsic_tubes[[k]]$r_max_lengths[j],
                                            theta = extrinsic_tubes[[k]]$theta_angles[j])
      
      ellipseRadii_a_steps_updated[j,k]<-newRadii[1]
      ellipseRadii_b_steps_updated[j,k]<-newRadii[2]
    }
  }
  
  
  
  tubes<-list()
  for (k in 1:numberOfSteps) {
    
    tubes[[k]]<-create_Elliptical_Tube(numberOfFrames = numberOfFrames,
                                       method = "basedOnTwistingFrames",
                                       twistingFramesBasedOnParents = extrinsic_tubes[[k]]$twistingFramesBasedOnParents,
                                       ellipseRadii_a = ellipseRadii_a_steps_updated[,k],
                                       ellipseRadii_b = ellipseRadii_b_steps_updated[,k],
                                       connectionsLengths = extrinsic_tubes[[k]]$connectionsLengths,
                                       plotting = FALSE,
                                       add = TRUE)
  }
  
  
  if(removeNonLocalSingularity==TRUE){
    for (k in 1:numberOfSteps) {
      cat("Removing non-local singularity form tube number",k,"out of",numberOfSteps,"tubes.\n")
      criticalIndices<-tubeCrossSetionsIndicesWith_NonLocal_SelfIntersections(tube =  tubes[[k]])$criticalElipses_index
      while (!is.null(criticalIndices)) {
        ellipseRadii_a_updated<-tubes[[k]]$ellipseRadii_a
        ellipseRadii_a_updated[criticalIndices]<-ellipseRadii_a_updated[criticalIndices]*scalingFactor
        ellipseRadii_b_updated<-tubes[[k]]$ellipseRadii_b
        ellipseRadii_b_updated[criticalIndices]<-ellipseRadii_b_updated[criticalIndices]*scalingFactor
        tubes[[k]]<-create_Elliptical_Tube(numberOfFrames = nrow(tubes[[k]]$spinalPoints3D),
                                           method = "basedOnTwistingFrames",
                                           twistingFramesBasedOnParents = tubes[[k]]$twistingFramesBasedOnParents,
                                           ellipseRadii_a = ellipseRadii_a_updated,
                                           ellipseRadii_b = ellipseRadii_b_updated,
                                           connectionsLengths =tubes[[k]]$connectionsLengths,
                                           plotting = FALSE)
        criticalIndices<-tubeCrossSetionsIndicesWith_NonLocal_SelfIntersections(tube =  tubes[[k]])$criticalElipses_index
        cat("\n Cross sections with non-local intersections are:",criticalIndices,"\n")
      }
    }
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


calculate_phi_theta_from_TwistingFrameBasedOnParent <- function(twistingFrameBasedOnParent) {
  
  #phi is the angle between tangent vector and (1,0,0)
  phi<-geodesicDistance(c(1,0,0),twistingFrameBasedOnParent[1,])
  
  
  #calculate theta by constructing the Frenet frame
  b_vec<-twistingFrameBasedOnParent[1,]
  u2<-b_vec
  u1<-c(-1,0,0)
  thetaTemp<-geodesicDistance(u1,u2)
  if(abs(thetaTemp-pi)>10^-5){
    #formula from geodesicPathOnUnitSphere()
    N_vec<-1/sin(thetaTemp)*(sin(pi/2)*u1+sin(thetaTemp-pi/2)*u2)  
    b_perb_vec<-convertVec2unitVec2(myCrossProduct(b_vec,N_vec))
    # FrenetFrame<-as.matrix(as.SO3(rbind(b_vec,N_vec,b_perb_vec)))
    FrenetFrame<-rbind(b_vec,N_vec,b_perb_vec)
  }else{
    FrenetFrame<-diag(3)
  }
  
  # vectors3d(mean_TwistingFrameBasedOnParent_jth_CS,col="pink")
  # vectors3d(FrenetFrame)
  
  theta<-geodesicDistance(twistingFrameBasedOnParent[2,],FrenetFrame[2,])

  # if(theta>pi/2){
  #   theta<-theta-pi/2
  # }
  
  result<-list("phi"=phi,"theta"=theta)
  
  return(result)
  
}


semiIntrinsic_Transformation_Elliptical_Tubes <- function(tube1,
                                                      tube2,
                                                      numberOfSteps=4,
                                                      r_max_limit=Inf, #r_max_limit must be a great number !!!!
                                                      considerSizeFirstCrossSection=FALSE,
                                                      removeNonLocalSingularity=FALSE,
                                                      scalingFactor=0.9,
                                                      n=1, #degree of tangent function
                                                      plotting=TRUE,
                                                      colorBoundary = "blue",
                                                      add=FALSE) {
  
  numberOfFrames<-dim(tube1$twistingFramesBasedOnParents)[3]
  
  if(dim(tube1$twistingFramesBasedOnParents)[3]!=dim(tube2$twistingFramesBasedOnParents)[3]){
    stop('Number of cross-sections are not the same!')
  }
  
  twistingFramesBasedOnParents_Steps<-array(NA,dim=c(dim(tube1$twistingFramesBasedOnParents),numberOfSteps))
  for (i in 1:numberOfFrames) {
    
    q1_twist<-as.Q4(as.SO3(tube1$twistingFramesBasedOnParents[,,i]))
    q2_twist<-as.Q4(as.SO3(tube2$twistingFramesBasedOnParents[,,i]))
    
    if(norm(as.vector(q1_twist)-as.vector(q2_twist),type = '2')<0.000001){
      for (j in 1:numberOfSteps) {
        twistingFramesBasedOnParents_Steps[,,i,j]<-as.SO3(q2_twist) 
      }
    }else{
      q4pointsOnAGeodesic_twist<-geodesicPathOnUnitSphere(as.vector(q1_twist),
                                                          as.vector(q2_twist),
                                                          numberOfneededPoints = numberOfSteps)
      for (j in 1:numberOfSteps) {
        twistingFramesBasedOnParents_Steps[,,i,j]<-as.SO3(as.Q4(q4pointsOnAGeodesic_twist[j,])) 
      }
    }
  }
  
  connectionsLengths_tube1<-tube1$connectionsLengths
  connectionsLengths_tube2<-tube2$connectionsLengths
  if(length(connectionsLengths_tube1)==(numberOfFrames-1)){
    connectionsLengths_tube1<-c(0,connectionsLengths_tube1)
  }
  if(length(connectionsLengths_tube2)==(numberOfFrames-1)){
    connectionsLengths_tube2<-c(0,connectionsLengths_tube2)
  }
  connectionsLengths_steps<-array(NA,dim=c(numberOfFrames,numberOfSteps))
  for (i in 1:numberOfFrames) {
    connectionsLengths_steps[i,]<-seq(from=connectionsLengths_tube1[i],
                                      to=connectionsLengths_tube2[i],
                                      length.out=numberOfSteps)
  }
  
  #kappa reflect eccentricity 
  kappa1_ratio_b_a_tube1<-tube1$ellipseRadii_b/tube1$ellipseRadii_a
  kappa1_ratio_b_a_tube2<-tube2$ellipseRadii_b/tube2$ellipseRadii_a
  
  kappa1_ratio_b_a_steps<-array(NA,dim=c(numberOfFrames,numberOfSteps))
  for (i in 1:numberOfFrames) {
    kappa1_ratio_b_a_steps[i,]<-seq(from=kappa1_ratio_b_a_tube1[i],
                                    to=kappa1_ratio_b_a_tube2[i],
                                    length.out=numberOfSteps)
  }
  
  
  r_project_tube1<-tube1$r_project_lengths
  r_project_tube2<-tube2$r_project_lengths
  
  r_max_tube1<-tube1$r_max_lengths
  r_max_tube2<-tube2$r_max_lengths
  
  if(max(c(r_project_tube1,r_project_tube2))>r_max_limit){
    r_max_limit<-max(c(r_project_tube1,r_project_tube2))+r_max_limit
  }
  
  r_max_tube1[r_max_tube1>r_max_limit]<-r_max_limit
  r_max_tube2[r_max_tube2>r_max_limit]<-r_max_limit

  
  kappa2_ratio_Rproject_Rmax_tube1<-atan(r_project_tube1^n)/atan(r_max_tube1^n)
  kappa2_ratio_Rproject_Rmax_tube2<-atan(r_project_tube2^n)/atan(r_max_tube2^n)
  
  kappa2_ratio_Rproject_Rmax_steps<-array(NA,dim=c(numberOfFrames,numberOfSteps))
  for (i in 1:numberOfFrames) {
    kappa2_ratio_Rproject_Rmax_steps[i,]<-seq(from=kappa2_ratio_Rproject_Rmax_tube1[i],
                                              to=kappa2_ratio_Rproject_Rmax_tube2[i],
                                              length.out=numberOfSteps)
  }
  
  phi_steps<-array(NA,dim=c(numberOfFrames,numberOfSteps))
  theta_steps<-array(NA,dim=c(numberOfFrames,numberOfSteps))
  for (j in 1:numberOfSteps) {
    for (i in 1:numberOfFrames) {
      phi_steps[i,j]<-calculate_phi_theta_from_TwistingFrameBasedOnParent(twistingFrameBasedOnParent = twistingFramesBasedOnParents_Steps[,,i,j])$phi 
      theta_steps[i,j]<-calculate_phi_theta_from_TwistingFrameBasedOnParent(twistingFrameBasedOnParent = twistingFramesBasedOnParents_Steps[,,i,j])$theta
    }
  }
  
  ellipseRadii_a_steps<-array(NA,dim=c(numberOfFrames,numberOfSteps))
  ellipseRadii_b_steps<-array(NA,dim=c(numberOfFrames,numberOfSteps))
  for (j in 1:numberOfSteps) {
    for (i in 2:numberOfFrames) {
      # calculate a and b
      kappa1<-abs(kappa1_ratio_b_a_steps[i,j])
      kappa2<-abs(kappa2_ratio_Rproject_Rmax_steps[i,j])
      phi<-phi_steps[i,j]
      theta<-theta_steps[i,j]
      x<-connectionsLengths_steps[i,j]
      
      # a<-abs((tan(kappa2*atan((x/sin(phi))^n))))^(1/n)/
      #   abs((cos(atan(-kappa1*tan(theta)))*cos(theta)-kappa1*atan(-kappa1*tan(theta))*sin(theta)))
      r_maxTemp<-abs(x/sin(phi))
      if(r_maxTemp>=r_max_limit){
        r_maxTemp<-r_max_limit
      }
      
      # a<-abs((tan(kappa2*atan((r_maxTemp)^n))))^(1/n)/
      #   abs(cos(atan(-kappa1*tan(theta)))*cos(theta)-kappa1*sin(atan(-kappa1*tan(theta))*sin(theta)))
      
      a<-abs((tan(kappa2*atan((r_maxTemp)^n))))^(1/n)/
        abs(cos(atan(kappa1*tan(theta)))*cos(theta)+kappa1*sin(atan(kappa1*tan(theta)))*sin(theta))
      
      
      # a<-abs((tan(kappa2_ratio_Rproject_Rmax_steps[,j]*atan((connectionsLengths_steps[,j]/sin(phi_steps[,j]))^n))))^(1/n)/
      #   abs((cos(atan(-kappa1_ratio_b_a_steps[,j]*tan(theta_steps[,j])))*cos(theta_steps[,j])-kappa1_ratio_b_a_steps[,j]*
      #          atan(-kappa1_ratio_b_a_steps[,j]*tan(theta_steps[,j]))*sin(theta_steps[,j])))
      
      b<-abs(kappa1*a)
      
      ellipseRadii_a_steps[i,j]<-a
      ellipseRadii_b_steps[i,j]<-b
    
    }
  }
  if(considerSizeFirstCrossSection==TRUE){
    ellipseRadii_a_steps[1,]<-seq(from=tube1$ellipseRadii_a[1],
                                  to=tube2$ellipseRadii_a[1],
                                  length.out=numberOfSteps)
    ellipseRadii_b_steps[1,]<-seq(from=tube1$ellipseRadii_b[1],
                                  to=tube2$ellipseRadii_b[1],
                                  length.out=numberOfSteps) 
    
    ellipseRadii_a_steps[numberOfFrames,]<-seq(from=tube1$ellipseRadii_a[numberOfFrames],
                                  to=tube2$ellipseRadii_a[numberOfFrames],
                                  length.out=numberOfSteps)
    ellipseRadii_b_steps[numberOfFrames,]<-seq(from=tube1$ellipseRadii_b[numberOfFrames],
                                  to=tube2$ellipseRadii_b[numberOfFrames],
                                  length.out=numberOfSteps)
  }else{
    
    ellipseRadii_a_steps[1,]<-ellipseRadii_a_steps[2,]
    ellipseRadii_b_steps[1,]<-ellipseRadii_b_steps[2,]
    
    ellipseRadii_a_steps[numberOfFrames,]<-ellipseRadii_a_steps[numberOfFrames-1,]
    ellipseRadii_b_steps[numberOfFrames,]<-ellipseRadii_b_steps[numberOfFrames-1,]

  }

  tubes<-list()
  for (j in 1:numberOfSteps) {
    
    tubes[[j]]<-create_Elliptical_Tube(numberOfFrames = numberOfFrames,
                                       method = "basedOnTwistingFrames",
                                       twistingFramesBasedOnParents = twistingFramesBasedOnParents_Steps[,,,j],
                                       ellipseRadii_a = ellipseRadii_a_steps[,j],
                                       ellipseRadii_b = ellipseRadii_b_steps[,j],
                                       connectionsLengths = connectionsLengths_steps[,j],
                                       plotting = FALSE,
                                       add = TRUE)
  }
  
  if(removeNonLocalSingularity==TRUE){
    for (k in 1:numberOfSteps) {
      cat("\n Removing non-local singularity form tube number",k,"out of",numberOfSteps,"tubes.\n")
      criticalIndices<-tubeCrossSetionsIndicesWith_NonLocal_SelfIntersections(tube =  tubes[[k]])$criticalElipses_index
      while (!is.null(criticalIndices)) {
        cat("\n Cross sections with non-local intersections are:",criticalIndices,"\n")
        ellipseRadii_a_updated<-tubes[[k]]$ellipseRadii_a
        ellipseRadii_a_updated[criticalIndices]<-ellipseRadii_a_updated[criticalIndices]*scalingFactor
        ellipseRadii_b_updated<-tubes[[k]]$ellipseRadii_b
        ellipseRadii_b_updated[criticalIndices]<-ellipseRadii_b_updated[criticalIndices]*scalingFactor
        tubes[[k]]<-create_Elliptical_Tube(numberOfFrames = nrow(tubes[[k]]$spinalPoints3D),
                                           method = "basedOnTwistingFrames",
                                           twistingFramesBasedOnParents = tubes[[k]]$twistingFramesBasedOnParents,
                                           ellipseRadii_a = ellipseRadii_a_updated,
                                           ellipseRadii_b = ellipseRadii_b_updated,
                                           connectionsLengths =tubes[[k]]$connectionsLengths,
                                           plotting = FALSE)
        criticalIndices<-tubeCrossSetionsIndicesWith_NonLocal_SelfIntersections(tube =  tubes[[k]])$criticalElipses_index
        
      }
    }
  }
  
  
  #plot tubes
  if(plotting==TRUE){
    for (j in 1:numberOfSteps) {
      plot_Elliptical_Tube(tubes[[j]],
                           plot_r_project = FALSE,
                           plot_r_max = FALSE,
                           colorBoundary=colorBoundary,
                           add = add)
    } 
  }
  
  return(tubes)

}
# semiIntrinsic_Transformation_Elliptical_Tubes(tube1 = e_Tubes_simulated[[1]],
#                                           tube2 = e_Tubes_simulated[[4]],
#                                           numberOfSteps = 4,
#                                           n=1,
#                                           plotting = TRUE,
#                                           add = TRUE)


intrinsicMean_Tubes_Without_Transformation <- function(tubes,
                                                       n=1,
                                                       considerSizeFirstCrossSection=TRUE,
                                                       plotting=TRUE) {
  
  numberOfSamples<-length(tubes)
  numberOfFrames<-dim(tubes[[1]]$twistingFramesBasedOnParents)[3]
  
  #mean twistingFramesBasedOnParents
  twistingFramesBasedOnParents_tubes<-array(NA,dim = c(3,3,numberOfFrames,numberOfSamples))
  for (j in 1:numberOfSamples) {
    twistingFramesBasedOnParents_tubes[,,,j]<-tubes[[j]]$twistingFramesBasedOnParents
  }
  vectorizedTwistingFramesBasedOnParents<-array(NA,dim = c(numberOfFrames,9,numberOfSamples))
  for (j in 1:numberOfSamples) {
    for (i in 1:numberOfFrames) {
      vectorizedTwistingFramesBasedOnParents[i,,j]<-as.vector(t(twistingFramesBasedOnParents_tubes[,,i,j]))
    }
  }
  meanTwistingFramesBasedOnParents<-array(NA, dim = c(3,3,numberOfFrames))
  for (i in 1:numberOfFrames) {
    tempVec<-mean(as.SO3(t(vectorizedTwistingFramesBasedOnParents[i,,])),type = 'projected')
    # tempVec<-mean(as.SO3(t(vectorizedTwistingFramesBasedOnParents[i,,])),type = 'geometric')
    meanTwistingFramesBasedOnParents[,,i]<-matrix(tempVec,nrow = 3,byrow = TRUE)
  }
  
  #mean connections' lengths
  connectionsLengths_samples<-array(NA,dim = c(numberOfFrames,numberOfSamples))
  for (j in 1:numberOfSamples) {
    connectionsLengths_samples[,j]<-tubes[[j]]$connectionsLengths
  }
  connectionsLengths_mean<-rowMeans(connectionsLengths_samples)
  
  
  ellipseRadii_a_samples<-array(NA,dim = c(numberOfFrames,numberOfSamples))
  ellipseRadii_b_samples<-array(NA,dim = c(numberOfFrames,numberOfSamples))
  for (j in 1:numberOfSamples) {
    ellipseRadii_a_samples[,j]<-tubes[[j]]$ellipseRadii_a
    ellipseRadii_b_samples[,j]<-tubes[[j]]$ellipseRadii_b
  }
  
  #mean kappa1
  kappa1_ratio_b_a_samples<-array(NA,dim = c(numberOfFrames,numberOfSamples))
  for (j in 1:numberOfSamples) {
    kappa1_ratio_b_a_samples[,j]<-ellipseRadii_b_samples[,j]/ellipseRadii_a_samples[,j]
  }
  mean_kappa1_ratios_b_a<-rowMeans(kappa1_ratio_b_a_samples)
  
  #mean kappa2
  kappa2_ratio_Rproject_Rmax_samples<-array(NA,dim = c(numberOfFrames,numberOfSamples))
  for (j in 1:numberOfSamples) {
    kappa2_ratio_Rproject_Rmax_samples[,j]<-atan(tubes[[j]]$r_project_lengths^n)/atan(tubes[[j]]$r_max_lengths^n)
  }
  mean_kappa2_ratios_Rproject_Rmax<-rowMeans(kappa2_ratio_Rproject_Rmax_samples)
  
  phi_mean<-rep(NA,numberOfFrames)
  theta_mean<-rep(NA,numberOfFrames)
  for (i in 1:numberOfFrames) {
    phi_mean[i]<-calculate_phi_theta_from_TwistingFrameBasedOnParent(meanTwistingFramesBasedOnParents[,,i])$phi
    theta_mean[i]<-calculate_phi_theta_from_TwistingFrameBasedOnParent(meanTwistingFramesBasedOnParents[,,i])$theta
  }
  
  
  ellipseRadii_a_mean<-rep(NA,numberOfFrames)
  ellipseRadii_b_mean<-rep(NA,numberOfFrames)
  for (i in 2:numberOfFrames) {
    kappa1<-mean_kappa1_ratios_b_a[i]
    kappa2<-mean_kappa2_ratios_Rproject_Rmax[i]
    phi<-phi_mean[i]
    theta<-theta_mean[i]
    x<-connectionsLengths_mean[i]
    
    r_maxTemp<-abs(x/sin(phi))
    
    a<-abs((tan(kappa2*atan((r_maxTemp)^n))))^(1/n)/
      abs(cos(atan(kappa1*tan(theta)))*cos(theta)+kappa1*sin(atan(kappa1*tan(theta)))*sin(theta))
    
    b<-abs(kappa1*a)
    
    ellipseRadii_a_mean[i]<-a
    ellipseRadii_b_mean[i]<-b
    
  }
  
  if(considerSizeFirstCrossSection==TRUE){
    ellipseRadii_a_mean[1]<-mean(ellipseRadii_a_samples[1,])
    ellipseRadii_b_mean[1]<-mean(ellipseRadii_b_samples[1,])
  }else{
    ellipseRadii_a_mean[1]<-ellipseRadii_a_mean[2]
    ellipseRadii_b_mean[1]<-ellipseRadii_b_mean[2]
    
  }

  meanTube<-create_Elliptical_Tube(numberOfFrames = numberOfFrames,
                                   method = "basedOnTwistingFrames",
                                   twistingFramesBasedOnParents = meanTwistingFramesBasedOnParents,
                                   ellipseResolution = 4,
                                   ellipseRadii_a = ellipseRadii_a_mean,
                                   ellipseRadii_b = ellipseRadii_b_mean,
                                   connectionsLengths = connectionsLengths_mean,
                                   plotting = plotting)
  
}


extrinsic_Distance_Between2_e_tubes<- function(tube1,tube2) {
  
  if(dim(tube1$twistingFramesBasedOnParents)[3]!=dim(tube2$twistingFramesBasedOnParents)[3]){
    stop('Number of cross-sections are not the same!')
  }
  
  numberOfFrames<-dim(tube1$twistingFramesBasedOnParents)[3]
  
  
  distancesTwistingFrames<-rep(NA,numberOfFrames)
  for (i in 1:numberOfFrames) {
    # q1_twist<-as.vector(as.Q4(as.SO3(tube1$twistingFramesBasedOnParents[,,i])))
    # q2_twist<-as.vector(as.Q4(as.SO3(tube2$twistingFramesBasedOnParents[,,i])))
    # distancesTwistingFrames[i]<-geodesicDistance(q1_twist,q2_twist)
    distancesTwistingFrames[i]<-rot.dist(as.SO3(tube1$twistingFramesBasedOnParents[,,i]),
                                         as.SO3(tube2$twistingFramesBasedOnParents[,,i]),
                                         method="intrinsic")
  }
  
  distancesConnectionsLengths<-abs(log(tube1$connectionsLengths)-log(tube2$connectionsLengths))
  distancesConnectionsLengths<-distancesConnectionsLengths[!is.na(distancesConnectionsLengths)]
  
  
  distancesRadii_a<-abs(log(tube1$ellipseRadii_a)-log(tube2$ellipseRadii_a))
  distancesRadii_b<-abs(log(tube1$ellipseRadii_b)-log(tube2$ellipseRadii_b))
  
  totalDistance<-sqrt(sum(distancesTwistingFrames^2)+
                        sum(distancesConnectionsLengths^2)+
                        sum(distancesRadii_a^2)+
                        sum(distancesRadii_b^2))
  
  return(totalDistance)
}

intrinsic_Distance_Between2_e_tubes <- function(tube1,
                                                tube2,
                                                numberOfSteps) {
  
  tubes_on_path<-semiIntrinsic_Transformation_Elliptical_Tubes(tube1 = tube1,
                                                           tube2 = tube2,
                                                           numberOfSteps =numberOfSteps,
                                                           considerSizeFirstCrossSection = TRUE,
                                                           r_max_limit = Inf,
                                                           n = 1,
                                                           plotting = FALSE)
  distance<-0
  for (i in 1:(numberOfSteps-1)) {
    distance<-distance+
      extrinsic_Distance_Between2_e_tubes(tubes_on_path[[i]],tubes_on_path[[i+1]])
  }
  return(distance)
}

mean_tube_basedOnSemiIntrinsicTransformation <- function(tubes,
                                                     numberOfSteps=5,
                                                     useWhichDistance="extrinsic",
                                                     n=1, #degree of tangent function
                                                     considerSizeFirstCrossSection=TRUE,
                                                     numberOfIteration=10,
                                                     plotting=TRUE) {
  numberOfTubes<-length(tubes)
  
  temp_tube<-tubes[[1]]
  
  for (k in 1:numberOfIteration) {
    
    tempIndex<-sample(1:numberOfTubes,size = 1)
    if(extrinsic_Distance_Between2_e_tubes(tube1 = temp_tube,tube2 = tubes[[tempIndex]])<1e-05){
      x<-1:numberOfTubes
      tempIndex<-sample(x[!x %in% tempIndex],size = 1)
    }
    
    generatedTubesOnAPath<-semiIntrinsic_Transformation_Elliptical_Tubes(tube1 = temp_tube,
                                                                         tube2 = tubes[[tempIndex]],
                                                                         considerSizeFirstCrossSection=considerSizeFirstCrossSection,
                                                                         n=n,
                                                                         numberOfSteps = numberOfSteps,
                                                                         plotting = FALSE)
    
    distanceMatrix<-Matrix(NA,nrow = length(generatedTubesOnAPath),ncol = numberOfTubes)
    for (i in 1:length(generatedTubesOnAPath)) {
      for (j in 1:numberOfTubes) {
        
        if(useWhichDistance=="extrinsic"){
          distanceMatrix[i,j]<-extrinsic_Distance_Between2_e_tubes(tube1 = generatedTubesOnAPath[[i]],
                                                                   tube2 = tubes[[j]]) 
        }else if(useWhichDistance=="intrinsic"){
          distanceMatrix[i,j]<-intrinsic_Distance_Between2_e_tubes(tube1 = generatedTubesOnAPath[[i]],
                                                                   tube2 = tubes[[j]],
                                                                   numberOfSteps=numberOfSteps)    
        }else{
          stop("Please specify useWhichDistance as extrinsic or intrinsic!")
        }
      
      }
    }
    temp_tube<-generatedTubesOnAPath[[which.min(rowSums(distanceMatrix^2))]]
    
    cat(k, "th number of iteration from total", numberOfIteration, "number of iteration is done \n!")
  }
  
  mean_tube<-temp_tube
  
  if(plotting==TRUE){
    plot_Elliptical_Tube(mean_tube,
                         plot_r_max = FALSE,
                         plot_r_project = FALSE,
                         plot_skeletal_sheet = TRUE,
                         colSkeletalSheet="blue")
  }
  
  return(mean_tube)
  
}



extrinsicMean_tubes <- function(tubes,
                                plotting=TRUE) {
  
  numberOfSamples<-length(tubes)
  numberOfFrames<-dim(tubes[[1]]$twistingFramesBasedOnParents)[3]
  
  twistingFramesBasedOnParents_tubes<-array(NA,dim = c(3,3,numberOfFrames,numberOfSamples))
  for (j in 1:numberOfSamples) {
    twistingFramesBasedOnParents_tubes[,,,j]<-tubes[[j]]$twistingFramesBasedOnParents
  }
  
  vectorizedTwistingFramesBasedOnParents<-array(NA,dim = c(numberOfFrames,9,numberOfSamples))
  for (j in 1:numberOfSamples) {
    for (i in 1:numberOfFrames) {
      vectorizedTwistingFramesBasedOnParents[i,,j]<-as.vector(t(twistingFramesBasedOnParents_tubes[,,i,j]))
    }
  }
  
  meanTwistingFramesBasedOnParents<-array(NA, dim = c(3,3,numberOfFrames))
  for (i in 1:numberOfFrames) {
    tempVec<-mean(as.SO3(t(vectorizedTwistingFramesBasedOnParents[i,,])),type = 'projected')
    # tempVec<-mean(as.SO3(t(vectorizedTwistingFramesBasedOnParents[i,,])),type = 'geometric')
    meanTwistingFramesBasedOnParents[,,i]<-matrix(tempVec,nrow = 3,byrow = TRUE)
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
                                   method = "basedOnTwistingFrames",
                                   twistingFramesBasedOnParents = meanTwistingFramesBasedOnParents,
                                   ellipseResolution = 4,
                                   ellipseRadii_a = mean_ellipseRadii_a,
                                   ellipseRadii_b = mean_ellipseRadii_b,
                                   connectionsLengths = mean_connectionsLengths,
                                   plotting = plotting)
  
}


tubeSmoother <- function(tube,
                         addPoints=2,
                         plotting=TRUE) {
  # skeletal smoother
  
  numberOfFrames<-dim(tube$twistingFramesGlobalCoordinate)[3]
  numberOfFrames_smoothed<-addPoints*(numberOfFrames-1)+numberOfFrames
  
  twistingFramesGlobalCoordinate_smooth<-array(NA,dim=c(3,3,numberOfFrames_smoothed))
  k<-1
  for (i in 2:numberOfFrames) {
    
    q1_twist<-as.Q4(as.SO3(tube$twistingFramesGlobalCoordinate[,,i-1]))
    q2_twist<-as.Q4(as.SO3(tube$twistingFramesGlobalCoordinate[,,i]))
    
    if(norm(as.vector(q1_twist)-as.vector(q2_twist),type = '2')<0.000001){
      twistingFramesGlobalCoordinate_smooth[,,k]<-as.SO3(q1_twist)
      k<-k+1
      for (j in 1:addPoints) {
        twistingFramesGlobalCoordinate_smooth[,,k]<-as.SO3(q2_twist) 
        k<-k+1
      }
    }else{
      q4pointsOnAGeodesic_twist<-geodesicPathOnUnitSphere(as.vector(q1_twist),
                                                          as.vector(q2_twist),
                                                          numberOfneededPoints = addPoints+2)
      for (j in 1:(nrow(q4pointsOnAGeodesic_twist)-1)) {
        twistingFramesGlobalCoordinate_smooth[,,k]<-as.SO3(as.Q4(q4pointsOnAGeodesic_twist[j,])) 
        k<-k+1
      }
    }
  }
  twistingFramesGlobalCoordinate_smooth[,,numberOfFrames_smoothed]<-twistingFramesGlobalCoordinate_smooth[,,numberOfFrames_smoothed-1]
  
  framesCenters<-1:numberOfFrames_smoothed
  framesParents<-c(1,1:(numberOfFrames_smoothed-1))
  
  twistingFramesBasedOnParents_smoothed<-array(NA,dim = c(3,3,numberOfFrames_smoothed))
  for (i in 1:numberOfFrames_smoothed) {
    k1<-framesCenters[i]
    k2<-framesParents[i]
    twistingFramesBasedOnParents_smoothed[,,k1]<-rotateFrameToMainAxes_standard(myFrame = twistingFramesGlobalCoordinate_smooth[,,k2],
                                                                                vectors2rotate = twistingFramesGlobalCoordinate_smooth[,,k1])
  } 
  
  spinalPoints3D<-tube$spinalPoints3D
  
  spinalPoints_smooth<-array(NA,dim = c(numberOfFrames_smoothed,3))
  k<-1
  for (i in 2:numberOfFrames) {
    point1<-spinalPoints3D[i-1,]
    point2<-spinalPoints3D[i,]
    
    pointsBetween<-generatePointsBetween2Points(point1 = point1,
                                                point2 = point2,
                                                numberOfPoints = addPoints+2)
    
    for (j in 1:(nrow(pointsBetween)-1)) {
      spinalPoints_smooth[k,]<-pointsBetween[j,] 
      k<-k+1
    }
    
  }
  spinalPoints_smooth[numberOfFrames_smoothed,]<-spinalPoints3D[numberOfFrames,]
  
  # plot3d(spinalPoints,type = 'l',lwd=2,col='blue',expand = 10,box=FALSE,add = TRUE)
  # plot3d(spinalPoints_smooth,type = 's',radius = 0.2,col='red',expand = 10,box=FALSE,add = TRUE)
  
  connectionsLengths_smooth<-rep(NA,numberOfFrames_smoothed)
  for (i in 2:numberOfFrames_smoothed) {
    connectionsLengths_smooth[i]<-norm(spinalPoints_smooth[i,]-spinalPoints_smooth[i-1,],type = "2")
  }
  connectionsLengths_smooth[1]<-0
  
  
  ellipseRadii_a<-tube$ellipseRadii_a
  ellipseRadii_a_smooth<-rep(NA,numberOfFrames_smoothed)
  ellipseRadii_b<-tube$ellipseRadii_b
  ellipseRadii_b_smooth<-rep(NA,numberOfFrames_smoothed)
  k<-1
  for (i in 2:numberOfFrames) {
    radii_a_CS_i_1<-ellipseRadii_a[i-1]
    radii_a_CS_i<-ellipseRadii_a[i]
    
    radii_b_CS_i_1<-ellipseRadii_b[i-1]
    radii_b_CS_i<-ellipseRadii_b[i]
    
    if(radii_a_CS_i_1==radii_a_CS_i){
      radii_a_CS_i<-radii_a_CS_i*1.001  
    }
    if(radii_b_CS_i_1==radii_b_CS_i){
      radii_b_CS_i<-radii_b_CS_i*1.001  
    }
    
    
    radii_a_Between<-generatePointsBetween2Points(point1 = radii_a_CS_i_1,
                                                  point2 = radii_a_CS_i,
                                                  numberOfPoints = addPoints+2)
    radii_b_Between<-generatePointsBetween2Points(point1 = radii_b_CS_i_1,
                                                  point2 = radii_b_CS_i,
                                                  numberOfPoints = addPoints+2)
    
    for (j in 1:(length(radii_a_Between)-1)) {
      ellipseRadii_a_smooth[k]<-radii_a_Between[j] 
      ellipseRadii_b_smooth[k]<-radii_b_Between[j] 
      k<-k+1
    }
    
  }
  ellipseRadii_a_smooth[numberOfFrames_smoothed]<-ellipseRadii_a[numberOfFrames]
  ellipseRadii_b_smooth[numberOfFrames_smoothed]<-ellipseRadii_b[numberOfFrames]
  
  tube_smoothed<-create_Elliptical_Tube(numberOfFrames = numberOfFrames_smoothed,
                                        method = "basedOnTwistingFrames",
                                        twistingFramesBasedOnParents =twistingFramesBasedOnParents_smoothed ,
                                        ellipseResolution = 4,
                                        ellipseRadii_a = ellipseRadii_a_smooth,
                                        ellipseRadii_b = ellipseRadii_b_smooth,
                                        connectionsLengths = connectionsLengths_smooth,
                                        plotting = FALSE)
  
  if(plotting==TRUE){
    # plot_Elliptical_Tube(tube,plot_frames = FALSE,add = FALSE,plot_r_project = FALSE)
    plot_Elliptical_Tube(tube_smoothed,plot_frames = FALSE,add = FALSE,plot_r_project = FALSE)
  }
  
  return(tube_smoothed)
  
}

check_Tube_Legality <- function(tube) {
  if(FALSE %in% c(tube$r_project_lengths<=tube$r_max_lengths)){
    print(which(tube$r_project_lengths>tube$r_max_lengths))
    return(FALSE)
  }else{
    return(TRUE)
  }
}






