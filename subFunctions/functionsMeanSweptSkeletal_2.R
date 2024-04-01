
# Extract the tangent vector as the second element of the Frenet frame
extractTangetVectorFrom_RCV_vectorIn6DHyperbola<- function(RCV_vectorIn6DHyperbola) {
  
  projectedVector<-RCV_vectorIn6DHyperbola[c(5,6)]
  x<-projectedVector[1]
  y<-projectedVector[2]
  z<-abs(sqrt(1-x^2-y^2))
  unitTangent<-convertVec2unitVec2(c(z,x,y))
  return(unitTangent)
  
}


calculate_RC_vectorsIn6DHyperbola_tube <- function(tube,
                                                   ellipseResolution=4) {
  
  frenetFramesBasedOnParents<-tube$frenetFramesBasedOnParents
  twistingFramesBasedOnParents<-tube$twistingFramesBasedOnParents 
  connectionsLengths<-tube$connectionsLengths
  ellipseRadii_a<-tube$ellipseRadii_a
  ellipseRadii_b<-tube$ellipseRadii_b
  
  # numberOfFrames<-dim(frenetFramesBasedOnParents)[3]
  # 
  # #theta twist 
  # theta_twists<-rep(NA,numberOfFrames)
  # for (i in 1:numberOfFrames) {
  #   twistingTemp<-twistingFramesBasedOnParents[,,i]
  #   frenetTemp<-frenetFramesBasedOnParents[,,i] 
  #   
  #   R1<-rotMat(twistingTemp[1,],c(1,0,0))
  #   rotatedTemp<-twistingTemp%*%t(R1)
  #   R2<-rotMat(rotatedTemp[2,],c(0,1,0))
  #   
  #   rotatedTwistingTemp<-twistingTemp%*%t(R2%*%R1)
  #   rotatedFrenetTemp<-frenetTemp%*%t(R2%*%R1)
  #   
  #   theta_twists[i]<-clockwiseAngle(c(1,0),rotatedFrenetTemp[2,2:3]) 
  #   
  # }
  
  theta_twists<-tube$theta_angles

  # projection of the tangent vectors to the plane of the previous frames
  tangentVectorsBasedOnParentFrames<-t(twistingFramesBasedOnParents[1,,])
  projectedVectors<-tangentVectorsBasedOnParentFrames[,2:3]
  
  
  # # twisting vector
  # twisting_UnitVectors<-t(apply(t(frenetFramesBasedOnParents[2,2:3,]),
  #                               MARGIN = 1,convertVec2unitVec2))
  # 
  # #theta
  # theta_twists<-rep(NA,numberOfFrames)
  # for (i in 1:numberOfFrames) {
  #   theta_twists[i]<-clockwiseAngle(c(1,0),twisting_UnitVectors[i,]) 
  # }
  
  #NB! twisting angle is different from theta_angle
  
  #connections' lengths
  X<-connectionsLengths
  
  #Relative curvature vectors
  RCV_VectorsIn6DHyperbola<-cbind(as.vector(ellipseRadii_a),
                                  as.vector(ellipseRadii_b),
                                  as.vector(X),
                                  theta_twists,
                                  projectedVectors)
  
  return(RCV_VectorsIn6DHyperbola)
}

# Converting the 6D hyperbola to a convex region (6D cylinder) based on
# swept skeletal coordinate
hyperbola6DWithXaxis_to_sweptCoordinate_3Dtubes <- function(vector_a_b_x_thetaTwist_projectedVec,
                                                            r_max_limit=10^5) {
  
  a<-vector_a_b_x_thetaTwist_projectedVec[1]
  b<-vector_a_b_x_thetaTwist_projectedVec[2]
  x<-vector_a_b_x_thetaTwist_projectedVec[3]
  thetaTwist<-vector_a_b_x_thetaTwist_projectedVec[4]
  projectedVec<-vector_a_b_x_thetaTwist_projectedVec[c(5,6)]
  
  
  # z<-abs(sqrt(1-projectedVec[1]^2-projectedVec[2]^2))
  # unitTangent<-convertVec2unitVec2(c(z,projectedVec[1],projectedVec[2]))
  # phi_angles_bend<-geodesicDistance(c(1,0,0),unitTangent)
  # if(phi_angles_bend==0){
  #   r_max<-r_max_limit
  # }else{
  #   r_max<-abs(x/sin(phi_angles_bend))
  # }
  
  r_project<-calculate_r_project_Length(a = a,b = b,theta = thetaTwist)
  
  projectRadius<-norm(projectedVec,type = '2')
  
  # maximum possible value of r inside the unit circle
  if(r_project<=x){ #i.e., if r_project<x then we can bend the frame up to pi/2 degree
    max_projectRadius<-1
  }else{
    max_projectRadius<-(x/r_project)
  }

  # maximum possible value of r
  ratio_projectRadius<-projectRadius/max_projectRadius
  
  if(projectRadius==0){
    projectedVecIn6DCylinder<-c(0,0)
  }else{
    projectedVecIn6DCylinder<-ratio_projectRadius*convertVec2unitVec(projectedVec) 
  }
  
  sweptSkeletalCoordinate<-c(a,b,x,thetaTwist,projectedVecIn6DCylinder)
  
  return(sweptSkeletalCoordinate)
  
}

sweptCoordinate_to_hyperbola6DWithXaxis_3Dtubes <- 
  function(vector_a_b_x_thetaTwist_projectedVec_In6DCylinder) {
    
    a<-vector_a_b_x_thetaTwist_projectedVec_In6DCylinder[1]
    b<-vector_a_b_x_thetaTwist_projectedVec_In6DCylinder[2]
    x<-vector_a_b_x_thetaTwist_projectedVec_In6DCylinder[3]
    thetaTwist<-vector_a_b_x_thetaTwist_projectedVec_In6DCylinder[4]
    projectedVecIn6DCylinder<-vector_a_b_x_thetaTwist_projectedVec_In6DCylinder[c(5,6)]
    
    
    # if(a<b){
    #   stop("Radius a must be grater than radius b!") 
    # }
    
    #length of the critical vector
    r_project<-calculate_r_project_Length(a = a,b = b,theta = thetaTwist)
    
    # r_project<-(a*b)/sqrt(b^2*cos(thetaTwist)^2+a^2*sin(thetaTwist)^2)
    
    # maximum possible value of r
    if(r_project<=x){ #i.e., if r_project<x then we can bend the frame up to pi/2 degree
      max_projectRadius<-1
    }else{
      max_projectRadius<-(x/r_project)
    }
    
    projectedVecIn6DHyperbola<-projectedVecIn6DCylinder*max_projectRadius
    
    if(sum(is.nan(projectedVecIn6DHyperbola))>0 | anyNA(projectedVecIn6DHyperbola)){
      projectedVecIn6DHyperbola<-c(0,0)
    }
    
    vector_a_b_x_thetaTwist_projectedVec<-c(a,b,x,thetaTwist,projectedVecIn6DHyperbola)
    
    return(vector_a_b_x_thetaTwist_projectedVec)
    
}



#discrete path between two points in 6D hyperbola
discretePathBetween2PointsIn_6D_Hyperbola <- function(point1,
                                                      point2,
                                                      numberOfpoints=10) {
  
  point1_SweptCoordinate<-hyperbola6DWithXaxis_to_sweptCoordinate_3Dtubes(vector_a_b_x_thetaTwist_projectedVec = point1)
  point2_SweptCoordinate<-hyperbola6DWithXaxis_to_sweptCoordinate_3Dtubes(vector_a_b_x_thetaTwist_projectedVec = point2)
  
  
  pathPointsIn6DCylinder<-generatePointsBetween2Points(point1 = point1_SweptCoordinate,
                                                       point2 = point2_SweptCoordinate,
                                                       numberOfPoints = numberOfpoints)
  
  
  pathPointsIn_6D_Hyperbola<-array(NA,dim = dim(pathPointsIn6DCylinder))
  for (i in 1:nrow(pathPointsIn6DCylinder)) {
    pathPointsIn_6D_Hyperbola[i,]<-sweptCoordinate_to_hyperbola6DWithXaxis_3Dtubes(vector_a_b_x_thetaTwist_projectedVec_In6DCylinder = 
                                                                                     pathPointsIn6DCylinder[i,])
  }
  
  
  return(pathPointsIn_6D_Hyperbola)
}



smoothTransformBetween2Ellipticaltubes_6D_Hyperbola <- function(tube1,
                                                                tube2,
                                                                numberOfSteps=5,
                                                                taloranceTwist=pi/10000,
                                                                flippingTolorance=pi/20,
                                                                plotting=TRUE) {
  
  if(dim(tube1$twistingFramesBasedOnParents)[3]!=dim(tube2$twistingFramesBasedOnParents)[3]){
    stop('Number of cross-sections are not the same!')
  }
  
  numberOfFrames<-dim(tube1$frenetFramesBasedOnParents)[3]
  I<-diag(3)
  
  
  RC_VectorsIn6DHyperbola_tube1<-calculate_RC_vectorsIn6DHyperbola_tube(tube = tube1)
  RC_VectorsIn6DHyperbola_tube2<-calculate_RC_vectorsIn6DHyperbola_tube(tube = tube2)
  
  pathsBetween_RC_vectors_In6DHyperbola<-array(NA,dim = c(numberOfSteps,6,numberOfFrames))
  for (i in 1:numberOfFrames) {
    p1<-RC_VectorsIn6DHyperbola_tube1[i,]
    p2<-RC_VectorsIn6DHyperbola_tube2[i,]
    if(norm(p1-p2,type = '2')==0){
      pathsBetween_RC_vectors_In6DHyperbola[,,i]<-matrix(rep(p1,numberOfSteps),ncol = length(p1),byrow = TRUE)
    }else{
      pathsBetween_RC_vectors_In6DHyperbola[,,i]<-discretePathBetween2PointsIn_6D_Hyperbola(
        point1=p1,
        point2=p2,
        numberOfpoints=numberOfSteps)
    }
    
  }
  
  unitTangentBasedOnParents4AllSamples<-array(NA,dim = c(numberOfFrames,3,numberOfSteps))
  for (j in 1:numberOfSteps) {
    for (i in 1:numberOfFrames) {
      unitTangentBasedOnParents4AllSamples[i,,j]<-
        extractTangetVectorFrom_RCV_vectorIn6DHyperbola(RCV_vectorIn6DHyperbola =  
                                                          pathsBetween_RC_vectors_In6DHyperbola[j,,i])
    }
  }
  
  # Frenet frames based on parents
  # parents are previous twisting frames !!!!
  frenetFramesBasedOnParents_Steps<-array(NA,dim=c(dim(tube1$frenetFramesBasedOnParents),numberOfSteps))
  frenetFramesBasedOnParents_Steps[,,1,]<-I
  for (j in 1:numberOfSteps) {
    for (i in 2:numberOfFrames) {
      
      #calculate theta by constructing the Frenet frame
      b_vec<-unitTangentBasedOnParents4AllSamples[i,,j]
      u2<-b_vec
      u1<-c(-1,0,0)
      thetaTemp<-geodesicDistance(u1,u2)
      if(abs(thetaTemp-pi)>10^-7){
        #formula from geodesicPathOnUnitSphere()
        N_vec<-1/sin(thetaTemp)*(sin(pi/2)*u1+sin(thetaTemp-pi/2)*u2)  
        b_perb_vec<-convertVec2unitVec2(myCrossProduct(b_vec,N_vec))
        # FrenetFrame<-matrix(as.SO3(rbind(b_vec,N_vec,b_perb_vec)),3,3)
        FrenetFrame<-rbind(b_vec,N_vec,b_perb_vec)
      }else{
        FrenetFrame<-I
      }
      frenetFramesBasedOnParents_Steps[,,i,j]<-FrenetFrame
    }
  }
  
  
  thetaTwistAll<-pathsBetween_RC_vectors_In6DHyperbola[,4,]
  twistingFramesBasedOnParents_Steps<-array(NA,dim=c(dim(tube1$twistingFramesBasedOnParents),numberOfSteps))
  twistingFramesBasedOnParents_Steps[,,1,]<-I
  for (k in 1:numberOfSteps) {
    for (i in 2:numberOfFrames) {
      tempFrame<-frenetFramesBasedOnParents_Steps[,,i,k]
      rotAxis<-tempFrame[1,]
      # thetaTwistTemp<-(-thetaTwistAll[k,i])
      thetaTwistTemp<-thetaTwistAll[k,i]
      
      if(abs(thetaTwistTemp)<taloranceTwist){
        
        R1<-rotMat(tempFrame[1,],c(1,0,0))
        if(anyNA(R1)){
          twistingFramesBasedOnParents_Steps[,,i,k]<-I  
        }else{
          twistingFramesBasedOnParents_Steps[,,i,k]<-I%*%R1
        }
        
      }else{
        # rotate frame by theta angle around the rotAxis clockwise
        R_temp<-rotaxisMat(u = rotAxis,theta = thetaTwistTemp)
        # R_temp<-rotationAbout_uAxis_byThetaAngle(u = rotAxis,theta = thetaTwistTemp)
        twistingFramesBasedOnParents_Steps[,,i,k]<-tempFrame%*%R_temp 
      }

    }
  }
  twistingFramesBasedOnParents_Steps[,,numberOfFrames,]<-
    twistingFramesBasedOnParents_Steps[,,numberOfFrames-1,]
  
  
  #fix flipping issue
  for (k in 1:numberOfSteps) {
    for (i in 2:numberOfFrames) {
      z_temp<-twistingFramesBasedOnParents_Steps[3,,i,k]
      I3<-c(0,0,1)
      tempFlipAngle<-geodesicDistance(z_temp,I3)
      if(tempFlipAngle>flippingTolorance){
        R1<-rotMat(z_temp,I3)
        twistingFramesBasedOnParents_Steps[,,i,k]<-twistingFramesBasedOnParents_Steps[,,i,k]%*%t(R1)
      }
    }
  }
  
  # # # #plot frames of a step
  # step<-2
  # vectors3d(t(twistingFramesBasedOnParents_Steps[1,,,step]),color="blue")
  # vectors3d(t(twistingFramesBasedOnParents_Steps[2,,,step]),color="red")
  # vectors3d(t(twistingFramesBasedOnParents_Steps[3,,,step]),color="green")
  # vectors3d(t(frenetFramesBasedOnParents_Steps[1,,,step]),color="darkblue")
  # vectors3d(t(frenetFramesBasedOnParents_Steps[2,,,step]),color="darkred")
  # vectors3d(t(frenetFramesBasedOnParents_Steps[3,,,step]),color="darkgreen")

  #connections' lengths steps
  connectionsLengths_steps<-t(pathsBetween_RC_vectors_In6DHyperbola[,3,])
  # connectionsLengths_steps<-connectionsLengths_stepsTemp[2:nrow(connectionsLengths_stepsTemp),]
  
  #radii steps
  ellipseRadii_a_steps<-t(pathsBetween_RC_vectors_In6DHyperbola[,1,])
  ellipseRadii_b_steps<-t(pathsBetween_RC_vectors_In6DHyperbola[,2,])
  
  
  tubes<-list()
  for (j in 1:numberOfSteps) {
    tubes[[j]]<-create_Elliptical_Tube(numberOfFrames = numberOfFrames,
                                       method = "basedOnTwistingFrames",
                                       twistingFramesBasedOnParents = twistingFramesBasedOnParents_Steps[,,,j],
                                       ellipseRadii_a = ellipseRadii_a_steps[,j],
                                       ellipseRadii_b = ellipseRadii_b_steps[,j],
                                       connectionsLengths = connectionsLengths_steps[,j],
                                       plotting = FALSE,
                                       add = FALSE)
  }
  
  #plot tubes
  if(plotting==TRUE){
    for (j in 1:numberOfSteps) {
      plot_Elliptical_Tube(tubes[[j]],
                           plot_boundary = TRUE,
                           plot_frames = TRUE,
                           plot_skeletal_sheet = FALSE,
                           plot_r_project = FALSE)
    } 
  }
  
  return(tubes)
  
}



convert_Matrix_In_6DHyperbola_to_tube <- function(meanMatrix_In_6DHyperbola,
                                                  taloranceTwist=pi/10000,
                                                  flippingTolorance=pi/20) {
  
  numberOfFrames<-dim(meanMatrix_In_6DHyperbola)[1]
  
  unitTangentBasedOnParents<-array(NA,dim = c(numberOfFrames,3))
  for (i in 1:numberOfFrames) {
    unitTangentBasedOnParents[i,]<-
      extractTangetVectorFrom_RCV_vectorIn6DHyperbola(
        RCV_vectorIn6DHyperbola = meanMatrix_In_6DHyperbola[i,])
  }
  
  # Frenet frames based on parents
  # parents are previous twisting frames !!!!
  frenetFramesBasedOnParents<-array(NA,dim=c(3,3,numberOfFrames))
  frenetFramesBasedOnParents[,,1]<-diag(3)
  for (i in 2:numberOfFrames) {
    #calculate theta by constructing the Frenet frame
    b_vec<-unitTangentBasedOnParents[i,]
    u2<-b_vec
    u1<-c(-1,0,0)
    thetaTemp<-geodesicDistance(u1,u2)
    if(abs(thetaTemp-pi)>10^-7){
      #formula from geodesicPathOnUnitSphere()
      N_vec<-1/sin(thetaTemp)*(sin(pi/2)*u1+sin(thetaTemp-pi/2)*u2)  
      b_perb_vec<-convertVec2unitVec2(myCrossProduct(b_vec,N_vec))
      # FrenetFrame<-matrix(as.SO3(rbind(b_vec,N_vec,b_perb_vec)),3,3)
      FrenetFrame<-rbind(b_vec,N_vec,b_perb_vec)
    }else{
      FrenetFrame<-diag(3)
    }
    frenetFramesBasedOnParents[,,i]<-FrenetFrame
  }
  
  thetaTwists<-meanMatrix_In_6DHyperbola[,4]
  twistingFramesBasedOnParents<-array(NA,dim=c(3,3,numberOfFrames))
  twistingFramesBasedOnParents[,,1]<-diag(3)
  for (i in 2:numberOfFrames) {
    tempFrame<-frenetFramesBasedOnParents[,,i]
    rotAxis<-tempFrame[1,]
    thetaTwistTemp<-thetaTwists[i]
    
    if(abs(thetaTwistTemp)<taloranceTwist){
      
      R1<-rotMat(tempFrame[1,],c(1,0,0))
      if(anyNA(R1)){
        twistingFramesBasedOnParents[,,i]<-diag(3)  
      }else{
        twistingFramesBasedOnParents[,,i]<-diag(3)%*%R1
      }
      
    }else{
      # rotate frame by theta angle around the rotAxis clockwise
      R_temp<-rotaxisMat(u = rotAxis,theta = thetaTwistTemp)
      # R_temp<-rotationAbout_uAxis_byThetaAngle(u = rotAxis,theta = thetaTwistTemp)
      twistingFramesBasedOnParents[,,i]<-tempFrame%*%R_temp 
    }
  }
  twistingFramesBasedOnParents[,,numberOfFrames]<-
    twistingFramesBasedOnParents[,,numberOfFrames-1]
  
  
  #fix flipping issue
  for (i in 2:numberOfFrames) {
    z_temp<-twistingFramesBasedOnParents[3,,i]
    I3<-c(0,0,1)
    tempFlipAngle<-geodesicDistance(z_temp,I3)
    if(tempFlipAngle>flippingTolorance){
      R1<-rotMat(z_temp,I3)
      twistingFramesBasedOnParents[,,i]<-twistingFramesBasedOnParents[,,i]%*%t(R1)
    }
  }
  
  # #plot frames of a step
  # vectors3d(t(twistingFramesBasedOnParents[1,,]),color="blue")
  # vectors3d(t(twistingFramesBasedOnParents[2,,]),color="red")
  # vectors3d(t(twistingFramesBasedOnParents[3,,]),color="green")
  # vectors3d(t(frenetFramesBasedOnParents[1,,]),color="darkblue")
  # vectors3d(t(frenetFramesBasedOnParents[2,,]),color="darkred")
  # vectors3d(t(frenetFramesBasedOnParents[3,,]),color="darkgreen")

  #connections' lengths steps
  connectionsLengths<-meanMatrix_In_6DHyperbola[,3]
  
  #radii steps
  ellipseRadii_a<-meanMatrix_In_6DHyperbola[,1]
  ellipseRadii_b<-meanMatrix_In_6DHyperbola[,2]
  
  
  tube<-create_Elliptical_Tube(numberOfFrames = numberOfFrames,
                               method = "basedOnTwistingFrames",
                               twistingFramesBasedOnParents = twistingFramesBasedOnParents,
                               ellipseRadii_a = ellipseRadii_a,
                               ellipseRadii_b = ellipseRadii_b,
                               connectionsLengths = connectionsLengths,
                               plotting = FALSE,
                               add = FALSE)
  return(tube)
  
}



mean_tube_basedOnIntrinsicSweptCoordinate <- function(tubes,
                                                      taloranceTwist=pi/10000,
                                                      flippingTolorance=pi/20,
                                                      plotting=TRUE) {
  
  numberOftubes<-length(tubes)
  tube1<-calculate_RC_vectorsIn6DHyperbola_tube(tube = tubes[[1]])
  numberOfFrames<-dim(tube1)[1]
  
  RC_VectorsIn6DHyperbola_tubes<-array(NA,dim = c(dim(tube1),numberOftubes))
  for (i in 1:numberOftubes) {
    RC_VectorsIn6DHyperbola_tubes[,,i]<-calculate_RC_vectorsIn6DHyperbola_tube(tube = tubes[[i]])
  }
  
  array_sweptCoordinates_6DCylinder<-array(NA,dim = dim(RC_VectorsIn6DHyperbola_tubes))
  for (j in 1:numberOftubes) {
    for (i in 1:numberOfFrames) {
      array_sweptCoordinates_6DCylinder[i,,j]<-hyperbola6DWithXaxis_to_sweptCoordinate_3Dtubes(
        vector_a_b_x_thetaTwist_projectedVec = RC_VectorsIn6DHyperbola_tubes[i,,j])  
    }
  }
  
  sumMatrix<-array(0,dim = dim(array_sweptCoordinates_6DCylinder[,,1]))
  for (i in 1:numberOftubes) {
    sumMatrix<-sumMatrix+array_sweptCoordinates_6DCylinder[,,i]
  }
  meanMatrix_In_6DCylinder<-sumMatrix/numberOftubes
  
  
  meanMatrix_In_6DHyperbola<-t(apply(meanMatrix_In_6DCylinder,
                                     MARGIN = 1,
                                     FUN = sweptCoordinate_to_hyperbola6DWithXaxis_3Dtubes))
  
  #convert mean matrix to a tube
  meantube<-convert_Matrix_In_6DHyperbola_to_tube(meanMatrix_In_6DHyperbola,
                                                  taloranceTwist = taloranceTwist,
                                                  flippingTolorance = flippingTolorance)
  
  if(plotting==TRUE){
    
    plot_Elliptical_Tube(meantube,
                         plot_frames = FALSE)
    
  }
  
  return(meantube)
  
}


intrinsic_Distance_Between2tubes <- function(tube1,tube2) {
  
  if(dim(tube1$frenetFramesBasedOnParents)[3]!=dim(tube2$frenetFramesBasedOnParents)[3]){
    stop('Number of cross-sections are not the same!')
  }
  
  numberOfFrames<-dim(tube1$frenetFramesBasedOnParents)[3]
  
  RC_VectorsIn6DHyperbola_tube1<-calculate_RC_vectorsIn6DHyperbola_tube(tube = tube1)
  RC_VectorsIn6DHyperbola_tube2<-calculate_RC_vectorsIn6DHyperbola_tube(tube = tube2)
  
  sweptCoordinates_tube1<-array(NA,dim = dim(RC_VectorsIn6DHyperbola_tube1))
  sweptCoordinates_tube2<-array(NA,dim = dim(RC_VectorsIn6DHyperbola_tube2))
  for (i in 1:numberOfFrames) {
    sweptCoordinates_tube1[i,]<-hyperbola6DWithXaxis_to_sweptCoordinate_3Dtubes(
      vector_a_b_x_thetaTwist_projectedVec = RC_VectorsIn6DHyperbola_tube1[i,]) 
    sweptCoordinates_tube2[i,]<-hyperbola6DWithXaxis_to_sweptCoordinate_3Dtubes(
      vector_a_b_x_thetaTwist_projectedVec = RC_VectorsIn6DHyperbola_tube2[i,]) 
  }
  
  #scale radii and 'connections' lengths based on log operator
  sweptCoordinates_tube1[,1:3]<-log(sweptCoordinates_tube1[,1:3])
  sweptCoordinates_tube2[,1:3]<-log(sweptCoordinates_tube2[,1:3])
  sweptCoordinates_tube1[1,3]<-0
  sweptCoordinates_tube2[1,3]<-0
  
  euclideanDistance<-sum(sqrt(rowSums(sweptCoordinates_tube1-sweptCoordinates_tube2)^2))
  
  return(euclideanDistance)
  
}



generate_one_Random_tube <- function(meantube,
                                     sd_a=0.1,
                                     sd_b=0.1,
                                     sd_x=0.1,
                                     sd_theta=0.1,
                                     sd_projectedVector=0.1,
                                     plotting=TRUE) {
  
  numberOfFrames<-dim(meantube$frenetFramesBasedOnParents)[3]
  
  RC_VectorsIn6DHyperbola<-calculate_RC_vectorsIn6DHyperbola_tube(tube = meantube)
  
  sweptCoordinates<-array(NA,dim = dim(RC_VectorsIn6DHyperbola))
  for (i in 1:numberOfFrames) {
    sweptCoordinates[i,]<-hyperbola6DWithXaxis_to_sweptCoordinate_3Dtubes(
      vector_a_b_x_thetaTwist_projectedVec = RC_VectorsIn6DHyperbola[i,]) 
  }
  
  #for projected vector
  a_mean<-sweptCoordinates[,1]
  b_mean<-sweptCoordinates[,2]
  x_mean<-sweptCoordinates[,3]
  theta_mean<-sweptCoordinates[,4]
  projectedVectorsMean<-sweptCoordinates[,c(5,6)]
  
  random_a<-rep(NA,numberOfFrames)
  for (i in 1:numberOfFrames) {
    random_a[i]<-rtruncnorm(n = 1,a = 0.001,b = a_mean[i]+10*sd_a,mean = a_mean[i],sd = sd_a)
  }
  random_b<-rep(NA,numberOfFrames)
  for (i in 1:numberOfFrames) {
    random_b[i]<-rtruncnorm(n = 1,a = 0.001,b = random_a[i],mean = b_mean[i],sd = sd_b)
  }
  random_x<-rep(0,numberOfFrames)
  for (i in 2:numberOfFrames) {
    random_x[i]<-rtruncnorm(n = 1,a = 0.001,b = x_mean[i]+10*sd_x,mean = x_mean[i],sd = sd_x)
  }
  random_theta<-rep(0,numberOfFrames)
  for (i in 2:numberOfFrames) {
    random_theta[i]<-rtruncnorm(n = 1,a = -pi/4,b = pi/4,mean = theta_mean[i],sd = sd_theta)
  }
  
  random_ProjectedVectors<-array(NA,dim = c(numberOfFrames,2))
  k<-1
  while (k<=numberOfFrames) {
    tempPoint<-rmvnorm(n = 1,mu = projectedVectorsMean[k,],Sigma = sd_projectedVector*diag(2))  
    if(tempPoint[1]^2+tempPoint[2]^2<1){
      random_ProjectedVectors[k,]<-tempPoint
      k<-k+1
    }
  }
  
  random_sweptCoordinates<-cbind(random_a,
                                 random_b,
                                 random_x,
                                 random_theta,
                                 random_ProjectedVectors)
  
  random_Matrix_In_6DHyperbola<-t(apply(random_sweptCoordinates,
                                        MARGIN = 1,
                                        FUN = sweptCoordinate_to_hyperbola6DWithXaxis_3Dtubes))
  
  #convert mean matrix to a tube
  random_tube<-convert_Matrix_In_6DHyperbola_to_tube(meanMatrix_In_6DHyperbola = random_Matrix_In_6DHyperbola,
                                                     taloranceTwist= pi/1000000,
                                                     flippingTolorance = pi/20)
  
  if(plotting==TRUE){
    plot_Elliptical_Tube(random_tube)
  }
  
  return(random_tube)
  
}

