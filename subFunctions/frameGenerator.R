
secondFrameVectorFunction <- function(p1,vertex,p2,normalVec) {
  
  u1<-convertVec2unitVec(p2-vertex)
  u2<-convertVec2unitVec(p1-vertex)
  
  v<-convertVec2unitVec(u1-u2)

  #projection of tempPoint on tangent space
  #sum(v*normalVec) is dot product that is the point distance from the plane
  projected_point <- v-(sum(v*normalVec)/norm(normalVec,type = "2"))*normalVec
  
  secondVec<-convertVec2unitVec(projected_point)
  
  return(secondVec)
}

# generate the frames
frameGenerator <- function(centeredSkel,medialNormals,
                           framesCenters,framesBackPoints,framesFronts) {
  
  numberOfFrames<-length(framesCenters)
  
  # NB!!! number of frames is equal to upSpoeksNumber
  framesFirstVec<-array(NA,dim = c(numberOfFrames,3))
  framesSecondVec<-array(NA,dim = c(numberOfFrames,3))
  framesThirdVec<-array(NA,dim = c(numberOfFrames,3))
  for (i in 1:numberOfFrames) {
    
    a<-framesBackPoints[i]
    b<-framesCenters[i]
    c<-framesFronts[i]
    
    framesFirstVec[b,]<-medialNormals[b,]
    
    if(c==Inf){
      framesSecondVec[b,]<-secondFrameVectorFunction(p1 = centeredSkel[a,],vertex = centeredSkel[b,],
                                                     p2 =convertVec2unitVec(centeredSkel[b,]-centeredSkel[a,])+centeredSkel[b,],
                                                     normalVec = medialNormals[b,])
    }else{
      framesSecondVec[b,]<-secondFrameVectorFunction(p1 = centeredSkel[a,],vertex = centeredSkel[b,],
                                                             p2 = centeredSkel[c,],normalVec = medialNormals[b,])
    }
    
    framesThirdVec[b,]<-convertVec2unitVec(myCrossProduct(framesFirstVec[b,],framesSecondVec[b,]))
  }
  
  result<-list(framesFirstVec=framesFirstVec,
               framesSecondVec=framesSecondVec,
               framesThirdVec=framesThirdVec)
  
  return(result)
}



source("subFunctions/normalsOfSkeletalSheetBySpline.R")

frameGenerator2 <- function(centeredSkel,
                            framesCenters,
                            framesBackPoints,
                            framesFronts,
                            numberOfFrames,
                            numberOfLayers,
                            numberOf2DspokePoints) {
  
  # normal vectors
  temp<-normalsOfSkeletalSheet(centeredSkel = centeredSkel)
  medialNormals<-temp$medialNormals
  
  framesFirstVec<-array(NA,dim = c(numberOfFrames,3))
  framesSecondVec<-array(NA,dim = c(numberOfFrames,3))
  framesThirdVec<-array(NA,dim = c(numberOfFrames,3))

  for (i in 1:numberOfFrames) {
    
    a<-framesBackPoints[i]
    b<-framesCenters[i]
    c<-framesFronts[i]
    
    framesFirstVec[b,]<-medialNormals[b,]
    
    if(b%%numberOfLayers>(numberOf2DspokePoints-1) | b%%numberOfLayers==0){
      framesSecondVec[b,]<-secondFrameVectorFunction(p1 = centeredSkel[a,],
                                                     vertex = centeredSkel[b,],
                                                     p2 = centeredSkel[c,],
                                                     normalVec = medialNormals[b,]) 
    }else{
      framesSecondVec[b,]<-secondFrameVectorFunction(p1 = centeredSkel[c,],
                                                     vertex = centeredSkel[b,],
                                                     p2 = centeredSkel[a,],
                                                     normalVec = medialNormals[b,])
    }
    
    framesThirdVec[b,]<-convertVec2unitVec(myCrossProduct(framesFirstVec[b,],framesSecondVec[b,]))
  }
  
  # frames
  result<-list(framesFirstVec=framesFirstVec,
               framesSecondVec=framesSecondVec,
               framesThirdVec=framesThirdVec)
  
  return(result)
  
}


frenetFrame3Dcurve <- function(curvePoints) {
  
  tangentVectors<-array(NA,dim = dim(curvePoints))
  normalVectors<-array(NA,dim = dim(curvePoints))
  b_Perb_vectors<-array(NA,dim = dim(curvePoints))
  
  for (i in 2:(nrow(curvePoints)-1)) {
    v1<-curvePoints[i,]-curvePoints[i-1,]
    v2<-curvePoints[i+1,]-curvePoints[i,]
    
    #NB! normal is the radius of the osculating circle in 3D
    u1<-convertVec2unitVec(v1)
    u2<-convertVec2unitVec(v2)
    tangentVectors[i,]<-convertVec2unitVec(u1+u2)
  }
  tangentVectors[1,]<-convertVec2unitVec(curvePoints[2,]-curvePoints[1,])
  tangentVectors[nrow(curvePoints),]<-convertVec2unitVec(curvePoints[nrow(curvePoints),]-curvePoints[nrow(curvePoints)-1,])
  
  
  for (i in 2:(nrow(curvePoints)-1)) {
    v1<-curvePoints[i,]-curvePoints[i-1,]
    v2<-curvePoints[i+1,]-curvePoints[i,]
    
    #NB! normal is the radius of the osculating circle in 3D
    u1<-(-convertVec2unitVec(v1))
    u2<-convertVec2unitVec(v2)
    normalVectors[i,]<-(-convertVec2unitVec(u1+u2))
  }
  
  R_first<-rotMat(tangentVectors[2,],tangentVectors[1,])
  R_last<-rotMat(tangentVectors[nrow(curvePoints)-1,],tangentVectors[nrow(curvePoints),])
  
  normalVectors[1,]<-normalVectors[2,]%*%t(R_first)
  normalVectors[nrow(curvePoints),]<-normalVectors[nrow(curvePoints)-1,]%*%t(R_last)
  
  
  for (i in 1:nrow(normalVectors)) {
    b_Perb_vectors[i,]<-(-myCrossProduct(tangentVectors[i,],normalVectors[i,]))
  }
  
  # we assume all b_perb vectors point toward the north i.e., c(0,0,1)
  for (i in 1:nrow(curvePoints)) {
    if(geodesicDistance(b_Perb_vectors[i,],c(0,0,1))>pi/2){
      b_Perb_vectors[i,]<-(-b_Perb_vectors[i,])
      normalVectors[i,]<-(-normalVectors[i,])
    }
  }
  
  frenetFrames<-array(NA,dim = c(3,3,nrow(curvePoints)))
  for (i in 1:nrow(curvePoints)) {
    frenetFrames[,,i]<-as.SO3(rbind(normalVectors[i,],tangentVectors[i,],b_Perb_vectors[i,]))
  }
  
  return(frenetFrames)
  
}

