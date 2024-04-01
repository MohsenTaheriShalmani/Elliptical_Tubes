library(shapes)

# This function returns the directions after its frame
# rotates to coincide the main axes
# NB!!! vectors2rotate must be a n*3 matrix consists of n vectors 
# myFrame is a 3*3 matrix consists of f1, f2, and f3
rotateFrameToMainAxes <- function(myFrame, vectors2rotate) {
  
  if(!is.SO3(myFrame)){
    stop('The frame does not belong to SO(3)!')
  }
  
  # if(!(round(acos(sum(myFrame[1,]*myFrame[2,])),2)==round(pi/2,2) &
  #      round(acos(sum(myFrame[1,]*myFrame[3,])),2)==round(pi/2,2) &
  #      round(acos(sum(myFrame[2,]*myFrame[3,])),2)==round(pi/2,2) )){
  #   stop("myFrame is not orthonormal!!!")
  # }
  
  R1<-rotMat(myFrame[1,],c(0,0,1))
  rotatedFrame1<-myFrame%*%t(R1)

  R2<-rotMat(rotatedFrame1[2,],c(1,0,0))
  rotatedFrame2<-rotatedFrame1%*%t(R2)

  rotatedVectors<-vectors2rotate%*%t(R2%*%R1) #NB! the theoretical formula is R1%*%R2%*%t(vectors2rotate)
  # frameRotationMatrix<-t(R2%*%R1)             #NB! t(R2%*%R1) must be multiplied from right side
  
  return(rotatedVectors)
}


#this function rotate the (parent) frames to main axes then rotate back the vectors from the main axes
#myFrame must be the parent
rotateFrameToMainAxesAndRotateBack <- function(myFrame, vectorsInMainAxes) {
  
  if(!is.SO3(myFrame)){
    stop('The frame does not belong to SO(3)!')
  }
  
  # if(!(round(acos(sum(myFrame[1,]*myFrame[2,])),2)==round(pi/2,2) &
  #      round(acos(sum(myFrame[1,]*myFrame[3,])),2)==round(pi/2,2) &
  #      round(acos(sum(myFrame[2,]*myFrame[3,])),2)==round(pi/2,2) )){
  #   stop("myFrame is not orthonormal!!!")
  # }
  
  # R1<-rotMat(myFrame[2,],c(1,0,0))
  # rotatedFrame1<-myFrame%*%t(R1)
  # 
  # R2<-rotMat(rotatedFrame1[1,],c(0,0,1))
  # rotatedFrame2<-rotatedFrame1%*%t(R2)
  
  R1<-rotMat(myFrame[1,],c(0,0,1))
  rotatedFrame1<-myFrame%*%t(R1)
  
  R2<-rotMat(rotatedFrame1[2,],c(1,0,0))
  # rotatedFrame2<-rotatedFrame1%*%t(R2)
  
  # frameRotationMatrix<-t(R2%*%R1)            
  R_back<-solve(t(R2%*%R1))
  rotatedBackVectors<-vectorsInMainAxes%*%R_back
  
  
  return(rotatedBackVectors)
}


rotateFrameToMainAxes_standard <- function(myFrame, vectors2rotate) {
  
  # if(!(round(acos(sum(myFrame[1,]*myFrame[2,])),2)==round(pi/2,2) &
  #      round(acos(sum(myFrame[1,]*myFrame[3,])),2)==round(pi/2,2) &
  #      round(acos(sum(myFrame[2,]*myFrame[3,])),2)==round(pi/2,2) )){
  #   stop("myFrame is not orthonormal!!!")
  # }
  
  if(!is.SO3(myFrame)){
    stop('The frame does not belong to SO(3)!')
  }
  
  R1<-rotMat(myFrame[1,],c(1,0,0))
  rotatedFrame1<-myFrame%*%t(R1)
  
  R2<-rotMat(rotatedFrame1[2,],c(0,1,0))
  rotatedFrame2<-rotatedFrame1%*%t(R2)
  
  rotatedVectors<-vectors2rotate%*%t(R2%*%R1) #NB! the theoretical formula is R1%*%R2%*%t(vectors2rotate)
  # frameRotationMatrix<-t(R2%*%R1)             #NB! t(R2%*%R1) must be multiplied from right side
  
  return(rotatedVectors)
}


rotateFrameToMainAxesAndRotateBack_standard <- function(myFrame, vectorsInMainAxes) {
  
  if(!is.SO3(myFrame)){
    stop('The frame does not belong to SO(3)!')
  }
  
  R1<-rotMat(myFrame[1,],c(1,0,0))
  rotatedFrame1<-myFrame%*%t(R1)
  
  R2<-rotMat(rotatedFrame1[2,],c(0,1,0))
  # rotatedFrame2<-rotatedFrame1%*%t(R2)
  
  # frameRotationMatrix<-t(R2%*%R1)            
  R_back<-solve(t(R2%*%R1))
  rotatedBackVectors<-vectorsInMainAxes%*%R_back
  
  
  return(rotatedBackVectors)
}


