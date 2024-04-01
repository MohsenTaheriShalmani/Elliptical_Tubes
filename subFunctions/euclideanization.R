# copyright belongs to Sungkyu Jung
# converted code from MATLAB
# output is the type of fitted circle 
# likelihood ratio test from shapes package
LRTpval2 <- function(resGreat,resSmall,n) {
  chi2 <- max(n*log(sum(resGreat^2)/sum(resSmall^2)))
  pval <- 1-pchisq(q = chi2, df = 1, lower.tail = T) # likelihood test p-value Also you can use chi2cdf(chi2,1) from library(PEIP) like matlab
}

kurtosisTestFunction <- function(sphericalData, alpha=0.1) {
  ndata<-dim(sphericalData)[2]
  
  subsphereSmall<-getSubSphere(sphericalData,geodesic = "small")
  subsphereGreat<-getSubSphere(sphericalData,geodesic = "great")
  
  currentSphere<-sphericalData
  
  rSmall<-subsphereSmall$r                     # rSmall is rs in matlab
  centerSmall<-subsphereSmall$center           # NB! center is the centerSmall is pnsSmall$PNS$orthaxis[[1]]
  # and centers in matlab
  resSmall <- acos(t(centerSmall)%*%currentSphere)-rSmall  # NB!!! resSmall==(pnsSmall$resmat)[2,] i.e., residuals are second coordinates of PNS
  
  rGreat<-subsphereGreat$r                     # rGreat is rg in matlab
  centerGreat<-subsphereGreat$center           # centerGreat is centers in matlab
  resGreat <- acos(t(centerGreat)%*%currentSphere)-rGreat  # NB!!! resGreat==(pnsGreat$resmat)[2,] i.e., residuals are second coordinates of PNS
  
  # LRTpval is the likelihood ratio test from 'shapes' package
  # Chi-squared statistic for a likelihood test
  pval1 <- LRTpval(resGreat,resSmall,n = ndata)
  pval1
  
  if(pval1>alpha){
    print('great by likelihood ratio test')
    return('great')
    break
  }
  
  # # equivalently we can find pval by pns function
  # pnsTest2<-pns(sphericalData)
  # pnsTest2$PNS$pvalues
  # sum(pnsTest2$resmat[2,]==resSmall)
  
  # kurtosis test routine
  X <- LogNPd(rotMat(centerSmall) %*% currentSphere)
  
  # plot3d(t(sphericalData),type="p",expand=10, add=TRUE)
  # plot3d(t(rbind(X,rep(1,dim(X)[2]))),type="p",col = "blue",expand=10, add=TRUE)
  
  # Note that the tangential point is the center of the small circle
  d<-dim(X)[1]
  n<-dim(X)[2]
  normX2 <- colSums(X^2)
  kurtosis <- sum( normX2^2 ) / n / ( sum( normX2 ) / (d * (n-1)) )^2
  M_kurt <- d * (d+2)^2 / (d+4)
  V_kurt <- (1/n) * (128*d*(d+2)^4) / ((d+4)^3*(d+6)*(d+8))
  pval2 <- pnorm((kurtosis - M_kurt) / sqrt(V_kurt))
  
  if(pval2>alpha){
    return('great')
  }else{
    # drawCircleS2(normalVec = centerSmall,radius = rSmall)
    return('small')
  }
}


euclideanization <- function(directions1,directions2,type="PNS") {
  
  nSamplesG1<-dim(directions1)[2]
  nSamplesG2<-dim(directions2)[2]
  
  if(type=="PNS"){
    
    # use pns instead of Fre'chet mean
    pooledDirection<-cbind(directions1,directions2)
    typeOfSphere<-kurtosisTestFunction(sphericalData = pooledDirection)
    pnsDirection<-pns(pooledDirection,sphere.type = typeOfSphere) #pooled directions
    res_G1<-t(pnsDirection$resmat[,1:nSamplesG1])
    res_G2<-t(pnsDirection$resmat[,(nSamplesG1+1):(nSamplesG1+nSamplesG2)])
    
    result<-list(euclideanG1=res_G1, euclideanG2=res_G2)
    
  }else if(type=="tangent space"){
    
    # mean by Fre'chet mean
    allDirTemp<-t(cbind(directions1,directions2))
    data1 <- list()
    for (j in 1:dim(allDirTemp)[1]){
      data1[[j]] <-allDirTemp[j,]
    }
    data2 <- riemfactory(data1, name="sphere")
    # Fre'chet Mean
    out1<- rbase.mean(data2)
    mu_g<-as.vector(out1$x)
    
    R <- rotMat(mu_g,c(0,0,1))
    shiftedG1<-R%*%directions1
    shiftedG2<-R%*%directions2
    
    theta1<-acos(shiftedG1[3,])
    logPointG1x<-shiftedG1[1,]*theta1/sin(theta1)
    logPointG1y<-shiftedG1[2,]*theta1/sin(theta1)
    logPointG1z<-rep(1,nSamplesG1)
    logG1<-cbind(logPointG1x,logPointG1y,logPointG1z)
    
    theta2<-acos(shiftedG2[3,])
    logPointG2x<-shiftedG2[1,]*theta2/sin(theta2)
    logPointG2y<-shiftedG2[2,]*theta2/sin(theta2)
    logPointG2z<-rep(1,nSamplesG2)
    logG2<-cbind(logPointG2x,logPointG2y,logPointG2z)
    
    result<-list(euclideanG1=logG1[,1:2], euclideanG2=logG2[,1:2])
    
    
  }else{
    stop("Please choose the type of analysis i.e., 'PNS' residual or 'tangent space'!")
  }
  
  return(result)
}

