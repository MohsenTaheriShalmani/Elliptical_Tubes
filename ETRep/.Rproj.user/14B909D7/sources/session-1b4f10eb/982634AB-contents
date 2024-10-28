# Math functions

# function to calculate Mahalanobis distance (Hotelling Metric)
#' @keywords internal
.MahalanobisDistance<-function(X,Y){
  k<-dim(X)[2]
  nx<-dim(X)[1]
  ny<-dim(Y)[1]
  Sx<-cov(X)
  Sy<-cov(Y)
  meanX<-colMeans(X)
  meanY<-colMeans(Y)
  n<-nx+ny-1
  S<-((nx-1)*Sx+(ny-1)*Sy)/(nx+ny-2) #S=pooled covariance matrix
  # T2<-t(meanX-meanY)%*%solve(S*(1/nx+1/ny))%*%(meanX-meanY)
  T2<-t(meanX-meanY)%*%matlib::Ginv(S*(1/nx+1/ny))%*%(meanX-meanY) # Ginv is the Moore-Penrose generalized inverse
  return(as.double(T2))
}

#' @keywords internal
.MahalanobisDistance_1D<-function(X,Y){
  nx<-length(X)
  ny<-length(Y)
  Sx<-var(X)
  Sy<-var(Y)
  meanX<-mean(X)
  meanY<-mean(Y)
  n<-nx+ny-1
  S<-((nx-1)*Sx+(ny-1)*Sy)/(nx+ny-2) #S=pooled variance
  T2<-abs(meanX-meanY)*(1/(S*sqrt(1/nx+1/ny)))
  return(T2)
}

# Hotelling T2 test
#' @keywords internal
.HotellingT2<-function(X,Y){
  if(dim(X)[2]!=dim(Y)[2]){
    stop("Dimension Error!\n")
  }
  k<-dim(X)[2]
  nx<-dim(X)[1]
  ny<-dim(Y)[1]
  Sx<-cov(X)
  Sy<-cov(Y)
  meanX<-colMeans(X)
  meanY<-colMeans(Y)
  n<-nx+ny-1
  S<-((nx-1)*Sx+(ny-1)*Sy)/(nx+ny-2) #S=pooled covariance matrix
  # T2<-t(meanX-meanY)%*%solve(S*(1/nx+1/ny))%*%(meanX-meanY)
  T2<-t(meanX-meanY)%*%matlib::Ginv(S*(1/nx+1/ny))%*%(meanX-meanY) # Ginv is the Moore-Penrose generalized inverse
  F_value<-((n-k)/(k*(n-1)))*T2
  df1<-k
  df2<-n-k
  p_value<-1-pf(F_value,df1,df2)
  return(p_value)
}

#' @keywords internal
.geodesicPathOnUnitSphere <- function(point1, point2, numberOfneededPoints=100) {

  u1<-.convertVec2unitVec(point1)
  u2<-.convertVec2unitVec(point2)
  theta<-.geodesicDistance(u1,u2)

  if(identical(u1, u2)){

    geodesicPoints<-base::matrix(rep(u1,numberOfneededPoints),ncol = length(u1),byrow = TRUE)

  }else{
    tau<-seq(from=0,to=1,length.out=numberOfneededPoints)

    geodesicPoints<-array(NA,dim = c(numberOfneededPoints,length(u1)))
    for (i in 1:numberOfneededPoints) {
      geodesicPoints[i,]<-1/sin(theta)*(sin(theta*(1-tau[i]))*u1+sin(theta*tau[i])*u2)
    }
  }

  return(geodesicPoints)
}


# convert vectors to unit vectors
#' @keywords internal
.convertVec2unitVec <- function(vec) {
  if(norm(vec,type = "2")==0){
    stop("vector is zero!")
  }
  return(vec/norm(vec,type = "2"))
}

# convert vectors to unit vector or c(1,0,0)
#' @keywords internal
.convertVec2unitVec2 <- function(vec) {
  if(norm(vec,type = "2")==0){
    return(c(1,rep(0,length(vec)-1)))
  }
  return(vec/norm(vec,type = "2"))
}

# cross product of 2 vectors
#' @keywords internal
.myCrossProduct <- function(v,u) {
  return(c(v[2]*u[3]-v[3]*u[2],v[3]*u[1]-v[1]*u[3],v[1]*u[2]-v[2]*u[1]))
}

# generate random von Mises distribution on circle in radian
# converted code of Sungkyu Jung 2013, and Byungwon Kim 2017, MATLAB .randVonMises.m
#' @keywords internal
.randVonMises <- function(n, mean, kappa) {
  tau<-1+sqrt(1+4*kappa^2)
  rho<-(tau-sqrt(2*tau))/(2*kappa)
  r<-(1+rho^2)/(2*rho)

  u1<-runif(n)
  z<-cos(pi*u1)
  f<-(1+r*z)/(r+z)
  c<-kappa*(r-f)
  u2<-runif(n)
  acceptanceid<-(c*(2-c)-u2>0) | (log(c/u2)+1-c>=0)
  u3<-runif(sum(acceptanceid))
  theta<-sign(u3-0.5)*acos(f[acceptanceid])
  nnow<-length(theta)

  while (n>nnow) {
    n_more<-ceiling(n/nnow*(n-nnow))
    u1<-runif(n_more)
    z<-cos(pi*u1)
    f<-(1+r*z)/(r+z)
    c<-kappa*(r-f)
    u2<-runif(n_more)
    acceptanceid<-(c*(2-c)-u2>0) | (log(c/u2)+1-c>=0)
    u3<-runif(sum(acceptanceid))
    thetamore<-sign(u3-0.5)*acos(f[acceptanceid])

    theta<-c(theta, thetamore)
    nnow<-length(theta)
  }

  theta<-theta[1:n] + mean

  return(theta)
}
# randVonMisesSamples<-.randVonMises(mean = pi/4,kappa = 10,n = 2000)
# hist(randVonMisesSamples)
# plotshapes(cbind(cos(randVonMisesSamples),sin(randVonMisesSamples)))

#generate ellipsoid PDM
#' @keywords internal
.ellipsoidGenerator_2D <- function(center,a,b,n,n2){
  phi<-seq(0, 2*pi, length.out = 4*n+1)
  points<-center
  for (i in phi[1:(length(phi)-1)]) {
    for (j in 1:n2) {
      x<-a*cos(i)/n2*j
      y<-b*sin(i)/n2*j
      points<-rbind(points,center+c(x,y))
    }
  }
  return(points)
}
# plot(.ellipsoidGenerator_2D(center = c(2,3),a = 1,b = 1,10,1),xlim = c(0,5),ylim = c(0,5),xlab = "",ylab = "")

#generate ellipsoid PDM without center
#' @keywords internal
.ellipsoidGenerator_2D_2 <- function(center,a,b,n,n2){
  phi<-seq(0, 2*pi, length.out = 4*n+1)
  # points<-center
  points<-c()
  for (i in phi[1:(length(phi)-1)]) {
    for (j in 1:n2) {
      x<-a*cos(i)/n2*j
      y<-b*sin(i)/n2*j
      points<-rbind(points,center+c(x,y))
    }
  }
  return(points)
}


#generate ellipsoid PDM without center
#' @keywords internal
.sphereGenerator_2D <- function(center,r=1,n=16,asymmetric=TRUE){
  if(asymmetric==TRUE & (n %% 2) != 0){
    n<-n+1
  }
  phi<-seq(0, 2*pi, length.out = n)
  points<-array(NA,dim = c(n-1,2))
  for (i in 1:(n-1)) {
    x<-r*cos(phi[i])
    y<-r*sin(phi[i])
    points[i,]<-center+c(x,y)
  }
  return(points)
}

# return the intersection of a ray with a triangle if the intersection exist
#' @keywords internal
.rayTriangleIntersection <- function(rayOrigin,rayDirection,triangleVertex1,triangleVertex2,triangleVertex3) {

  O<-rayOrigin #origin of the ray
  D<-rayDirection #direction of the ray
  A<-triangleVertex1 #triangle vertices
  B<-triangleVertex2
  C<-triangleVertex3

  E1<-B-A
  E2<-C-A
  N<-.myCrossProduct(E1,E2)

  det<-(-sum(D*N))
  invdet <- 1/det
  AO<-O-A
  DAO<-.myCrossProduct(AO,D)
  u<-sum(E2*DAO)*invdet
  v<-(-sum(E1*DAO)*invdet)
  t<-sum(AO*N)*invdet
  if (abs(det) >= 1e-6 & t >= 0 & u >= 0 & v >= 0 & (u+v) <= 1){
    intersection<-O + t * D
  }else{
    intersection<-c(NA,NA,NA)
  }
  return(intersection)
}

#' @keywords internal
.rayTriangleIntersection2D <- function(rayOrigin,
                                      rayDirection,
                                      point1,
                                      point2) {
  v1 = rayOrigin - point1
  v2 = point2 - point1
  v3 = c(-rayDirection[2], rayDirection[1])


  dotProduct = sum(v2 * v3)
  if (abs(dotProduct) < 1e-6){
    return(c(NA,NA))
  }else{
    t1 = (v1[2]*v2[1]-v1[1]*v2[2]) / dotProduct

    t2 = (v1 * v3) / sum(v2*v3)

    if (t1 >= 0.0 && (t2 >= 0.0 && t2 <= 1.0)){
      return(rayOrigin+t1*rayDirection)
    }else{
      return(c(NA,NA))
    }
  }
}


# calculate triangle area
#' @keywords internal
.triangleArea <- function(p1,p2,p3) {
  .myCrossProduct((p3-p2),(p2-p1))
}

#' @keywords internal
.intersectionPointsBetween2Ellipses_In3D <- function(ellipse1,ellipse2,plotting=TRUE) {

  centroidEllipse1<-colMeans(ellipse1)
  centroidEllipse2<-colMeans(ellipse2)

  ellipse_withCentroid_1<-rbind(centroidEllipse1,ellipse1)
  ellipse_withCentroid_2<-rbind(centroidEllipse2,ellipse2)

  verts_1<-rbind(t(base::as.matrix(ellipse_withCentroid_1)),1)
  verts_2<-rbind(t(base::as.matrix(ellipse_withCentroid_2)),1)

  polyMatrix_1<-c()
  for (i in 1:(nrow(ellipse_withCentroid_1)-2)) {
    polyMatrix_1<-rbind(polyMatrix_1,c(1,i+1,i+2))
  }
  polyMatrix_1<-rbind(polyMatrix_1,c(1,nrow(ellipse_withCentroid_1),2))
  polyMatrix_2<-c()
  for (i in 1:(nrow(ellipse_withCentroid_2)-2)) {
    polyMatrix_2<-rbind(polyMatrix_2,c(1,i+1,i+2))
  }
  polyMatrix_2<-rbind(polyMatrix_2,c(1,nrow(ellipse_withCentroid_2),2))

  if(plotting==TRUE){
    v1_e1<-centroidEllipse1-ellipse1[1,]
    v2_e1<-centroidEllipse1-ellipse1[2,]
    normal_ellipse_1<-.convertVec2unitVec2(.myCrossProduct(v1_e1,v2_e1))
    normals_ellipse_1<-matrix(rep(normal_ellipse_1,nrow(ellipse_withCentroid_1)),ncol = 3,byrow = TRUE)

    v1_e2<-centroidEllipse2-ellipse2[1,]
    v2_e2<-centroidEllipse2-ellipse2[2,]
    normal_ellipse_2<-.convertVec2unitVec2(.myCrossProduct(v1_e2,v2_e2))
    normals_ellipse_2<-matrix(rep(normal_ellipse_2,nrow(ellipse_withCentroid_2)),ncol = 3,byrow = TRUE)

    trgls_1 <- base::as.matrix(t(polyMatrix_1))
    normals_1 <- base::as.matrix(t(normals_ellipse_1))
    tmesh_1<-rgl::mesh3d(vertices = verts_1, triangles = trgls_1,normals = normals_1)
    tmesh_1 <- Rvcg::vcgUpdateNormals(tmesh_1)

    trgls_2 <- base::as.matrix(t(polyMatrix_2))
    normals_2 <- base::as.matrix(t(normals_ellipse_2))
    tmesh_2<-rgl::mesh3d(vertices = verts_2, triangles = trgls_2,normals = normals_2)
    tmesh_2 <- Rvcg::vcgUpdateNormals(tmesh_2)

    # wire3d(tmesh_1, col="blue")  #wire mesh
    rgl::shade3d(tmesh_1, col="blue")
    rgl::shade3d(tmesh_2, col="red")
  }

  intersectionPoints<-c()
  for (k in 1:nrow(ellipse1)) {
    # ellipse 1 intersect with ellipse 2
    rayDirectionTemp<-.convertVec2unitVec2(ellipse1[k,]-centroidEllipse1)
    for (i in 1:nrow(polyMatrix_2)) {
      intersectionTemp<-.rayTriangleIntersection(rayOrigin = centroidEllipse1,
                                                rayDirection = rayDirectionTemp,
                                                triangleVertex1 = ellipse_withCentroid_2[polyMatrix_2[i,1],],
                                                triangleVertex2 = ellipse_withCentroid_2[polyMatrix_2[i,2],],
                                                triangleVertex3 = ellipse_withCentroid_2[polyMatrix_2[i,3],])

      if(!anyNA(intersectionTemp)){
        d1<-norm(ellipse1[k,]-centroidEllipse1,type = "2")
        d2<-norm(intersectionTemp-centroidEllipse1,type = "2")
        if(d1>=d2){
          intersectionPoints<-rbind(intersectionPoints,intersectionTemp)
        }
      }
    }
  }

  if(plotting==TRUE){
    plot3d(rbind(intersectionPoints,intersectionPoints),type = 'p',col='blue',expand = 10,box=FALSE,add = TRUE)
  }

  return(intersectionPoints)
}


#' @keywords internal
.rotateFrameToMainAxes_standard <- function(myFrame, vectors2rotate) {

  if(!is.SO3(myFrame)){
    stop('The frame does not belong to SO(3)!')
  }

  # R1<-shapes::rotMat(myFrame[1,],c(1,0,0))
  # rotatedFrame1<-myFrame%*%t(R1)
  # R2<-shapes::rotMat(rotatedFrame1[2,],c(0,1,0))
  # rotatedVectors<-vectors2rotate%*%t(R1)%*%t(R2) #first rotate R1 then by R2.
  #                                                 #NB!! t(R1)%*%t(R2)=t(R2%*%R1)

  rotatedVectors<-vectors2rotate%*%t(myFrame)
  return(rotatedVectors)
}

#' @keywords internal
.rotateFrameToMainAxesAndRotateBack_standard <- function(myFrame, vectors_In_I_Axes) {

  if(!is.SO3(myFrame)){
    stop('The frame does not belong to SO(3)!')
  }

  # R1<-shapes::rotMat(myFrame[1,],c(1,0,0))
  # rotatedFrame1<-myFrame%*%t(R1)
  # R2<-shapes::rotMat(rotatedFrame1[2,],c(0,1,0))
  # # R_back<-solve(t(R1)%*%t(R2)) #NB! first rotate by R1 then by R2 then inverse but
  # R_back<-R2%*%R1 # because solve(t(R1)%*%t(R2))=solve(t(R2%*%R1))=t(t(R2%*%R1))=R2%*%R1
  # rotatedVectors<-vectors_In_I_Axes%*%R_back #NB! myFrame%*%t(R1)%*%t(R2)%*%R2%*%R1=myFrame

  rotatedVectors<-vectors_In_I_Axes%*%myFrame

  return(rotatedVectors)
}


#convert 3D Cartesian coordinate to spherical coordinate
#' @keywords internal
.cartesian_to_spherical <- function(v) {
  if(length(v)!=3){stop("v is not a 3-dimensional vector!")}

  x<-v[1]
  y<-v[2]
  z<-v[3]

  r <- sqrt(x^2 + y^2 + z^2)
  phi <- atan2(y, x)
  theta <- -atan2(z, sqrt(x^2 + y^2))

  #phi is the yaw angle and theta is the pitch angle
  return(list(r = r, phi = phi, theta = theta))
}

#convert 3D spherical coordinate to Cartesian coordinate
#' @keywords internal
.spherical_to_cartesian <- function(r, phi, theta) {
  #theta is the pitch phi is the yaw
  x <- r * cos(theta) * cos(phi)
  y <- r * cos(theta) * sin(phi)
  z <- r * sin(theta)
  return(c(x , y , z ))
}

#interpolate points between two points
#' @keywords internal
.generatePointsBetween2Points <- function(point1, point2, numberOfPoints) {

  if(identical(point1, point2)){

    middlePoints<-matrix(rep(point1,numberOfPoints),ncol = length(point1),byrow = TRUE)

  }else{

    dimension<-length(point1)

    totalDis<-norm(point1-point2,type = "2")
    smallDis<-seq(0, totalDis, length.out = numberOfPoints)

    direction<-.convertVec2unitVec(point2-point1)

    middlePoints<-array(NA, dim = c(numberOfPoints,dimension))
    for (i in 1:length(smallDis)) {
      tempPoint<-point1+smallDis[i]*direction
      middlePoints[i,]<-tempPoint
    }

  }

  return(middlePoints)
}

#generate spheres between 2 spheres in 2D
#' @keywords internal
.generateSpheresBetween2Points_2D <- function(center1,
                                             center2,
                                             radius1,
                                             radius2,
                                             numberOfSpheres=10,
                                             resolution=20) {

  s1<-.sphereGenerator_2D(center = center1,r = radius1,n = resolution,asymmetric = FALSE)
  s2<-.sphereGenerator_2D(center = center2,r = radius2,n = resolution,asymmetric = FALSE)

  centers<-.generatePointsBetween2Points(point1 = center1, point2 = center2,
                                        numberOfPoints = numberOfSpheres)

  radii<-seq(from=radius1,to=radius2,length.out = numberOfSpheres)

  numberOfBoundaryPoints<-nrow(s1)
  spheres<-array(NA,dim = c(numberOfSpheres*numberOfBoundaryPoints,2))

  for (i in 1:numberOfSpheres) {
    tempSphere<-.sphereGenerator_2D(center = centers[i,],r = radii[i],n = resolution,asymmetric = FALSE)
    spheres[((i-1)*numberOfBoundaryPoints+1):(i*numberOfBoundaryPoints),]<-tempSphere
  }
  return(spheres)
}

#points whithout tips ans tails
#' @keywords internal
.generatePointsBetween2Points2 <- function(point1, point2, numberOfPoints) {

  dimension<-length(point1)

  totalDis<-norm(point1-point2,type = "2")
  smallDis<-seq(0, totalDis, length.out = numberOfPoints)

  direction<-.convertVec2unitVec(point2-point1)

  middlePoints<-c()
  for (i in 2:(length(smallDis)-1)) {
    tempPoint<-point1+smallDis[i]*direction
    middlePoints<-rbind(middlePoints,tempPoint)
  }

  return(middlePoints)
}


#smooth the meshPDM
#' @keywords internal
.generatePointsBetween3Points <- function(point1,point2,point3,numberOf2DspokePoints) {

  centroid<-colMeans(rbind(point1,point2,point3))

  points<-c()

  points<-rbind(points,.generatePointsBetween2Points2(centroid,point1,numberOfPoints = numberOf2DspokePoints))
  points<-rbind(points,.generatePointsBetween2Points2(centroid,point2,numberOfPoints = numberOf2DspokePoints))
  points<-rbind(points,.generatePointsBetween2Points2(centroid,point3,numberOfPoints = numberOf2DspokePoints))

  return(points)
}

#Euclidean norm
#' @keywords internal
.myNorm <- function(vec) {
  return(sqrt(sum(vec^2)))
}

# geodesicDistance between 2 unit vectors
#' @keywords internal
.geodesicDistance <- function(u1,u2) {
  acos(pmin(pmax(sum(u1*u2),-1.0),1.0))
}


#' @keywords internal
.normalOfaVertex <- function(point1,vertex,point2) {

  vecTemp<-.convertVec2unitVec2(point2-point1)

  R1<-shapes::rotMat(c(1,0),c(0,1))

  vertexUnitNormal<-vecTemp%*%t(R1)

  return(vertexUnitNormal)

}

#' @keywords internal
.normalOfaVertex2 <- function(point1,vertex,point2) {

  dx_line1 = vertex[1] - point1[1]
  dy_line1 = vertex[2] - point1[2]

  unitNormal_line1<-c(-dy_line1, dx_line1)/sqrt(sum(c(-dy_line1, dx_line1)^2))

  dx_line2 = point2[1] - vertex[1]
  dy_line2 = point2[2] - vertex[2]

  unitNormal_line2<-c(-dy_line2, dx_line2)/sqrt(sum(c(-dy_line2, dx_line2)^2))

  sumVec<-unitNormal_line1+unitNormal_line2
  vertexUnitNormal<-sumVec/norm(sumVec,type = '2')

  return(vertexUnitNormal)

}

