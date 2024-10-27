####################################################################
####################################################################
# Required libraries 

library(shapes) 
library(rgl) 
library(Morpho) 
library(matlib)
library(RiemBase)
library(doBy) 
library(plotrix)
library(Directional)
library(RSpincalc)
library(rotations)
library(SphericalCubature)
library(Rvcg) 
library(fields) 
library(Matrix) 
library(pracma) 
library(truncnorm) 
library(ggplot2) 
library(reshape2)
library(dplyr)
library(pracma) 

####################################################################
####################################################################
# Clear the environment

remove(list=ls())

####################################################################
####################################################################
# Set working directory to file location

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

####################################################################
####################################################################
# Load functions 

if(TRUE){
  source("functions/MathFunctions.R")
  source("functions/ETRep_Functions.R")
}

####################################################################
######################## An Example for ############################
##### Intrinsic and Non-Intrinsic Means and Transformations ########
####################################################################
####################################################################

# Create e_tubes A and B

# Create e-tube A
numberOfFrames<-12
addedFrames1<-100
addedFrames2<-40
ellipseResolution<-4
EulerAngles_alpha_A<-c(rep(0,addedFrames1),rep(pi/12,numberOfFrames),rep(0,addedFrames2),rep(-pi/12,numberOfFrames),rep(0,addedFrames2))
EulerAngles_beta_A<--c(rep(0,addedFrames1),rep(0,numberOfFrames),rep(0,addedFrames2),rep(0,numberOfFrames),rep(0,addedFrames2))
EulerAngles_gamma_A<--c(rep(0,addedFrames1),rep(0,numberOfFrames),rep(0,addedFrames2),rep(0,numberOfFrames),rep(0,addedFrames2))
ellipseRadii_a_A<-seq(1,4,length.out=numberOfFrames+addedFrames1+addedFrames2+numberOfFrames+addedFrames2)
ellipseRadii_b_A<-seq(0.9,2,length.out=numberOfFrames+addedFrames1+addedFrames2+numberOfFrames+addedFrames2)
connectionsLengths_A<-rep(1,numberOfFrames+addedFrames1+addedFrames2+numberOfFrames+addedFrames2)

tube_A<-create_Elliptical_Tube(numberOfFrames=length(ellipseRadii_a_A),
                               EulerAngles_Matrix =cbind(EulerAngles_alpha_A,EulerAngles_beta_A,EulerAngles_gamma_A),
                               method="basedOnEulerAngles",
                               ellipseResolution=ellipseResolution,
                               ellipseRadii_a=ellipseRadii_a_A,
                               ellipseRadii_b=ellipseRadii_b_A,
                               connectionsLengths=connectionsLengths_A,
                               plotting=FALSE)
# Plot
plot_Elliptical_Tube(e_tube = tube_A,plot_skeletal_sheet = FALSE,plot_r_project = FALSE,plot_r_max = FALSE,add = FALSE)

# Check tube eligibility
check_Tube_Legality(tube = tube_A)

# Create e-tube B
EulerAngles_alpha_B<--c(rep(0,addedFrames1),rep(pi/100,numberOfFrames),rep(pi/100,addedFrames2),rep(pi/100,numberOfFrames),rep(pi/100,addedFrames2))
EulerAngles_beta_B<--c(rep(0,addedFrames1),rep(0,numberOfFrames),rep(0,addedFrames2),rep(0,numberOfFrames),rep(0,addedFrames2))
EulerAngles_gamma_B<--c(rep(0,addedFrames1),rep(0,numberOfFrames),rep(0,addedFrames2),rep(0,numberOfFrames),rep(0,addedFrames2))
ellipseRadii_a_B<-seq(1,30,length.out=numberOfFrames+addedFrames1+addedFrames2+numberOfFrames+addedFrames2)
ellipseRadii_b_B<-seq(0.9,10,length.out=numberOfFrames+addedFrames1+addedFrames2+numberOfFrames+addedFrames2)
connectionsLengths_B<-rep(1,numberOfFrames+addedFrames1+addedFrames2+numberOfFrames+addedFrames2)

tube_B<-create_Elliptical_Tube(numberOfFrames=length(ellipseRadii_a_A),
                               EulerAngles_Matrix =cbind(EulerAngles_alpha_B,EulerAngles_beta_B,EulerAngles_gamma_B),
                               method="basedOnEulerAngles",
                               ellipseResolution=ellipseResolution,
                               ellipseRadii_a=ellipseRadii_a_B,
                               ellipseRadii_b=ellipseRadii_b_B,
                               connectionsLengths=connectionsLengths_B,
                               plotting=FALSE)

# Plot
plot_Elliptical_Tube(e_tube = tube_B,plot_skeletal_sheet = FALSE,plot_r_project = FALSE,plot_r_max = FALSE,add = FALSE)

# Check tube eligibility
check_Tube_Legality(tube = tube_B)

# Transformations
# Number of steps
numberOfSteps<-10

# Non-intrinsic transformation
nonIntrinsicTubes<-
  nonIntrinsic_Transformation_Elliptical_Tubes(tube1 = tube_A,
                                               tube2 = tube_B,
                                               numberOfSteps = numberOfSteps,
                                               plotting = FALSE)

# Plot transformation
for (i in 1:length(nonIntrinsicTubes)) {
  plot_Elliptical_Tube(e_tube = nonIntrinsicTubes[[i]],plot_skeletal_sheet = FALSE,plot_r_project = FALSE,plot_r_max = FALSE,add = FALSE)
}

# Check tubes eligibility
ElligibleTubes<-c()
for (i in 1:length(nonIntrinsicTubes)) {
  if((check_Tube_Legality(nonIntrinsicTubes[[i]]))){
    ElligibleTubes<-c(ElligibleTubes,i)
  }
}
cat("Elligible tubes of the transformation are :", ElligibleTubes)


# intrinsic transformation
intrinsicTubes<-
  intrinsic_Transformation_Elliptical_Tubes(tube1 = tube_A,
                                            tube2 = tube_B,
                                            numberOfSteps=numberOfSteps,
                                            plotting = FALSE)
# Plot transformation
for (i in 1:length(intrinsicTubes)) {
  plot_Elliptical_Tube(e_tube = intrinsicTubes[[i]],plot_skeletal_sheet = FALSE,plot_r_project = FALSE,plot_r_max = FALSE,add = FALSE)
}

# Check tubes eligibility
ElligibleTubes<-c()
for (i in 1:length(intrinsicTubes)) {
  if((check_Tube_Legality(intrinsicTubes[[i]]))){
    ElligibleTubes<-c(ElligibleTubes,i)
  }
}
cat("Elligible tubes of the transformation are :", ElligibleTubes)

####################################################################
# Calculating intrinsic and non-intrinsic means

# Non-intrinsic mean
nonIntrinsicMeanTube<-nonIntrinsic_mean_tube(tubes = list(tube_A,tube_B),
                                             plotting = FALSE)

# Plot
plot_Elliptical_Tube(e_tube = nonIntrinsicMeanTube,,colorBoundary = "red",plot_r_project = FALSE,plot_r_max = FALSE,add = FALSE)

# Check tube eligibility
check_Tube_Legality(tube = nonIntrinsicMeanTube)

# Intrinsic mean
intrinsicMeanTube<-intrinsic_mean_tube(tubes = list(tube_A,tube_B),
                                       plotting = FALSE)

# Plot
plot_Elliptical_Tube(e_tube = intrinsicMeanTube,colorBoundary = "darkblue",plot_r_project = FALSE,plot_r_max = FALSE,add = FALSE)

# Check tube eligibility
check_Tube_Legality(tube = intrinsicMeanTube)


cat("**** Despite the intrinsic approach, the non-intrinsic mean and transformation are invalid due to local self-intersections! ****")


####################################################################
# Calculating Procrustes mean

boundaryPoints_A_B<-abind(tube_A$boundaryPoints,tube_B$boundaryPoints,along = 3)
proc_A_B<-procGPA(boundaryPoints_A_B,scale = FALSE,reflect = FALSE)

# plot tubes and the Procrustes mean
if(TRUE){
  open3d()
  plotProcTube(boundaryPoints = proc_A_B$rotated[,,1],
               numberOfEllispePoints = dim(tube_A$slicingEllipsoids[,,1])[1],
               colorBoundary = "blue",
               colorSpine = "black")
  open3d()
  plotProcTube(boundaryPoints = proc_A_B$rotated[,,2],
               numberOfEllispePoints = dim(tube_A$slicingEllipsoids[,,2])[1],
               colorBoundary = "blue",
               colorSpine = "black")
  open3d()
  plotProcTube(boundaryPoints = proc_A_B$mshape,
               numberOfEllispePoints = dim(tube_A$slicingEllipsoids[,,1])[1],
               colorBoundary = "darkgreen",
               colorSpine = "black")
}

cat("Obviously, in this example the Porcrustes mean is not an e-tube.")

####################################################################
####################################################################
####################### Simulation Example #########################
####################################################################
####################################################################

#create a reference tube
numberOfFrames<-14
ellipseResolution<-10
EulerAngles_alpha<-c(rep(0,numberOfFrames))
EulerAngles_beta<-c(rep(-pi/20,numberOfFrames))
EulerAngles_gamma<-c(rep(0,numberOfFrames))
ellipseRadii_a<-rep(3,length.out=numberOfFrames)
ellipseRadii_b<-rep(2,length.out=numberOfFrames)
connectionsLengths<-rep(2.5,numberOfFrames)

referenceTube<-create_Elliptical_Tube(numberOfFrames=length(ellipseRadii_a),
                                      EulerAngles_Matrix =cbind(EulerAngles_alpha,EulerAngles_beta,EulerAngles_gamma),
                                      method="basedOnEulerAngles",
                                      ellipseResolution=ellipseResolution,
                                      ellipseRadii_a=ellipseRadii_a,
                                      ellipseRadii_b=ellipseRadii_b,
                                      connectionsLengths=connectionsLengths,
                                      plotting=FALSE)

# Plot 
plot_Elliptical_Tube(referenceTube,
                     plot_spine = TRUE,
                     plot_frames = FALSE,
                     decorate = TRUE,
                     plot_skeletal_sheet = FALSE,
                     plot_r_project = FALSE)



# simulation
numberOfSimulation<-5

# Define variations
sd_v_s<-seq(0.01,0.2,length.out=numberOfSimulation)
sd_psi_s<-seq(0.001,0.5,length.out=numberOfSimulation)
sd_x_s<-sd_a_s<-sd_b_s<-seq(0.001,0.004,length.out=numberOfSimulation)

numberOfFrames<-nrow(referenceTube$spinalPoints3D)
numberOfEllispePoints<-nrow(referenceTube$slicingEllipsoids[,,1])

# Simulate the e-tubes and plot the means
# The intrinsic mean is in red, and the non-intrinsic mean is in black

cat("**** Simulated samples are in blue, while the intrinsic and non-intrinsic means are in red and black, respectively! ****")

set.seed(1234)
for (i in 1:numberOfSimulation) {
  simulatedTubes<-list()
  simulatedTubes<-simulate_etube(referenceTube = referenceTube,
                                 numberOfSimulation=numberOfSimulation,
                                 sd_v=sd_v_s[i],
                                 sd_psi=sd_psi_s[i],
                                 sd_x=sd_x_s[i],
                                 sd_a=sd_a_s[i],
                                 sd_b=sd_b_s[i],
                                 rangeSdScale=c(1,1.00000001),
                                 plotting=FALSE)
  open3d()
  for (i in 1:length(simulatedTubes)) {
    color_spectrum <- colorRampPalette(c("lightblue","darkblue"))
    colors <- color_spectrum(length(simulatedTubes))
    plot_Elliptical_Tube(simulatedTubes[[i]],
                         colorBoundary = colors[i],
                         plot_frames = FALSE,plot_r_project = FALSE,
                         plot_spine = TRUE,plot_normal_vec = FALSE,
                         plot_skeletal_sheet = FALSE,
                         add = TRUE)
  }
  
  # Calculate means
  intrinsicMean<-intrinsic_mean_tube(simulatedTubes,plotting = FALSE)
  nonIntrinsicMean<-nonIntrinsic_mean_tube(simulatedTubes,plotting = FALSE)
  
  plot_Elliptical_Tube(intrinsicMean,
                       colorBoundary = "red",
                       plot_frames = FALSE,plot_r_project = FALSE,
                       plot_spine = TRUE,plot_normal_vec = FALSE,
                       plot_skeletal_sheet = FALSE,
                       add = TRUE)
  
  plot_Elliptical_Tube(nonIntrinsicMean,
                       colorBoundary = "black",
                       plot_frames = FALSE,plot_r_project = FALSE,
                       plot_spine = TRUE,plot_normal_vec = FALSE,
                       plot_skeletal_sheet = FALSE,
                       add = TRUE)
}

####################################################################
####################################################################
##################### Simulation Colon Example #####################
####################################################################
####################################################################

# Load data
load("files/colone3D.Rdata")

referenceTube<-colone3D
numberOfFrames<-nrow(referenceTube$spinalPoints3D)
numberOfEllispePoints<-nrow(referenceTube$slicingEllipsoids[,,1])

# Plot the reference tube
plot_Elliptical_Tube(referenceTube,
                     plot_spine = TRUE,
                     plot_frames = FALSE,
                     decorate = FALSE,
                     plot_skeletal_sheet = FALSE,
                     plot_r_project = FALSE)

# Simulation
numberOfSimulation<-5

# Define standard deviations

# Scenario 1 simulation based on small variation
sd_v=1e-03
sd_psi=1e-03
sd_x=1e-04
sd_a=1e-04
sd_b=1e-04

if(TRUE){
  set.seed(2)
  simulatedTubes<-simulate_etube(referenceTube = referenceTube,
                                 numberOfSimulation=numberOfSimulation,
                                 sd_v=sd_v,
                                 sd_psi=sd_psi,
                                 sd_x=sd_x,
                                 sd_a=sd_a,
                                 sd_b=sd_b,
                                 rangeSdScale=c(1,2),
                                 plotting=FALSE)
  
  boundaryPointsAllTubes<-array(NA,dim = c(numberOfFrames*numberOfEllispePoints,3,length(simulatedTubes)))
  for (j in 1:length(simulatedTubes)) {
    boundaryPointsAllTubes[,,j]<-simulatedTubes[[j]]$boundaryPoints
  }
  
  procAll<-procGPA(boundaryPointsAllTubes,scale = FALSE,reflect = FALSE)
  procAlignedTubes<-procAll$rotated
  
  # plot simulated tubes
  open3d()
  for (j in 1:dim(procAlignedTubes)[3]) {
    color_spectrum <- colorRampPalette(c("grey","darkgrey"))
    colors <- color_spectrum(length(simulatedTubes))
    plotProcTube(boundaryPoints = procAlignedTubes[,,j],
                 numberOfEllispePoints = dim(referenceTube$slicingEllipsoids[,,1])[1],
                 colorBoundary = colors[j],
                 colorSpine = "black")
  }
  #decorate3d()
  
  mean_intrinsic<-intrinsic_mean_tube(tubes = simulatedTubes,
                                      type = "sizeAndShapeAnalysis",
                                      plotting = FALSE)
  
  mean_nonIntrinsic<-nonIntrinsic_mean_tube(tubes = simulatedTubes,
                                            type = "sizeAndShapeAnalysis",
                                            plotting = FALSE)
  
  procMean<-procAll$mshape
  
  #proc alignment for visualization
  boundaryPointsIntrinsicMean<-mean_intrinsic$boundaryPoints
  boundaryPointsNonIntrinsicMean<-mean_nonIntrinsic$boundaryPoints
  
  procOPAMeans1<-procOPA(A = procMean,B = boundaryPointsIntrinsicMean,scale = FALSE,reflect = FALSE)
  intrinsicMeanProcAligned<-procOPAMeans1$Bhat
  
  procOPAMeans2<-procOPA(A = procMean,B = boundaryPointsNonIntrinsicMean,scale = FALSE,reflect = FALSE)
  nonIntrinsicMeanProcAligned<-procOPAMeans2$Bhat
  
  #plot intrinsic mean
  open3d()
  #plot non-intrinsic mean
  plotProcTube(boundaryPoints = intrinsicMeanProcAligned,
               numberOfEllispePoints = dim(referenceTube$slicingEllipsoids[,,1])[1],
               colorBoundary = "darkblue",
               colorSpine = "black")
  #plot non-intrinsic mean
  plotProcTube(boundaryPoints = nonIntrinsicMeanProcAligned,
               numberOfEllispePoints = dim(referenceTube$slicingEllipsoids[,,1])[1],
               colorBoundary = "red",
               colorSpine = "black")
  #plot proc mean 
  plotProcTube(boundaryPoints = procMean,
               numberOfEllispePoints = dim(referenceTube$slicingEllipsoids[,,1])[1],
               colorBoundary = "darkgreen",
               colorSpine = "black")
  
}
  


# Scenario 2 simualtion based on significant variation
sd_v=1e-01
sd_psi=1e-01
sd_x=1e-04
sd_a=1e-04
sd_b=1e-04


if(TRUE){
  set.seed(2)
  simulatedTubes<-simulate_etube(referenceTube = referenceTube,
                                 numberOfSimulation=numberOfSimulation,
                                 sd_v=sd_v,
                                 sd_psi=sd_psi,
                                 sd_x=sd_x,
                                 sd_a=sd_a,
                                 sd_b=sd_b,
                                 rangeSdScale=c(1,2),
                                 plotting=FALSE)
  
  boundaryPointsAllTubes<-array(NA,dim = c(numberOfFrames*numberOfEllispePoints,3,length(simulatedTubes)))
  for (j in 1:length(simulatedTubes)) {
    boundaryPointsAllTubes[,,j]<-simulatedTubes[[j]]$boundaryPoints
  }
  
  procAll<-procGPA(boundaryPointsAllTubes,scale = FALSE,reflect = FALSE)
  procAlignedTubes<-procAll$rotated
  
  # plot simulated tubes
  open3d()
  for (j in 1:dim(procAlignedTubes)[3]) {
    color_spectrum <- colorRampPalette(c("grey","darkgrey"))
    colors <- color_spectrum(length(simulatedTubes))
    plotProcTube(boundaryPoints = procAlignedTubes[,,j],
                 numberOfEllispePoints = dim(referenceTube$slicingEllipsoids[,,1])[1],
                 colorBoundary = colors[j],
                 colorSpine = "black")
  }
  #decorate3d()
  
  mean_intrinsic<-intrinsic_mean_tube(tubes = simulatedTubes,
                                      type = "sizeAndShapeAnalysis",
                                      plotting = FALSE)
  
  mean_nonIntrinsic<-nonIntrinsic_mean_tube(tubes = simulatedTubes,
                                            type = "sizeAndShapeAnalysis",
                                            plotting = FALSE)
  
  procMean<-procAll$mshape
  
  #proc alignment for visualization
  boundaryPointsIntrinsicMean<-mean_intrinsic$boundaryPoints
  boundaryPointsNonIntrinsicMean<-mean_nonIntrinsic$boundaryPoints
  
  procOPAMeans1<-procOPA(A = procMean,B = boundaryPointsIntrinsicMean,scale = FALSE,reflect = FALSE)
  intrinsicMeanProcAligned<-procOPAMeans1$Bhat
  
  procOPAMeans2<-procOPA(A = procMean,B = boundaryPointsNonIntrinsicMean,scale = FALSE,reflect = FALSE)
  nonIntrinsicMeanProcAligned<-procOPAMeans2$Bhat
  
  #plot intrinsic mean
  open3d()
  #plot non-intrinsic mean
  plotProcTube(boundaryPoints = intrinsicMeanProcAligned,
               numberOfEllispePoints = dim(referenceTube$slicingEllipsoids[,,1])[1],
               colorBoundary = "darkblue",
               colorSpine = "black")
  #plot non-intrinsic mean
  plotProcTube(boundaryPoints = nonIntrinsicMeanProcAligned,
               numberOfEllispePoints = dim(referenceTube$slicingEllipsoids[,,1])[1],
               colorBoundary = "red",
               colorSpine = "black")
  #plot proc mean 
  plotProcTube(boundaryPoints = procMean,
               numberOfEllispePoints = dim(referenceTube$slicingEllipsoids[,,1])[1],
               colorBoundary = "darkgreen",
               colorSpine = "black")
  
}


