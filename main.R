library(shapes)
library(rgl)
require(Morpho)
library(matlib)
library(RiemBase)
library(doBy) #for which.maxn and which.minn
library(alphahull) #for alpha convex hull
library(alphashape3d) #for alpha 3d
library(plotrix)
library(Directional)
library(RSpincalc)
library(rotations)
library(SphericalCubature)
library(Rvcg) #for triangle mesh 
library(ptinpoly) #to check whether a point is inside a 2D or 3D mesh or no
library(fields) #To plot color bar
library(Matrix) #for forceSymmetric function
library(fdasrvf) #(elastic metric) contains 2D images
library(pracma) #for cross product
library(truncnorm)
library(rgl)
require(Morpho)
library(matlib)
library(RiemBase)
library(Directional)
library(doBy) #for which.maxn and which.minn
library(alphahull) #for alpha convex hull
library(alphashape3d) #for alpha 3d
library(plotrix)
library(Rvcg) #for triangle mesh 
library(ptinpoly) #to check whether a point is inside a 2D or 3D mesh or no
library(fields) #To plot color bar
library(Matrix) #for forceSymmetric function
library(fdasrvf) #(elastic metric) contains 2D images
library(rotations)
library(princurve)

#####################################################################################################
#####################################################################################################

#clear the environment
remove(list=ls())

#####################################################################################################
#####################################################################################################
#set working directory to file location

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#####################################################################################################
#####################################################################################################
# functions 

if(TRUE){
  source("subFunctions/MathFunctions.R")
  source("subFunctions/rotateFrameForwardAndBackward.R")
  source("subFunctions/functionsMeanSweptSkeletal_1.R")
  source("subFunctions/functionsMeanSweptSkeletal_2.R")
}


#####################################################################################################
#####################################################################################################
# load simulated E-Tubes as ETReps

load("e_Tubes_simulated.Rdata")


#####################################################################################################
#####################################################################################################
# plot the simulated data in blue

if(TRUE){
  for (i in 1:length(e_Tubes_simulated)) {
    plot_Elliptical_Tube(e_Tubes_simulated[[i]],
                         plot_frames = FALSE,
                         plot_skeletal_sheet = FALSE,
                         plot_r_project = FALSE,
                         plot_r_max = FALSE,
                         decorate = FALSE,
                         add = TRUE)
    
  }
  decorate3d() 
}

#####################################################################################################
#####################################################################################################
# calculating the intrinsic mean shape of the simulated E-Tubes

intrinsic_mean<-
  mean_tube_basedOnIntrinsicSweptCoordinate(tubes = e_Tubes_simulated,
                                            taloranceTwist=pi/1000,
                                            flippingTolorance=pi/2,
                                            plotting=FALSE)


#####################################################################################################
#####################################################################################################
# plot the intrinsic mean shape in red

plot_Elliptical_Tube(intrinsic_mean,
                     plot_r_project = FALSE,
                     plot_r_max = FALSE,
                     colorBoundary = "red",
                     plot_skeletal_sheet = TRUE,
                     colSkeletalSheet = "red",
                     plot_frames = FALSE,
                     add = TRUE)


