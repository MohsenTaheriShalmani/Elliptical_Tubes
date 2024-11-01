% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ETRep_Functions.R
\name{nonIntrinsic_Transformation_Elliptical_Tubes}
\alias{nonIntrinsic_Transformation_Elliptical_Tubes}
\title{Non-intrinsic Transformation Between Two ETReps}
\usage{
nonIntrinsic_Transformation_Elliptical_Tubes(
  tube1,
  tube2,
  type = "sizeAndShapeAnalysis",
  numberOfSteps = 4,
  plotting = TRUE,
  colorBoundary = "blue",
  add = FALSE
)
}
\arguments{
\item{tube1}{List containing details of the first ETRep.}

\item{tube2}{List containing details of the second ETRep.}

\item{type}{String defining the type of analysis as sizeAndShapeAnalysis or shapeAnalysis}

\item{numberOfSteps}{Integer, number of transformation steps.}

\item{plotting}{Logical, enables visualization during transformation (default is TRUE).}

\item{colorBoundary}{String defining the color of the e-tube}

\item{add}{Logical, enables overlay plotting}
}
\value{
List containing intermediate ETReps.
}
\description{
Performs the non-intrinsic transformation from one ETRep to another.
}
\examples{
\donttest{
# Load tubes
data("tube_A")
data("tube_B")
numberOfSteps <- 10
transformation_Tubes<-
  nonIntrinsic_Transformation_Elliptical_Tubes(
    tube1 = tube_A,tube2 = tube_B,
    numberOfSteps = numberOfSteps,
    plotting = FALSE)
# Plotting
for (i in 1:length(transformation_Tubes)) {
  plot_Elliptical_Tube(tube = transformation_Tubes[[i]],
  plot_frames = FALSE,plot_skeletal_sheet = FALSE
  ,plot_r_project = FALSE,
  plot_r_max = FALSE,
  add = FALSE)
}
}
}
