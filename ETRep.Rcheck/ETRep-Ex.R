pkgname <- "ETRep"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "ETRep-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('ETRep')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("check_Tube_Legality")
### * check_Tube_Legality

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_Tube_Legality
### Title: Check the Legality of an Elliptical Tube (ETRep)
### Aliases: check_Tube_Legality

### ** Examples

# Load tube
data("colon3D")
check_Tube_Legality(tube = colon3D)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_Tube_Legality", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_Elliptical_Tube")
### * create_Elliptical_Tube

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_Elliptical_Tube
### Title: Create a Discrete Elliptical Tube (ETRep)
### Aliases: create_Elliptical_Tube

### ** Examples

numberOfFrames<-15
EulerAngles_alpha<-c(rep(0,numberOfFrames))
EulerAngles_beta<-c(rep(-pi/20,numberOfFrames))
EulerAngles_gamma<-c(rep(0,numberOfFrames))
EulerAngles_Matrix<-cbind(EulerAngles_alpha,
                          EulerAngles_beta,
                          EulerAngles_gamma)
tube <- create_Elliptical_Tube(numberOfFrames = numberOfFrames,
                               method = "basedOnEulerAngles",
                               EulerAngles_Matrix = EulerAngles_Matrix,
                               ellipseResolution = 10,
                               ellipseRadii_a = rep(3, numberOfFrames),
                               ellipseRadii_b = rep(2, numberOfFrames),
                               connectionsLengths = rep(4, numberOfFrames),
                               plotting = FALSE)
# Plotting
## Not run: 
##D  plot_Elliptical_Tube(tube = tube,plot_frames = FALSE,
##D                       plot_skeletal_sheet = TRUE,
##D                       plot_r_project = FALSE,
##D                       plot_r_max = FALSE,add = FALSE)
##D  
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_Elliptical_Tube", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("elliptical_Tube_Euclideanization")
### * elliptical_Tube_Euclideanization

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: elliptical_Tube_Euclideanization
### Title: Convert an ETRep to a Matrix in the Convex Transformed Space.
### Aliases: elliptical_Tube_Euclideanization

### ** Examples

#Example
# Load tube
data("tube_A")
Euclideanized_Tube<- elliptical_Tube_Euclideanization(tube = tube_A)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("elliptical_Tube_Euclideanization", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("intrinsic_Distance_Between2tubes")
### * intrinsic_Distance_Between2tubes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: intrinsic_Distance_Between2tubes
### Title: Calculating the intrinsic distance between two ETReps
### Aliases: intrinsic_Distance_Between2tubes

### ** Examples

# Load tubes
data("tube_A")
data("tube_B")
intrinsic_Distance_Between2tubes(tube1 = tube_A,tube2 = tube_B)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("intrinsic_Distance_Between2tubes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("intrinsic_Transformation_Elliptical_Tubes")
### * intrinsic_Transformation_Elliptical_Tubes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: intrinsic_Transformation_Elliptical_Tubes
### Title: Intrinsic Transformation Between Two ETReps
### Aliases: intrinsic_Transformation_Elliptical_Tubes

### ** Examples

## No test: 
# Load tubes
data("tube_A")
data("tube_B")
numberOfSteps <- 10
transformation_Tubes<-
  intrinsic_Transformation_Elliptical_Tubes(
    tube1 = tube_A,tube2 = tube_B,
    numberOfSteps = numberOfSteps,
    plotting = FALSE)
# Plotting
## Not run: 
##D for (i in 1:length(transformation_Tubes)) {
##D   plot_Elliptical_Tube(tube = transformation_Tubes[[i]],
##D   plot_frames = FALSE,plot_skeletal_sheet = FALSE
##D   ,plot_r_project = FALSE,
##D   plot_r_max = FALSE,
##D   add = FALSE)
##D }
##D ##
## End(Not run)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("intrinsic_Transformation_Elliptical_Tubes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("intrinsic_mean_tube")
### * intrinsic_mean_tube

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: intrinsic_mean_tube
### Title: Calculate Intrinsic Mean of ETReps
### Aliases: intrinsic_mean_tube

### ** Examples

#Example 1
# Load tubes
data("tube_A")
data("tube_B")
intrinsic_mean<-
  intrinsic_mean_tube(tubes = list(tube_A,tube_B),
                      plotting = FALSE)
# Plotting
## Not run: 
##D plot_Elliptical_Tube(tube = intrinsic_mean,
##D                      plot_frames = FALSE,
##D                      plot_skeletal_sheet = FALSE,
##D                      plot_r_project = FALSE,
##D                      plot_r_max = FALSE,
##D                      add = FALSE)
##D  
## End(Not run)

#Example 2
data("simulatedColons")
intrinsic_mean<-
  intrinsic_mean_tube(tubes = simulatedColons,
                      plotting = FALSE)
# Plotting
## Not run: 
##D plot_Elliptical_Tube(tube = intrinsic_mean,
##D                      plot_frames = FALSE,
##D                      plot_skeletal_sheet = FALSE,
##D                      plot_r_project = FALSE,
##D                      plot_r_max = FALSE,
##D                      add = FALSE)
##D  
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("intrinsic_mean_tube", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("nonIntrinsic_Distance_Between2tubes")
### * nonIntrinsic_Distance_Between2tubes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: nonIntrinsic_Distance_Between2tubes
### Title: Calculating the non-intrinsic distance between two ETReps
### Aliases: nonIntrinsic_Distance_Between2tubes

### ** Examples

# Load tubes
data("tube_A")
data("tube_B")
intrinsic_Distance_Between2tubes(tube1 = tube_A,tube2 = tube_B)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("nonIntrinsic_Distance_Between2tubes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("nonIntrinsic_Transformation_Elliptical_Tubes")
### * nonIntrinsic_Transformation_Elliptical_Tubes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: nonIntrinsic_Transformation_Elliptical_Tubes
### Title: Non-Intrinsic Transformation Between Two ETReps
### Aliases: nonIntrinsic_Transformation_Elliptical_Tubes

### ** Examples

## No test: 
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
## Not run: 
##D for (i in 1:length(transformation_Tubes)) {
##D   plot_Elliptical_Tube(tube = transformation_Tubes[[i]],
##D   plot_frames = FALSE,plot_skeletal_sheet = FALSE
##D   ,plot_r_project = FALSE,
##D   plot_r_max = FALSE,
##D   add = FALSE)
##D }
##D  
## End(Not run)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("nonIntrinsic_Transformation_Elliptical_Tubes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("nonIntrinsic_mean_tube")
### * nonIntrinsic_mean_tube

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: nonIntrinsic_mean_tube
### Title: Compute Non-Intrinsic Mean of ETReps
### Aliases: nonIntrinsic_mean_tube

### ** Examples

#Example 1
# Load tubes
data("tube_A")
data("tube_B")
nonIntrinsic_mean<-
  nonIntrinsic_mean_tube(tubes = list(tube_A,tube_B),
                         plotting = FALSE)
# Plotting
## Not run: 
##D plot_Elliptical_Tube(tube = nonIntrinsic_mean,
##D                      plot_frames = FALSE,
##D                      plot_skeletal_sheet = FALSE,
##D                      plot_r_project = FALSE,
##D                      plot_r_max = FALSE,
##D                      add = FALSE)
##D  
## End(Not run)

#Example 2
data("simulatedColons")
nonIntrinsic_mean<-
  nonIntrinsic_mean_tube(tubes = simulatedColons,
                         plotting = FALSE)
# Plotting
## Not run: 
##D plot_Elliptical_Tube(tube = nonIntrinsic_mean,
##D                      plot_frames = FALSE,
##D                      plot_skeletal_sheet = FALSE,
##D                      plot_r_project = FALSE,
##D                      plot_r_max = FALSE,
##D                      add = FALSE)
##D  
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("nonIntrinsic_mean_tube", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_Elliptical_Tube")
### * plot_Elliptical_Tube

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_Elliptical_Tube
### Title: Plot an Elliptical Tube (ETRep)
### Aliases: plot_Elliptical_Tube

### ** Examples

# Load tube
data("colon3D")
## Not run: 
##D plot_Elliptical_Tube(tube = colon3D,
##D                      plot_frames = FALSE)
##D  
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_Elliptical_Tube", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("simulate_etube")
### * simulate_etube

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: simulate_etube
### Title: Simulate Random Elliptical Tubes (ETReps)
### Aliases: simulate_etube

### ** Examples

## No test: 
# Load tube
data("colon3D")
#Set Parameters
sd_v<-sd_psi<-1e-03
sd_x<-sd_a<-sd_b<-1e-04
numberOfSimulation<-4
random_Tubes<-
  simulate_etube(referenceTube = colon3D,
                 numberOfSimulation = numberOfSimulation,
                 sd_v = sd_v,
                 sd_psi = sd_psi,
                 sd_x = sd_x,
                 sd_a = sd_a,
                 sd_b = sd_b,
                 rangeSdScale = c(1, 2),
                 plotting = FALSE)
# Plotting
## Not run: 
##D plot_Elliptical_Tube(random_Tubes[[1]], add = FALSE)
##D plot_Elliptical_Tube(random_Tubes[[2]], add = TRUE)
##D plot_Elliptical_Tube(random_Tubes[[3]], add = TRUE)
##D plot_Elliptical_Tube(random_Tubes[[4]], add = TRUE)
##D  
## End(Not run)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("simulate_etube", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tube_Surface_Mesh")
### * tube_Surface_Mesh

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tube_Surface_Mesh
### Title: Create surface mesh of a tube
### Aliases: tube_Surface_Mesh

### ** Examples

## Not run: 
##D quad_mesh<-tube_Surface_Mesh(tube = ETRep::tube_B, 
##D                              meshType = "quadrilateral", 
##D                              plotMesh = TRUE, 
##D                              decorate = TRUE, 
##D                              color = "orange")
##D # draw wireframe of the mesh
##D rgl::wire3d(quad_mesh, color = "black", lwd = 1)   # add wireframe
##D # Display in browser
##D ETRep:::.etrep_show3d(width = 800, height = 600)
##D 
##D tri_mesh<-tube_Surface_Mesh(tube = ETRep::tube_B, 
##D                             meshType = "triangular", 
##D                             plotMesh = TRUE, 
##D                             decorate = TRUE, 
##D                             color = "green")
##D # draw wireframe of the mesh
##D rgl::wire3d(tri_mesh, color = "black", lwd = 1)   # add wireframe
##D # Display in browser
##D ETRep:::.etrep_show3d(width = 800, height = 600)
##D  
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tube_Surface_Mesh", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
