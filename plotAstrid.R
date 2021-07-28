#!/usr/bin/Rscript

###################
### PLOT ASTRID ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###################

### This script is the base for plotting various forms of the astrid geometric thing.

### The main function requires an x-axis and a y-axis of either the same length, or a whole-number ratio of lengths
  ### If one number is given, will be same for both, if two, then will do ratio
### Can have from 1:n of these axis pairs
### The various pairs must exist on the regular coordinate plane
  ### Each axis is defined as a line between the origin and some given point.
  ### Needs to be fully customizable and also automatic.
  ### Default will be equidistant angles with positive y-axis as main reference
    ### Two axes - quadrant I - maybe just omit this since it's not regular
    ### Three axes - y-axis from origin to length, then other two axes in quadrants 3 and 4 b/c 120* between them
    ### Four axes - full x and y axis, b/c 90* between them
    ### Five : 72; Six : 60; Seven : 51.428; Eight : 45
    ### Populated by:
      ### Everything is referenced from positive y-axis going counter-clockwise
        ### Quadrants are one less than the "standard", so the "top-left" quadrant (i.e. -x,y) is I (instead of the usual II)
        ### To determine the (x,y) coordinate of the point that will make the line extending from the origin that will make the
        ### "new" axis limit, take the sine and cosine of the appropriate angle. The sign of the x and y are determined by quadrant.
        ### I: x = -sin(theta); y = cos(theta)
        ### II: x = -cos(theta); y = -sin(theta)
        ### III: x = sin(theta); y = -cos(theta)
        ### IV: x = cos(theta); y = sin(theta)
        ### Quadrant is determined by how many times you must subtract 90 from your starting angle to get an angle less than 90
        ### Quadrant one is comprised of angles <90, so you never subtract
        ### II is comprised of 91-180, so you have to subtract 90 once
        ### must subtract twice for III and three times for IV

##############################
### DEPENDENCIES/FUNCTIONS ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####################

suppressMessages(library(ggplot2))
suppressMessages(library(data.table))
suppressMessages(library(wrh.rUtils))

pointFinder <- function(fullAngle_v,trigAngle_v, segmentLength_v) {
  #' Point Finder
  #' @description Find points for line segments of axes that make up astrid.
  #' @param fullAngle_v The full angle of the current line segment relative to the positive y-axis and going counter-clockwise.
  #' @param trigAngle_v The angle used to create a 90* triangle in order to use sin/cos to find points. Basically subtract 90 from
  #' the fullAngle until the remainder is less than 90.
  #' @param segmentLength_v The distance of the point from the origin, i.e. the length of the line segment. Used to find the x and y.
  #' @return Returns vector of format c(x,y).
  
  ## Convert trigAngle_v from degrees to radians
  trigAngle_v <- trigAngle_v * pi / 180
  
  if (fullAngle_v <= 90) {
    point_v <- c(-sin(trigAngle_v), cos(trigAngle_v))
  } else if ((90 < fullAngle_v) & (fullAngle_v <= 180)) {
    point_v <- c(-cos(trigAngle_v), -sin(trigAngle_v))
  } else if ((180 < fullAngle_v) & (fullAngle_v <= 270)) {
    point_v <- c(sin(trigAngle_v), -cos(trigAngle_v))
  } else {
    point_v <- c(cos(trigAngle_v), sin(trigAngle_v))
  }
  
  point_v <- point_v * segmentLength_v
  
  return(point_v)
} # pointFinder

getWebPoints <- function(lengthAxes_v, density_v, fullAngles_v, trigAngles_v) {
  
  ### Use the density and axis length to get the "step" of the web and the sequence of steps
  ### along the axis
  step_v <- lengthAxes_v / density_v
  sequences_lsv <- mapply(function(x,y) seq(x, y, x), step_v, lengthAxes_v, SIMPLIFY = F)
  
  ### Remove first list element (always the positive y-axis) 
  firstPoints_lsv <- Map(c, rep(0,length(sequences_lsv[[1]])), sequences_lsv[[1]])
  sequences_lsv[[1]] <- NULL
  
  ### Use pointFinder to get points for the other axes
  webPoints_lslsv <- lapply(1:length(trigAngles_v), function(x) {
    points_lsv <- sapply(sequences_lsv[[x]], function(y){
      pointFinder(fullAngles_v[x], trigAngles_v[x], y)
    }, simplify = F)
  })
  
  ### Add first points back
  ### Do I need to do this? Can i just plug it in with the angle of 0?
  webPoints_lslsv <- rlist::list.prepend(webPoints_lslsv, firstPoints_lsv)
  
} # getWebPoints

plotWeb <- function(webPoints_lslsv, save_v = F, outDir_v = "./", outName_v = "astrid.pdf") {
  
  ### Open device
  if (save_v) {
    pdf(file.path(outDir_v, outName_v))
  }
  
  ### Now for each set of points, use the subsequent set to make line segments. For the last set, use the first set.
  for (i in 1:length(webPoints_lslsv)) {
    
    ### Get first set
    currFirstPoints_lsv <- webPoints_lslsv[[i]]
    
    ### Get second set
    if (i == length(webPoints_lslsv)) {
      currSecondPoints_lsv <- webPoints_lslsv[[1]]
    } else {
      currSecondPoints_lsv <- webPoints_lslsv[[i+1]]
    }
    
    ### Reverse second set
    currSecondPoints_lsv <- rev(currSecondPoints_lsv)
    
    ### Plot
    if (save_v) {
      pdf(file.path(outDir_v, outName_v))
    }
    for (j in 1:length(currFirstPoints_lsv)) {
      points(x = c(currFirstPoints_lsv[[j]][1], currSecondPoints_lsv[[j]][1]),
             y = c(currFirstPoints_lsv[[j]][2], currSecondPoints_lsv[[j]][2]),
             type = 'l')
    } # for j
  } # for i
  
  ### Close device
  if (save_v) dev.off()
} # plotWeb


#################
### ARGUMENTS ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################

### numAxes - numerical vector of how many axes and correspondigly "webs" you want, with 3 being the minimum.
### length - the length of the line that each axis is relative to a standard scale
### density - the number of lines in the web, default is to be same as length

# ### Sparse 5-point
# numAxes_v <- 5
# lengthAxes_v <- rep(6,5)
# density_v <- 6
# 
# ### Dense 5-point
# numAxes_v <- 5
# lengthAxes_v <- rep(6,5)
# density_v <- 12

### Dense 4-point
numAxes_v <- 4
lengthAxes_v <- rep(10, 4)
density_v <- 20

# ### Dense 8-point
# numAxes_v <- 8
# lengthAxes_v <- rep(10,8)
# density_v <- 20

plotAstrid <- function(numAxes_v = 4, lengthAxes_v = rep(10,4), density_v = 20) {
  
  ###
  ### Get main axis information
  ###
  
  ### Get coordinates for each of the axes
  baseAngle_v <- 360 / numAxes_v
  
  ### Get the full angles
  fullAngles_v <- sapply(1:(numAxes_v-1), function(x) baseAngle_v * x)
  
  ### Get the trig angles
  trigAngles_v <- sapply(fullAngles_v, function(x) {
    while(x > 90) x <- x - 90
    return(x)
  })
  
  ### Get the points for the axis segments
  axisPoints_lsv <- mapply(pointFinder, fullAngles_v, trigAngles_v, lengthAxes_v[2:numAxes_v], SIMPLIFY = F)
  
  ### Prepend the first axis (don't do the trig on it b/c it's always 1:1 ratio on positive y-axis)
  axisPoints_lsv <- rlist::list.prepend(axisPoints_lsv, c(0,lengthAxes_v[1]))
  
  ### Get web points
  webPoints_lslsv <- getWebPoints(lengthAxes_v, density_v, fullAngles_v, trigAngles_v)
  
  ### Plot axes
  plot(1, type = "n", xlim=c(-10,10), ylim = c(-10,10))
  for (i in 1:length(axisPoints_lsv)) {
    points(x = c(0, axisPoints_lsv[[i]][1]), y = c(0,axisPoints_lsv[[i]][2]), type = 'l')
  }
  
  ### Plot webs
  plotWeb(webPoints_lslsv)
  
} # plotAstrid

plotAstrid()

#################
### MAKE AXES ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################

# ### Get coordinates for each of the axes
# baseAngle_v <- 360 / numAxes_v
# 
# ### Get the full angles
# fullAngles_v <- sapply(1:(numAxes_v-1), function(x) baseAngle_v * x)
# 
# ### Get the trig angles
# trigAngles_v <- sapply(fullAngles_v, function(x) {
#   while(x > 90) x <- x - 90
#   return(x)
# })
# 
# ### Get the points for the axis segments
# axisPoints_lsv <- mapply(pointFinder, fullAngles_v, trigAngles_v, lengthAxes_v[2:numAxes_v], SIMPLIFY = F)
# 
# ### Prepend the first axis
# axisPoints_lsv <- rlist::list.prepend(axisPoints_lsv, c(0,lengthAxes_v[1]))
# 
# ### Test plot
# plot(1, type = "n", xlim=c(-10,10), ylim = c(-10,10))
# for (i in 1:length(axisPoints_lsv)) {
#   points(x = c(0, axisPoints_lsv[[i]][1]), y = c(0,axisPoints_lsv[[i]][2]), type = 'l')
# }

#################
### MAKE WEBS ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################

# ### Have to draw one web for each axis
# ### As many lines as there is density.
# ### Start at the positive y axis and then go counter-clockwise.
#   ### Point 1 of axis1 goes to pointAxisLength on axis2
#   ### Point 2 of axis1 goes to (point (AxisLength-1)) on axis2
#   ### etc.
# ### How to find the points? Same trig thing, but instead of the length being the axis length, it will be a division of it.
# ### These are the things that need to be lined up in inverse of each other for the web lines.
# ### Will be dependent on the density.
# 
# ### In this example, there will be 5 sets of 12 lines
# 
# ### For the first one, each point will be c(0,len/dens), c(0,(len/dens)-1) etc.
# ### Second point will be trigValue*len, trigValue*len-1, etc.
# ### Get web points
# webPoints_lslsv <- getWebPoints(lengthAxes_v, density_v, fullAngles_v, trigAngles_v)
# ### Plot them
# plotWeb(webPoints_lslsv)





