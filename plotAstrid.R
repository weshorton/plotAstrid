#!/usr/bin/Rscript

###################
### PLOT ASTRID ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###################

### This script contains various functions used for plotting different permutations of the
### astrid geometric shape.

### Using a regular coordinate plane. Plot new "axes" based on regular geometry and then plot
### "webs" of lines between these new axes based upon a given density.

### Positive y-axis is always the first/reference axis.


### TO DO ###

### 1. NEED TO ADD SAVE/FILE ARGUMENTS TO PLOTASTRID
### 2. REMOVE DATA MANIPULATION FROM PLOTWEB AND PUT INTO GETWEBPOINTS
### 3. ORGANIZE PLOTTING OF AXES AND WEBS TOGETHER IN PLOTWEB (no plotting in plotAstrid except the call to plotWeb)
### 4. ADD MORE PLOTTING FUNCTIONALITY
### 5. OUTPUT POINTS TO OBJECT FOR LATER PLOTTING

#################
### FUNCTIONS ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################

pointFinder <- function(fullAngle_v,trigAngle_v, segmentLength_v) {
  #' Point Finder
  #' @description Find points for line segments of axes that make up astrid.
  #' @param fullAngle_v The full angle of the current line segment relative to the positive y-axis and going counter-clockwise.
  #' @param trigAngle_v The angle used to create a 90* triangle in order to use sin/cos to find points. Basically subtract 90 from
  #' the fullAngle until the remainder is less than 90.
  #' @param segmentLength_v The distance of the point from the origin, i.e. the length of the line segment. Used to find the x and y.
  #' @return Returns vector of format c(x,y).
  #' @export
  
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

getWebPoints <- function(lengthAxes_v, density_v = NULL, fullAngles_v, trigAngles_v) {
  #' Get Web Points
  #' @description Get all of the points for all of the line segments that will make the web
  #' @param lengthAxes_v Numeric vector whose values are the length of each axis.
  #' @param density_v Numeric vector representing the number of lines that will be in the web. Default
  #' is to be the same as the length of the axis.
  #' @param fullAngles_v Numeric vector containing each axis' angle relative to the positive y-axis
  #' @param trigAngles_v Numeric vector containing each axis' "trigonometric angle", found by subtracting
  #' 90* until acute angle is found, this angle is used with sine/cosine properties to get x,y coordinates
  #' @return list of lists. Each main list element corresponds to one of the main axes. Its child is a list
  #' of x,y coordinates for the points along that line that the web will intersect.
  #' @export
  
  ### Use the density and axis length to get the "step" of the web 
  ### and the sequence of steps along the axis
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
  #' Plot Web
  #' @description Use web points to add line segments to existing plot.
  #' This should not be a function. Need to take the first-second point construction and add it to
  #' getWebPoints I think so that I can save it as an object. All the plotting should be done together.
  #' @param webPoints_lslsv list with one element for each axis that contains a list of x,y coordinates for the web.
  #' @param save_v logical. TRUE - save using outDir_v and outName_v; FALSE - print to console
  #' @param outDir_v directory to save file. Default to current directory
  #' @param outName_v filename. Default to astrid.pdf
  #' @return plot
  #' @export
  
  ### Open device
  if (save_v) {
    pdf(file.path(outDir_v, outName_v))
  }
  
  ### For each set of points, use the subsequent set to make line segments. 
  ### For the last set, use the first set.
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

plotAstrid <- function(numAxes_v = 4, lengthAxes_v = rep(10,4), density_v = 20) {
  #' Plot Astrid
  #' @description Create various permutations on the astrid geometric shape
  #' @param numAxes_v Numeric value indicating the number of axes to plot webs between. 3 is minimum.
  #' @param lengthAxes_v Numeric vector indicating the length of each axis. length(lengthAxes_v) = numAxes_v
  #' @param density_v Numeric vector indicating the number of lines in each web
  #' @return astrid data and also a plot.
  
  ### Get the full and trig angles of each axis
  baseAngle_v <- 360 / numAxes_v
  fullAngles_v <- sapply(1:(numAxes_v-1), function(x) baseAngle_v * x)
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

################
### EXAMPLES ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
################

### Dense 4-point
plotAstrid()

### Dense 8-point
plotAstrid(numAxes_v = 8, lengthAxes_v = rep(10,8), density_v = 20)

### Sparse 8-point
plotAstrid(numAxes_v = 8, lengthAxes_v = rep(10,8), density_v = 10)

### Dense 5-point
plotAstrid(numAxes_v = 5, lengthAxes_v = rep(10,5), density_v = 20)





