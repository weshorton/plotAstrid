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
### 4. ADD MORE PLOTTING FUNCTIONALITY

#################
### FUNCTIONS ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################

### Arguments for walkthrough
numAxes_v <- 5
lengthAxes_v <- rep(10,5)
density_v <- 20
save_v <- F
outDir_v <- "./"
outName_v <- "astrid.pdf"

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
  if (is.null(density_v)) density_v <- lengthAxes_v
  step_v <- lengthAxes_v / density_v
  sequences_lsv <- mapply(function(x,y) seq(x, y, x), step_v, lengthAxes_v, SIMPLIFY = F)
  
  ### Use pointFinder to get points for the other axes
  webPoints_lslsv <- lapply(1:length(trigAngles_v), function(x) {
    points_lsv <- sapply(sequences_lsv[[x]], function(y){
      pointFinder(fullAngles_v[x], trigAngles_v[x], y)
    }, simplify = F)
  })
  
  ### For each set of points, use the subsequent set to make line segments. 
  ### For the last set, use the first set.
  webPoints_lsdt <- list()
  for (i in 1:length(webPoints_lslsv)) {
    
    ### Get first set
    currFirstPoints_dt <- as.data.table(t(as.data.table(webPoints_lslsv[[i]])))
    colnames(currFirstPoints_dt) <- c("x", "y")
    
    ### Get second set
    if (i == length(webPoints_lslsv)) {
      currSecondPoints_dt <- as.data.table(t(as.data.table(webPoints_lslsv[[1]])))
    } else {
      currSecondPoints_dt <- as.data.table(t(as.data.table(webPoints_lslsv[[i+1]])))
    }
    
    ### Reverse second set
    currSecondPoints_dt <- currSecondPoints_dt[currSecondPoints_dt[,.N]:1,]
    colnames(currSecondPoints_dt) <- c("x","y")
    
    ### Combine into one data.table.
    currLineSegment_dt <- cbind(currFirstPoints_dt,currSecondPoints_dt)
    colnames(currLineSegment_dt) <- c("x1", "y1", "x2", "y2")
    
    ### Add to output
    webPoints_lsdt[[i]] <- currLineSegment_dt

  } # for i
  
  ### Return
  return(webPoints_lsdt)
  
} # getWebPoints

plotAstrid <- function(axisPoints_lsv,
                    webPoints_lsdt) {
  #' Plot Astrid
  #' @description Plot axes and webs of astrid.
  #' @param axisPoints_lsv list of (x,y) vectors that combine with origin to create line segments for axes of astrid
  #' @param webPoints_lsdt list with one element for each axis that contains a dt of x,y coordinates for the web.
  #' @return plot
  #' @export
  
  ### Base plot and axes
  plot(1, type = "n", xlim=c(-10,10), ylim = c(-10,10))
  for (i in 1:length(axisPoints_lsv)) {
    points(x = c(0, axisPoints_lsv[[i]][1]), y = c(0,axisPoints_lsv[[i]][2]), type = 'l')
  } # for i

  ### Iterate over list of webs
  for (i in 1:length(webPoints_lsdt)) {
    ### Get x,y coordinates for current web
    currLineSegment_dt <- webPoints_lsdt[[i]]
    ### Iterate over rows of x,y coords
    for (j in 1:currLineSegment_dt[,.N]) {
      ### Add to plot.
      points(x = c(currLineSegment_dt$x1[j], currLineSegment_dt$x2[j]),
             y = c(currLineSegment_dt$y1[j], currLineSegment_dt$y2[j]),
             type = "l")
    }  # for j
  } # for i
} # plotAstrid

astrid <- function(numAxes_v = 4, lengthAxes_v = rep(10,4), density_v = 20, save_v = F,
                       outDir_v = "./", outName_v = "astrid.pdf") {
  #' Astrid
  #' @description Create various permutations on the astrid geometric shape
  #' @param numAxes_v Numeric value indicating the number of axes to plot webs between. 3 is minimum.
  #' @param lengthAxes_v Numeric vector indicating the length of each axis. length(lengthAxes_v) = numAxes_v
  #' @param density_v Numeric vector indicating the number of lines in each web
  #' @param save_v logical. TRUE - save using outDir_v and outName_v; FALSE - print to console
  #' @param outDir_v directory to save file. Default to current directory
  #' @param outName_v filename. Default to numAxes_v_lengthAxes_v[1]_density_v_astrid.pdf
  #' @return astrid data and also a plot.
  
  ### Construct output name
  if (outName_v == "astrid.pdf") {
    outName_v <- paste(numAxes_v, lengthAxes_v[1], density_v, outName_v, sep = "_")
  }
  
  ### Get the full and trig angles of each axis
  baseAngle_v <- 360 / numAxes_v
  fullAngles_v <- c(0, sapply(1:(numAxes_v-1), function(x) baseAngle_v * x))
  trigAngles_v <- sapply(fullAngles_v, function(x) {
    while(x > 90) x <- x - 90
    return(x)
  })
  
  ### Get the points for the axis segments
  axisPoints_lsv <- mapply(pointFinder, fullAngles_v, trigAngles_v, lengthAxes_v, SIMPLIFY = F)
  
  ### Get web points
  webPoints_lsdt <- getWebPoints(lengthAxes_v, density_v, fullAngles_v, trigAngles_v)
  
  ### Open device
  if (save_v) {
    pdf(file.path(outDir_v, outName_v))
  }
  
  ### Plot astrid
  plotAstrid(axisPoints_lsv,
             webPoints_lsdt)
  
  ### Close device
  if (save_v) dev.off()
  
  ### Return data
  out_ls <- list("axisPoints" = axisPoints_lsv,
                 "webPoints" = webPoints_lsdt)
  return(out_ls)
  
} # astrid

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





