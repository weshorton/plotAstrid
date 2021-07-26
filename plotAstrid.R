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

####################
### DEPENDENCIES ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####################

suppressMessages(library(ggplot2))
suppressMessages(library(data.table))
suppressMessages(library(wrh.rUtils))

#################
### ARGUMENTS ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################

### numAxes - numerical vector of how many axes and correspondigly "webs" you want, with 3 being the minimum.
### length - the length of the line that each axis is relative to a standard scale
### density - the number of lines in the web, default is to be same as length

numAxes_v <- 5
lengthAxes_v <- 6
density_v <- 6

