# Copyright Steven M. Holland, 2018
# May not be redistributed or published without prior permission of the author.

# Basic functions require a vector of three values, or a data frame with three columns
# The first element of the vector or the first column of the data frame corresponds to the top corner of the triangle
# The second element/column corresponds to the bottom left corner of the triangle
# The third element/column corresponds to the bottom right corner of the triangle

# Create a ternary plot from a data frame

ternaryPlot <- function(x, plotPoints=TRUE, labels=c('', '', ''), grid=TRUE, increment=20, ...) {
  ternaryTriangle()
  ternaryLabels(labels[1], labels[2], labels[3])
  if (grid == TRUE) { ternaryGrid(increment) }
  if (plotPoints == TRUE) { ternaryPoints(x, ...) }
}

# Add points from a data frame to an existing ternary plot

ternaryPoints <- function(x, ...) {
  x <- validatedTernaryPoints(x)
  coords <- cartesianFromTernary(x[, 1], x[ , 2], x[, 3])
  points(coords$x, coords$y, ...)
}

# Add a line segment to an existing ternary plot
# Color and line type may be added as additional arguments

ternarySegment <- function(x0, x1, ...) {
  # x0 and x1 are vectors of the endpoint ternary coordinates
  coords0 <- cartesianFromTernary(x0[1], x0[2], x0[3])
  coords1 <- cartesianFromTernary(x1[1], x1[2], x1[3])
  segments(coords0$x, coords0$y, coords1$x, coords1$y, ...)  
}

# Add a polygon to an existing ternary plot
# Color may be added as an additional argument

ternaryPolygon <- function(x, ...) {
  nPoints <- nrow(x)
  xCoord <- vector(mode='numeric', length=nPoints)
  yCoord <- vector(mode='numeric', length=nPoints)
  for (i in 1:nPoints) {
    coords <- cartesianFromTernary(x[i,1], x[i,2], x[i,3])
    xCoord[i] <- coords$x
    yCoord[i] <- coords$y
  }
  polygon(xCoord, yCoord, ...)
}

# Add text to an existing ternary plot
# Text styling may be added as additional arguments

ternaryText <- function(x, label='', ...) {
  coords <- cartesianFromTernary(x[1], x[2], x[3])
  text(coords$x, coords$y, label=label, ...)
}

# ---------------------------------------------------------------------------------------
# The following functions are called by ternaryPlot() and generally will not need to be
# called directly

# Plotting primitives -------------------------------------------------------------------

ternaryTriangle <- function() {
  top <- cartesianFromTernary(100, 0, 0)
  left <- cartesianFromTernary(0, 100, 0)
  right <- cartesianFromTernary(0, 0, 100)
  lim <- c(-1.1, 1.1)
  plot(top$x, top$y, xlim=lim, ylim=lim, type='n', asp=1, axes=FALSE, xlab='', ylab='')
  segments(top$x, top$y, right$x, right$y)
  segments(top$x, top$y, left$x, left$y)
  segments(left$x, left$y, right$x, right$y)
}

ternaryLabels <- function(top='', left='', right='') {
  topCoord   <- cartesianFromTernary(100, 0, 0)
  leftCoord  <- cartesianFromTernary(0, 100, 0)
  rightCoord <- cartesianFromTernary(0, 0, 100)
  text(topCoord$x, topCoord$y, top, pos=3)
  text(leftCoord$x, leftCoord$y, left, pos=1)
  text(rightCoord$x, rightCoord$y, right, pos=1)
}

ternaryGrid <- function(increment) {
  low <- increment
  high <- 100-increment
  
  m  <- seq(low, high, increment)
  nLines <- length(m)
  
  n1 <- o2 <- seq(high, low, -increment)
  n2 <- o1 <- rep(0, nLines)
  
  for (i in 1:nLines) {
    a <- cartesianFromTernary(m[i], n1[i], o1[i])
    b <- cartesianFromTernary(m[i], n2[i], o2[i])
    segments(a$x, a$y, b$x, b$y, col='lightgray', lty=3)
    
    a <- cartesianFromTernary(n1[i], m[i], o1[i])
    b <- cartesianFromTernary(n2[i], m[i], o2[i])
    segments(a$x, a$y, b$x, b$y, col='lightgray', lty=3)
    
    a <- cartesianFromTernary(n1[i], o1[i], m[i])
    b <- cartesianFromTernary(n2[i], o2[i], m[i])
    segments(a$x, a$y, b$x, b$y, col='lightgray', lty=3)
  }
}

# Convert from ternary coordinates to cartesian (x, y) coordinates ----------------------

cartesianFromTernary <- function(top, left, right) {
  y <- (top - 50) / 50      # vertically spans from -1 to 1
  baseHalfWidth <- 1.1547   # 2/tan(60Â°): equilateral triangle
  horizontalHalfWidth <- ((100 - top) * baseHalfWidth) / 100
  horizontalProportion <- (right / (right+left+0.0000001) - 0.5) * 2
  x <- horizontalProportion * horizontalHalfWidth
  xyCoords <- data.frame(cbind(x=x, y=y))
  colnames(xyCoords) <- c('x', 'y')
  xyCoords
}

# Remove rows with NA values and rows that don't sum to 100 -----------------------------

validatedTernaryPoints <- function(x) {
  rowsWithNA <- rowSums(is.na(x))
  xNAremoved <- x[rowsWithNA==0, ]
  rowsEqualing100 <- abs(rowSums(xNAremoved) - 100) <= 2
  xFinal <- xNAremoved[rowsEqualing100, ]
  xFinal
}

# Recalculate percentages so that their sum is 100% -----------------------------------

percentages <- function(x) {
  sums <- rowSums(x)
  y <- x / sums * 100
  y
}