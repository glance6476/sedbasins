# Copyright Steven M. Holland, 2018
# May not be redistributed or published without prior permission of the author.

# NOTE: requires ternary.r

# MAIN FUNCTION ------------------------------------------------------------------------
# Generally, provenancePlot() is the only function that will need to be called. It uses
# all the remaining functions to do its work.

provenancePlot <- function(x, type=c('QFL', 'QmFLt', 'QpLvLs', 'QmPK', 'allFour'), ...) {
  if (type == 'QFL') {
    y <- ternaryValues(x, type)
    ternaryPlot(y, labels=colnames(y), grid=FALSE, plotPoints = FALSE)
    qflFields()
    ternaryPoints(y, ...)
  } else if (type == 'QmFLt') {
    y <- ternaryValues(x, type)
    ternaryPlot(y, labels=colnames(y), grid=FALSE, plotPoints = FALSE)
    qmfltFields()
    ternaryPoints(y, ...)
  } else if (type == 'QpLvLs') {
    y <- ternaryValues(x, type)
    ternaryPlot(y, labels=colnames(y), grid=FALSE, ...)
  } else if (type == 'QmPK') {
    y <- ternaryValues(x, type)
    ternaryPlot(y, labels=colnames(y), grid=FALSE, ...)
  } else if (type == 'allFour') {
    oldpar <- par(no.readonly = TRUE)
    par(mfrow=c(2,2), mar=c(2, 1, 1, 1) + 0.1)
    
    y <- ternaryValues(x, 'QFL')
    ternaryPlot(y, labels=colnames(y), grid=FALSE, plotPoints = FALSE)
    qflFields()
    ternaryPoints(y, ...)
    
    y <- ternaryValues(x, 'QmFLt')
    ternaryPlot(y, labels=colnames(y), grid=FALSE, plotPoints = FALSE)
    qmfltFields()
    ternaryPoints(y, ...)
    
    y <- ternaryValues(x, 'QpLvLs')
    ternaryPlot(y, labels=colnames(y), grid=FALSE, ...)
    
    y <- ternaryValues(x, 'QmPK')
    ternaryPlot(y, labels=colnames(y), grid=FALSE, ...)
    
    par(oldpar)
  } else {
    warning(paste('Did not recognize plot type \'', type, '\'. Choices are QFL, QmFLt, QpLvLs, and QmPK', sep=''), call.=FALSE)
  }
}

# DATA FRAME FUNCTION -----------------------------------------------------------------

ternaryValues <- function(x, type=c('QFL', 'QmFLt', 'QpLvLs', 'QmPK')) {
  # create dummy values of zero in case columns don't exist
  quartz <- 0
  chert <- 0
  plagioclase <- 0
  kfeldspar <- 0
  feldspar <- 0
  lithic <- 0
  plutonic <- 0
  volcanic <- 0
  metamorphic <- 0
  sedimentary <- 0
  shale <- 0
  
  # if the columns do exist, replace the dummy values with the actual values
  if (length(x$quartz) > 0)      quartz      <- x$quartz
  if (length(x$chert) > 0)       chert       <- x$chert
  if (length(x$plagioclase) > 0) plagioclase <- x$plagioclase
  if (length(x$kfeldspar) > 0)   kfeldspar   <- x$kfeldspar
  if (length(x$feldspar) > 0)    feldspar    <- x$feldspar
  if (length(x$lithic) > 0)      lithic      <- x$lithic
  if (length(x$plutonic) > 0)    plutonic    <- x$plutonic
  if (length(x$volcanic) > 0)    volcanic    <- x$volcanic
  if (length(x$metamorphic) > 0) metamorphic <- x$metamorphic
  if (length(x$sedimentary) > 0) sedimentary <- x$sedimentary
  if (length(x$shale) > 0)       shale       <- x$shale
  
  top <- left <- right <- 0
  colnames <- c('', '', '')
  if (type=='QFL') {
    top   <- quartz + chert
    left  <- plagioclase + kfeldspar + feldspar
    right <- lithic + plutonic + volcanic + metamorphic + sedimentary + shale
    colnames <- c('Q', 'F', 'L')	
  } else if (type=='QmFLt') {
    top   <- quartz
    left  <- plagioclase + kfeldspar + feldspar
    right <- chert + lithic + plutonic + volcanic + metamorphic + sedimentary + shale
    colnames <- c('Qm', 'F', 'Lt')
  } else if (type=='QpLvLs') {
    top   <- chert
    left  <- volcanic
    right <- shale + sedimentary
    colnames <- c('Qp', 'Lv', 'Ls')
  } else if (type=='QmPK') {
    top   <- quartz
    left  <- plagioclase
    right <- kfeldspar
    colnames <- c('Qm', 'P', 'K')
  } else {
    warning(paste('Did not recognize plot type \'', type, '\'. Choices are QFL, QmFLt, QpLvLs, and QmPK', sep=''), call.=FALSE)
  }
  
  # recalculate percentages
  total  <- top + left + right
  top    <- round(  top / total * 100, 1)
  left   <- round( left / total * 100, 1)
  right  <- round(right / total * 100, 1)
  
  # assemble and return the data frame
  y <- as.data.frame(cbind(top, left, right))
  rownames(y) <- rownames(x)
  colnames(y) <- colnames
  if (type=='QpLvLs') {
    y <- y[!is.nan(y$Lv), ]   # remove rocks that lack any Qp, Lv, or Ls grains
  }
  y
}

# FIELD BOUNDARIES --------------------------------------------------------------------

pettijohnClassification <- function() {
  ternarySegment(c(95,  5,  0), c(90,  5,  5)) # quartz arenite / subarkosic arenite
  ternarySegment(c(95,  0,  5), c(90,  5,  5)) # quartz arenite / sublithic arenite
  ternarySegment(c(75, 25,  0), c(50, 25, 25)) # subarkose / arkose
  ternarySegment(c(75,  0, 25), c(50, 25, 25)) # sublithic arenite / lithic arenite
  ternarySegment(c(90,  5,  5), c(0,  50, 50)) # lithic-rich / feldspar-rich
}

qflFields <- function(shading = TRUE) {
  if (shading == TRUE) {
    ternaryPolygon(rbind(c(100, 0, 0), c(0, 100, 0), c(0, 85, 15), c(97, 0, 3), c(100, 0, 0)), col=gray(0.8)) # continental block in darker gray
    ternaryPolygon(rbind(c(97, 0, 3), c(51.2, 40, 8.7), c(25, 0, 75), c(97, 0, 3)), col=gray(0.95))        # recycled orogen in lighter gray
    # arc is white (unshaded)
  }
  
  ternarySegment(c(97.0,  0.0,  3.0), c( 0.0, 85.0, 15.0))        # continental block
  ternarySegment(c(82.0, 18.0,  0.0), c(79.9, 15.8,  5.4), lty=3) # craton interior
  ternarySegment(c(55.0, 45.0,  0.0), c(51.2, 40.0,  8.7), lty=3) # transitional cont.
  ternarySegment(c(51.2, 40.0,  8.7), c(25.0,  0.0, 75.0))        # recycled orogen
  ternarySegment(c(17.5, 70.5, 13.4), c(33.3, 12.8, 54.3), lty=3) # dissected arc
  ternarySegment(c( 0.0, 50.0, 50.0), c(25.0,  0.0, 75.0), lty=3) # transitional arc
}

qmfltFields <- function(shading = TRUE) {
  if (shading == TRUE) {
    ternaryPolygon(rbind(c(100, 0, 0), c(0, 100, 0), c(0, 77, 23), c(89, 0, 11), c(100, 0, 0)), col=gray(0.8))        # continental block in darker gray
    ternaryPolygon(rbind(c(89, 0, 11), c(67.1, 19.1, 14.2), c(0, 13, 87), d <- c(0, 0, 100), c(89, 0, 11)), col=gray(0.95)) # recycled orogen in lighter gray
    # arc is white (unshaded)
  }
  ternarySegment(c(89.0,  0.0, 11.0), c( 0.0, 77.0, 23.0))        # continental block
  ternarySegment(c(80.0, 20.0,  0.0), c(74.2, 13.2, 13.4), lty=3) # craton interior
  ternarySegment(c(67.1, 19.1, 14.2), c( 0.0, 13.0, 87.0))        # recycled orogen
  ternarySegment(c(49.8, 16.7, 33.1), c(58.0,  0.0, 42.0), lty=3) # quartzose recycled
  ternarySegment(c(21.1, 14.2, 63.9), c(29.0,  0.0, 71.0), lty=3) # trans. recycled
  ternarySegment(c(49.1, 34.9, 16.5), c(29.4, 16.0, 55.0))        # mixed arc
  ternarySegment(c(57.0, 43.0,  0.0), c(49.1, 34.9, 16.5), lty=3) # transitional cont.
  ternarySegment(c(21.9, 58.4, 20.4), c(29.4, 16.0, 55.0), lty=3) # dissected arc
  ternarySegment(c( 0.0, 47.0, 53.0), c(12.7, 14.4, 73.1), lty=3) # transitional arc
}


# DATA INTEGRITY CHECK

columnNameCheck <- function(x) {
  missing <- ''
  if (length(x$quartz) == 0)      missing <- paste(missing, ' quartz')
  if (length(x$chert) == 0)       missing <- paste(missing, ' chert')
  if (length(x$plagioclase) == 0) missing <- paste(missing, ' plagioclase')
  if (length(x$kfeldspar) == 0)   missing <- paste(missing, ' kfeldspar')
  if (length(x$feldspar) == 0)    missing <- paste(missing, ' feldspar')
  if (length(x$lithic) == 0)      missing <- paste(missing, ' lithic')
  if (length(x$plutonic) == 0)    missing <- paste(missing, ' plutonic')
  if (length(x$volcanic) == 0)    missing <- paste(missing, ' volcanic')
  if (length(x$metamorphic) == 0) missing <- paste(missing, ' metamorphic')
  if (length(x$sedimentary) == 0) missing <- paste(missing, ' sedimentary')
  if (length(x$shale) == 0)       missing <- paste(missing, ' shale')
  
  if (nchar(missing) > 0) {
    warning(paste('One or more columns appear to be missing: ', missing, '\n', ' If you do not have these grain types in your data, this is not a problem.', '\n', ' If you do have these grain types, rename the columns in your data frame to match these names exactly'), call.=FALSE)
  }
}

