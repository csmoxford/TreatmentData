#' AddGrid
#' 
#' Adds gridlines to the plot. All parameters are optional. This is a method of the reference class \code{\link{TreatmentPlot}}.
#' 
#' 
#' @name TreatmentPlot_AddGrid
#' @param xDistance Distance between major grid lines in days in the x direction
#' @param xSubDistance Distance between sub grid lines in days in the x direction. These are dashed
#' @param col the colour of the gridlines
#' @param lwd the line weight of the gridlines
TreatmentPlot$methods(
  AddGrid = function(xDistance = NA, xSubDistance = NA, col = "grey", lwd = 1) {
    screen(screens[1], new = FALSE)
    # horizontal lines
    abline(h = 2:(dim(data)[1]), col = col, lwd = lwd)
    
    # vertical lines
    if(!is.na(xSubDistance)) {
      abline(v = -200:200*xSubDistance, col = col, lty = 2, lwd = lwd)
    }
    if(!is.na(xDistance)) {
      abline(v = -100:100*xDistance, col = col, lty = 1, lwd = lwd)
    }
  }
)

#' AddTreatmentYAxis
#' 
#' Adds treatment labels to the yaxis. All parameters are optional. This is a method of the reference class \code{\link{TreatmentPlot}}.
#' 
#' 
#' @name TreatmentPlot_AddTreatmentYAxis
#' @param treatmentLabels A character vector of treatment labels to use instead of the codes stored in the \code{\link{TreatmentPlot}} object.
#' @param xSubDistance Distance between sub grid lines in days in the x direction. These are dashed
#' @param lwd The line weight of the axis
#' @param cex The text size
TreatmentPlot$methods(
  AddTreatmentYAxis = function(treatmentLabels = NA,lwd = 2, cex = 1) {
    print(screens)
    screen(screens[1], new = FALSE)
    treatmentCodes = unique(data[,treatmentColumn])
    if(!is.na(treatmentLabels)) {
      if(length(treatmentLabels) != length(treatmentCodes)) {
        stop("treatmentCodes must match the data stored in treatmentColumn (", treatmentColumn, ") including the order")
      }
      
      message("TreatmentLabels are matched as follows:\n", paste0("Code ",treatmentCodes, " to ", treatmentLabels, collapse = "\n"),"\nCorrect this order if necessary")
      
    } else {
      treatmentLabels = treatmentCodes
    }
    
    nTreatments = sapply(treatmentCodes, function(t) sum(data[,treatmentColumn] == treatmentCodes))

    values = c(0,cumsum(rev(nTreatments))) + 1
    labelAt = c()
  
    
    par(xpd = TRUE)
    for(i in 2:length(values)) {
      rect(xlim[1],values[i-1],xlim[2],values[i], lwd = lwd)
      labelAt = c(labelAt, (values[i-1] + values[i]) / 2)
    }
    par(xpd = FALSE)
   
    axis(2, at = labelAt, labels = treatmentLabels, tick = FALSE, cex = 1)

    
  }
)

#' AddXAxis
#' 
#' Adds the xasis to the plot. All parameters are optional. This is a method of the reference class \code{\link{TreatmentPlot}}.
#' 
#' 
#' @name TreatmentPlot_AddXAxis
#' @param every Distance between each tick. On the scale of scale
#' @param scale A character string one of "days","weeks","months","years". Default is days
#' @param lwd The line weight of the axis
TreatmentPlot$methods(
  AddXAxis = function(every = 21, scale = "days", lwd = 2) {
    screen(screens[1], new = FALSE)
    if(substr(scale,1,1) == "d") {
      axis(1, at = -100:100*every, lwd = lwd)
    } else if(substr(scale,1,1) == "w") {
      axis(1, at = -100:100*7*every, labels = -100:100*every, lwd = lwd)
    } else if(substr(scale,1,1) == "m") {
      axis(1, at = -100:100*30.4*every, labels = -100:100*every, lwd = lwd)
    } else if(substr(scale,1,1) == "y") {
      axis(1, at = -100:100*365.25*every, labels = -100:100*every, lwd = lwd)
    } else {
      stop("scale must be one of days, weeks, months, years (or the first letter)")
    }
    
  }
)


#' AddPatientKey
#' 
#' Adds the patientKeys to the plot. All parameters are optional. This is a method of the reference class \code{\link{TreatmentPlot}}.
#' 
#' 
#' @name TreatmentPlot_AddPatientKey
#' @param xpos The position on the xaxis. Default is 0, these will be left-aligned to the right of this point
#' @param col The colour of the text
#' @param cex The size of the text
TreatmentPlot$methods(
  AddPatientKey = function(xpos = 0, col = 1, cex = 1) {
    WasCreated()
    screen(screens[1], new = FALSE)
    text(xpos, data$row + 0.5, labels = data[,patientKey], col = col, cex = cex, pos = 4)
  }
)

#' AddXLabel
#' 
#' Adds a label to the xaxis. This is a method of the reference class \code{\link{TreatmentPlot}}.
#' 
#' 
#' @name TreatmentPlot_AddXLabel
#' @param xlab The label
#' @param cex The size of the text
#' @param line The distance to draw the label from the xaxis
TreatmentPlot$methods(
  AddXLabel = function(xlab, cex = 1, line = 2.2) {
    WasCreated()
    screen(screens[1], new = FALSE)
    mtext(xlab, side = 1, cex = cex, line = line)
  }
)

#' AddLegend
#' 
#' Adds the legend to the plot. Items to add to the legend are added to the TreatmentPlot object automatically. This is a method of the reference class \code{\link{TreatmentPlot}}.
#' 
#' 
#' @name TreatmentPlot_AddLegend
#' @param itemsOnRow The number of items to display on each row
#' @param mar Give the legend a margin (is passed to par). The default is c(0,0,0,0). Each entry corresponds to bottom, left, top, right
#' @param xoffset An offset parameter in the x direction. Should be between 0 and 1
TreatmentPlot$methods(
  AddLegend = function(itemsOnRow, mar = c(0,0,0,0), xoffset = 0) {
    WasCreated()
    screen(screens[2], new = FALSE)
    
    totalItems = length(legendElements)
    numberRows = ceiling(totalItems /itemsOnRow)
    
    par(mar = mar)
    plot(0,0,xlim=c(0,1),ylim = 0.5+c(0,numberRows),col = 0,axes = FALSE,xlab = "",ylab = "", xaxs = "i", yaxs = "i")
    
    for(i in 1:totalItems) {
      element = legendElements[[i]]
      position = .legendGetPosition(i, itemsOnRow, totalItems)
      position$x = position$x + xoffset
      if(element$type == "segments") {
        segments(position$x + 0.1/itemsOnRow, position$y - 0.3, position$x + 0.1/itemsOnRow, position$y + 0.3, col = element$col, lwd = element$lwd, lend = 1)
      } else if(element$type == "rect") {
        rect(position$x, position$y - 0.3, position$x + 0.2/itemsOnRow, position$y + 0.3, col = element$col, border = NA) 
      }
      text(position$x,position$y, labels = element$label, pos = 2)
      
      # points(position$x, position$y)
    }
  }
)