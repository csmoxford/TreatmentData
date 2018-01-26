
#' CreatePlot 
#' 
#' Prepares the plot regions. Must be called prior to calling other plot functions. This is a method of the reference class \code{\link{TreatmentPlot}}.
#' 
#' 
#' @name TreatmentPlot_CreatePlot 
#' @param xlim The range of values in the x direction
#' @param numberRowsLegend Is used to define the space for the legend screen size. If zero then there will be no space for the legend and you should avoid calling AddLegend
#' @param xlab Add a xaxis title. Consider using AddXLabel for more flexibility
#' @param ylab Add a yaxis title. Not recommended
#' @param mar set the mar parameter of par for the main plot area
TreatmentPlot$methods(
  CreatePlot = function(xlim = c(0,7), numberRowsLegend = 0, xlab = "", ylab = "", mar = c(4,4,1,1)) {
    if(length(screens) != 0) {
      stop("Screens already initialised")
    }
    size = dev.size()
    split = numberRowsLegend * 0.6 / size[1]
    
    par(mar = c(0,0,0,0))
    screens <<- split.screen(
      figs = matrix(
        c(0,1,split,1,
          0,1,0,split),
        ncol =4, byrow = TRUE)
    )
    screen(screens[1])
    par(mar = mar)
    xlim <<- xlim
    
    plot(0,0,col = 0, xlab = xlab, ylab = ylab, xlim = xlim, ylim = 1 + c(0,dim(data)[1]), axes = FALSE, xaxs = "i", yaxs = "i")
    box()
  }
)