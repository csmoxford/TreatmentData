
#' \code{TreatmentPlot-class}
#' 
#' A reference class used to create a treatment plot at a patient level.
#' 
#' @details
#' 
#' Once the TreatmentPlot class is created you need to prepare the plot using the \code{\link[=TreatmentPlot_CreatePlot]{CreatePlot}} method (\code{object$CreatePlot()}). Use the Add methods to add data, axes, labels and legends to the plot and then call the \code{\link[=TreatmentPlot_CloseScreens]{CloseScreens}} method (\code{object$CloseScreens()}) to finish. There is a list of methods below and an example.
#' 
#' List of methods which can be called using the syntax \code{object$Method()}:
#' 
#' \itemize{
#'   \item{\code{\link[=TreatmentPlot_CreatePlot]{CreatePlot}}}
#'   \item{\code{\link[=TreatmentPlot_AddEvent]{AddEvent}}}
#'   \item{\code{\link[=TreatmentPlot_AddEventLabel]{AddEventLabel}}}
#'   \item{\code{\link[=TreatmentPlot_AddEventDuration]{AddEventDuration}}}
#'   \item{\code{\link[=TreatmentPlot_AddGrid]{AddGrid}}}
#'   \item{\code{\link[=TreatmentPlot_AddTreatmentYAxis]{AddTreatmentYAxis}}}
#'   \item{\code{\link[=TreatmentPlot_AddXAxis]{AddXAxis}}}
#'   \item{\code{\link[=TreatmentPlot_AddPatientKey]{AddPatientKey}}}
#'   \item{\code{\link[=TreatmentPlot_AddXLabel]{AddXLabel}}}
#'   \item{\code{\link[=TreatmentPlot_AddLegend]{AddLegend}}}
#'   \item{\code{\link[=TreatmentPlot_CloseScreens]{CloseScreens}}}
#' }
#' 
#' Data can be accessed using the @ operator E.G. object@data. Methods are accessed using the $ operator E.G. object$CreatePlot. Making a copy and then modifying the copy will cause the original to change since this is a reference class.
#' 
#' @field data A data.frane containing patient key and treatment
#' @field patientKey the name of the patientKey column
#' @field treatmentColumn the name of the treatmentColumn
#' @field timeZeroColumn the name of the column to use as time zero
#' @field legendElements A list of elements to add to the legend. This is generated automatically. This is then used by \code{\link[=TreatmentPlot_AddLegend]{AddLegend}} to add the legend to the plot
#' @field screens A vector of screen numbers for the main plot and legend. This is set by \code{\link[=TreatmentPlot_CreatePlot]{CreatePlot}}. More details can be found in \code{\link[=TreatmentPlot_CloseScreens]{CloseScreens}} and the full documentation (\code{\link[graphics]{screen}})
#' @field xlim The range the xaxis should encompass. This is set by \code{\link[=TreatmentPlot_CreatePlot]{CreatePlot}}
#' 
#' 
#' @example inst/Examples/TreatmentPlot_helpFile.R
#' @name TreatmentPlot
#' @exportClass TreatmentPlot
#' @aliases TreatmentPlot-class
#' @export TreatmentPlot
#' @import methods
TreatmentPlot = setRefClass("TreatmentPlot", fields = c(
  data = "data.frame",
  patientKey = "character",
  treatmentColumn = "character",
  timeZeroColumn = "character",
  legendElements = "list",
  screens = "numeric",
  xlim = "numeric"
))


TreatmentPlot$methods(
  initialize = function(patientData, patientKey, treatmentColumn, timeZeroColumn) {
    
    if(!patientKey %in% names(patientData)) {
      stop("patientKey (",patientKey,") must be a column name in patientData")
    }
    if(!treatmentColumn %in% names(patientData)) {
      stop("treatmentColumn (",treatmentColumn,") must be a column name in patientData")
    }
    if(!timeZeroColumn %in% names(patientData)) {
      stop("timeZeroColumn (",timeZeroColumn,") must be a column name in patientData")
    }
    if(class(patientData[,timeZeroColumn]) != "Date") {
      stop("timeZeroColumn (",timeZeroColumn,") must be of class 'Date'")
    }
    
    if(dim(patientData)[1] > 1) {
      nTreatments = length(unique(patientData[,treatmentColumn]))
      
      nTreatmentChanges = 1
      for(i in 2:dim(patientData)[1]) {
        if(patientData[i-1,treatmentColumn] != patientData[i,treatmentColumn]) {
          nTreatmentChanges = nTreatmentChanges + 1
        }
      }
      if(nTreatments != nTreatmentChanges) {
        warning("Patients are not ordered by treatment. Some functions may not work")
      }
    }
    
    patientData$row = dim(patientData)[1]:1
    
    data <<- patientData
    patientKey <<- patientKey
    treatmentColumn <<- treatmentColumn
    timeZeroColumn <<- timeZeroColumn
    legendElements <<- list()
    
  }
)


TreatmentPlot$methods(
  new = function(patientData, patientKey, treatmentColumn) initialize(patientData, patientKey, treatmentColumn)
)

TreatmentPlot$methods(
  show = function() {
    message("Reference Object of type TreatmentPlot.")
  }
)

TreatmentPlot$methods(
  Join = function(joinData) {

    # get row for each patient
    joinData$row = sapply(joinData[,patientKey], function(id) data$row[data[, patientKey] == id])
    joinData$timeZero = sapply(joinData[,patientKey], function(id) data[data[, patientKey] == id, timeZeroColumn])
    
    return(joinData)
  }
)

#' CloseScreens
#' 
#' Closes the screens created by \code{\link[=TreatmentPlot_CreatePlot]{CreatePlot}}.  This is a method of the reference class \code{\link{TreatmentPlot}}.
#' 
#' This class uses two screens. The reference to each is stored in \code{object$screens}. \code{object$screens[1]} is the screen number for the main plot area. \code{object$screens[2]} is te screen number for the legend. To add to screen i use \code{screen(i)}. For more details see \code{\link[graphics]{screen}}.
#' 
#' 
#' @name TreatmentPlot_CloseScreens
TreatmentPlot$methods(
  CloseScreens = function(joinData) {
    WasCreated()
    close.screen(screens)
    screens <<- numeric(0)
  }
)

TreatmentPlot$methods(
  WasCreated = function() {
    if(length(screens) != 2) {
      stop("The plot region has not been created using CreatePlot. For help see ?TreatmentPlot_CreatePlot")
    }
  }
)