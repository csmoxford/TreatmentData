#' \code{eventDuration-class} 
#'
#' Stores event duration data and plot formatting information.
#'
#' @slot data A data.frame containing the necessary data
#' @slot startDateColumns A vector of columns contain the start of treatment times
#' @slot endDateColumns A vector of columns contain the end of treatment times. These are paired with startDateColumns
#' @slot label A short name for the treatment
#' @slot ypos A vector of length 2. This specifies the vertical min and max to plot the rect in
#' @slot col The colour to use for each treatment
#'
#' @return An object of class \code{\link{EventDuration}}
#'
#' @name EventDuration-class
#' @exportClass EventDuration
.eventDuration = setClass("EventDuration", slots = c(
  data = "data.frame",
  startDateColumns = "character",
  endDateColumns = "character",
  ypos = "numeric",
  label = "character",
  col = "ANY"
))

setValidity("EventDuration", function(object) {
  if(length(object@startDateColumns) < 1) {
    return("startDateColumns must be a least length 1")
  }
  if(length(object@endDateColumns) < 1) {
    return("endDateColumns must be a least length 1")
  }
  if(length(object@startDateColumns) != length(object@endDateColumns)) {
    return("startDateColumns and endDateColumns must be the same length. These are used in pairs if length is greater than 1")
  }
  if(length(object@label) != 1) {
    return("label should be of length 1")
  }
  if(length(object@col) != 1) {
    return("col should be of length 1")
  }
  
  for(i in 1:length(object@startDateColumns)) {
    if(!object@startDateColumns[i] %in% names(object@data)) {
      return(paste0("column (", object@startDateColumns[i], ") is not contained in data"))
    } else if(class(object@data[,object@startDateColumns[i]]) != "Date") {
      return(paste0("column (", object@startDateColumns[i], ") is not of class 'Date'"))
    }
  }
  
  for(i in 1:length(object@endDateColumns)) {
    if(!object@endDateColumns[i] %in% names(object@data)) {
      return(paste0("column (", object@endDateColumns[i], ") is not contained in data"))
    } else if(class(object@data[,object@endDateColumns[i]]) != "Date") {
      return(paste0("column (", object@endDateColumns[i], ") is not of class 'Date'"))
    }
  }
  
  TRUE
})


#' \code{EventDuration} class constructor
#'
#' A constructor for the \code{\link[=EventDuration-class]{EventDuration}} class.
#'
#' @param data A data.frame containing the necessary data
#' @param startDateColumns A vector of columns contain the start of treatment times
#' @param endDateColumns A vector of columns contain the end of treatment times. These are paired with startDateColumns
#' @param label A short name for the treatment
#' @param ypos A vector of length 2. This specifies the vertical min and max to plot the rect in
#' @param col The colour to use for each treatment
#'
#' @return An object of class \code{\link[=EventDuration-class]{EventDuration}}
#'
#' @name EventDuration
#' @aliases EventDuration
#' @export EventDuration
EventDuration = function(data, startDateColumns, endDateColumns, label = startDateColumns[1], col = "blue", ypos = c(0.2,0.8)) {
  .eventDuration(
    data = data,
    startDateColumns = startDateColumns,
    endDateColumns = endDateColumns,
    label = label,
    ypos = ypos,
    col = col
  )
}