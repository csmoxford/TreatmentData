#' \code{EventDate-class}
#'
#' Stores event data and plot formatting information.
#'
#' @slot data A data.frame containing the necessary data
#' @slot columns A vector of columns contain the event
#' @slot label A short name for the event
#' @slot lwd Line width
#' @slot col The colour to use for each event
#'
#'
#' @name EventDate-class
#' @exportClass EventDate
.eventDate = setClass("EventDate", slots = c(
  data = "data.frame",
  columns = "character",
  label = "character",
  lwd = "numeric",
  col = "ANY"
))

setValidity("EventDate", function(object) {
  
  

  if(length(object@columns) < 1) {
    return("columns must be a least length 1")
  }
  if(length(object@label) != 1) {
    return("label should be of length 1")
  }
  if(length(object@lwd) != 1) {
    return("lwd should be of length 1")
  }
  if(length(object@col) != 1) {
    return("col should be of length 1")
  }

  for(i in 1:length(object@columns)) {
    if(!object@columns[i] %in% names(object@data)) {
      return(paste0("column (", object@columns[i], ") is not contained in data"))
    } else if(class(object@data[,object@columns[i]]) != "Date") {
      return(paste0("column (", object@columns[i], ") is not of class 'Date'"))
    }
  }
  
  TRUE
})

#' EventDate class
#'
#' #' A constructor for the \code{\link[=EventDate-class]{EventDate}} class.
#'
#' @param data A data.frame containing the necessary data
#' @param columns A vector of columns contain the event
#' @param label A short name for the event
#' @param lwd Line width
#' @param col The colour to use for each event
#'
#' @return An object of class \code{\link[=EventDate-class]{EventDate}}
#'
#' @name EventDate
#' @export EventDate
EventDate = function(data, columns, label = columns[1], lwd = 4, col = "grey") {
  .eventDate(
    data = data,
    columns = columns,
    label = label,
    lwd = lwd,
    col = col
  )
}
