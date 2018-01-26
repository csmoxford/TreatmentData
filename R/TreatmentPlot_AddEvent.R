#' AddEvent
#' 
#' Adds line segments to the plot at the date specified in event. This is a method of the reference class \code{\link{TreatmentPlot}}.
#' 
#' 
#' @name TreatmentPlot_AddEvent 
#' @param event An object of class \code{\link{EventDate}}
#' @param ypos A vector of length 2 defining the vertical location of the line segment within a patients row. Should be between 0 and 1
TreatmentPlot$methods(
  AddEvent = function(event, ypos = c(0,1)) {
    screen(screens[1], new = FALSE)
    if(class(event) != "EventDate") {
      stop("event must be of class eventDate")
    }
    
    validObject(event)
    if(!patientKey %in% names(event@data)) {
      stop("patientKey (", patientKey, ") was not present in event cannot join datasets")
    }
    
    WasCreated()
    
    event@data = Join(event@data)
    
    for(i in 1:length(event@columns)){
      daysFromZero = event@data[,event@columns[i]] - event@data$timeZero
      segments(daysFromZero,event@data$row + ypos[1],daysFromZero,event@data$row + ypos[2], col = event@col, lwd = event@lwd, lend = 1)
    }
    
    labels = sapply(legendElements, function(l) { l$label})
    if(event@label != "" & !event@label %in% labels) {
      legendElements[[length(legendElements) + 1]] <<- list(type = "segments", label = event@label, lwd = event@lwd, col = event@col)
    }
    
  }
)

#' AddEvent
#' 
#' Adds labels to events. This acts as a wrapper of \code{\link[graphics]{text}}. This is a method of the reference class \code{\link{TreatmentPlot}}.
#' 
#' 
#' @name TreatmentPlot_AddEventLabel 
#' @param event An object of class \code{\link{EventDate}}
#' @param cex Relative font size
#' @param pos A position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the specified coordinates.
TreatmentPlot$methods(
  AddEventLabel = function(event, labelColumns, cex = 1, pos = 4) {
    screen(screens[1], new = FALSE)
    if(class(event) != "EventDate") {
      stop("event must be of class eventDate")
    }
    
    validObject(event)
    if(!patientKey %in% names(event@data)) {
      stop("patientKey (", patientKey, ") was not present in event cannot join datasets")
    }
    
    WasCreated()
    
    event@data = Join(event@data)
    
    for(i in 1:length(event@columns)){
      if(!labelColumns[i] %in% names(event@data)) {
        stop("(",labelColumns[i],") was not a column of event@data")
      }
      daysFromZero = event@data[,event@columns[i]] - event@data$timeZero
      text(daysFromZero,event@data$row + 0.5, labels = event@data[,labelColumns[i]], col = event@col, pos = pos, cex = cex)
    }
  }
)

#' AddEventDuration
#' 
#' Adds rectangles to the plot between the dates specified in eventDuration. This is a method of the reference class \code{\link{TreatmentPlot}}.
#' 
#' 
#' @name TreatmentPlot_AddEventDuration
#' @param eventDuration An object of class \code{\link{EventDuration}}
#' @param ypos A vector of length 2 defining the vertical location of the line segment within a patients row. Should be between 0 and 1
TreatmentPlot$methods(
  AddEventDuration = function(eventDuration) {
    screen(screens[1], new = FALSE)
    if(class(eventDuration) != "EventDuration") {
      stop("eventDuration should be of class EventDuration")
    }
    validObject(eventDuration)
    
    WasCreated()
    
    eventDuration@data = Join(eventDuration@data)
    
    
    for(i in 1:length(eventDuration@startDateColumns)) {
      startFromZero = eventDuration@data[,eventDuration@startDateColumns[i]] - eventDuration@data$timeZero
      endFromZero = eventDuration@data[,eventDuration@endDateColumns[i]] - eventDuration@data$timeZero
      rect(startFromZero,eventDuration@data$row + eventDuration@ypos[1],endFromZero,eventDuration@data$row + eventDuration@ypos[2], col =  eventDuration@col, border =  NA )
    }
    
    labels = sapply(legendElements, function(l) { l$label})
    if(eventDuration@label != "" & !eventDuration@label %in% labels) {
      legendElements[[length(legendElements) + 1]] <<- list(type = "rect", label = eventDuration@label, col = eventDuration@col)
    }
  }
)


