% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TreatmentPlot.R
\docType{class}
\name{TreatmentPlot}
\alias{TreatmentPlot}
\alias{TreatmentPlot-class}
\title{\code{TreatmentPlot-class}}
\description{
A reference class used to create a treatment plot at a patient level.
}
\details{
Once the TreatmentPlot class is created you need to prepare the plot using the \code{\link[=TreatmentPlot_CreatePlot]{CreatePlot}} method (\code{object$CreatePlot()}). Use the Add methods to add data, axes, labels and legends to the plot and then call the \code{\link[=TreatmentPlot_CloseScreens]{CloseScreens}} method (\code{object$CloseScreens()}) to finish. There is a list of methods below and an example.

List of methods which can be called using the syntax \code{object$Method()}:

\itemize{
  \item{\code{\link[=TreatmentPlot_CreatePlot]{CreatePlot}}}
  \item{\code{\link[=TreatmentPlot_AddEvent]{AddEvent}}}
  \item{\code{\link[=TreatmentPlot_AddEventLabel]{AddEventLabel}}}
  \item{\code{\link[=TreatmentPlot_AddEventDuration]{AddEventDuration}}}
  \item{\code{\link[=TreatmentPlot_AddGrid]{AddGrid}}}
  \item{\code{\link[=TreatmentPlot_AddTreatmentYAxis]{AddTreatmentYAxis}}}
  \item{\code{\link[=TreatmentPlot_AddXAxis]{AddXAxis}}}
  \item{\code{\link[=TreatmentPlot_AddPatientKey]{AddPatientKey}}}
  \item{\code{\link[=TreatmentPlot_AddXLabel]{AddXLabel}}}
  \item{\code{\link[=TreatmentPlot_AddLegend]{AddLegend}}}
  \item{\code{\link[=TreatmentPlot_CloseScreens]{CloseScreens}}}
}

Data can be accessed using the @ operator E.G. object@data. Methods are accessed using the $ operator E.G. object$CreatePlot. Making a copy and then modifying the copy will cause the original to change since this is a reference class.
}
\section{Fields}{

\describe{
\item{\code{data}}{A data.frane containing patient key and treatment}

\item{\code{patientKey}}{the name of the patientKey column}

\item{\code{treatmentColumn}}{the name of the treatmentColumn}

\item{\code{timeZeroColumn}}{the name of the column to use as time zero}

\item{\code{legendElements}}{A list of elements to add to the legend. This is generated automatically. This is then used by \code{\link[=TreatmentPlot_AddLegend]{AddLegend}} to add the legend to the plot}

\item{\code{screens}}{A vector of screen numbers for the main plot and legend. This is set by \code{\link[=TreatmentPlot_CreatePlot]{CreatePlot}}. More details can be found in \code{\link[=TreatmentPlot_CloseScreens]{CloseScreens}} and the full documentation (\code{\link[graphics]{screen}})}

\item{\code{xlim}}{The range the xaxis should encompass. This is set by \code{\link[=TreatmentPlot_CreatePlot]{CreatePlot}}}
}}


\examples{
##########################################################################
# Create some dummy data for testing purposes
patientData = data.frame(
  patid = paste0("PT", 1:20), 
  treatment = c(rep("Placebo",10), rep("Treatment", 10)), 
  zeroDate = as.Date(rep(15000,20), origin = "1970-01-01"), stringsAsFactors = FALSE)

# End of treatment event
eventData = data.frame(
  patid = paste0("PT", 1:20), 
  EOT = as.Date(floor(runif(20,15040,15050)), origin = "1970-01-01", stringsAsFactors = FALSE)
)

# treatment X
treatmentData = data.frame(
  patid = paste0("PT", 1:20), 
  SOT1 = as.Date(floor(runif(20,15005,15010)), origin = "1970-01-01"),
  EOT1 = as.Date(floor(runif(20,15011,15015)), origin = "1970-01-01"),
  SOT2 = as.Date(floor(runif(20,15020,15025)), origin = "1970-01-01"),
  EOT2 = eventData$EOT,
  stringsAsFactors = FALSE)

# pauses to treatment x
treatmentPauses = data.frame(
  patid = sample(paste0("PT", 1:20), size = 10),
  SOT = as.Date(floor(runif(20,15030,15033)), origin = "1970-01-01"),
  EOT = as.Date(floor(runif(20,15034,15038)), origin = "1970-01-01"),
  stringsAsFactors = FALSE)


# treatment y
treatmentData2 = data.frame(
  patid = paste0("PT", 1:20), 
  SOT1 = as.Date(floor(runif(20,15005,15010)), origin = "1970-01-01"),
  EOT1 = as.Date(floor(runif(20,15011,15015)), origin = "1970-01-01"),
  SOT2 = as.Date(floor(runif(20,15020,15025)), origin = "1970-01-01"),
  EOT2 = eventData$EOT
  , stringsAsFactors = FALSE)


# treatment z, these are single day events with two possible doses
n = 300
treatmentDataSingle = data.frame(
  patid = paste0("PT", sample(1:20,size = n,replace = TRUE)), 
  treatmentTime = as.Date(floor(runif(n,15005,15040)), origin = "1970-01-01"),
  dose = sample(c(50,100), size = n, replace = TRUE),
  stringsAsFactors = FALSE
)

##########################################################################
# Create some EventDate objects for single date events
# Create some EventDuration objects for events over a number of days

# treatment x
tDuration = EventDuration(
  data = treatmentData,
  startDateColumns = c("SOT1","SOT2"),
  endDateColumns = c("EOT1", "EOT2"),
  label = "Treatment X",
  ypos = c(0.5,0.8),
  col = "blue"
)

# pause in treatment x, use white to clear this area of plot
tDurationPause = EventDuration(
  data = treatmentPauses,
  startDateColumns = c("SOT"),
  endDateColumns = c("EOT"),
  label = "Treatment X",
  ypos = c(0.5,0.8),
  col = "white"
)

# treatment y
tDuration2 = EventDuration(
  data = treatmentData2,
  startDateColumns = c("SOT1","SOT2"),
  endDateColumns = c("EOT1", "EOT2"),
  label = "Treatment Y",
  ypos = c(0.2,0.5),
  col = "orange"
)



library(sqldf)
# treatment z, dose 50mg
singleData = EventDate(
  sqldf("select * from treatmentDataSingle where dose == 50"),
  columns = "treatmentTime",
  label = "treatment Z (50mg)",
  col = "green",
  lwd = 3
)

# treatment z dose 100mg
singleData2 = EventDate(
  sqldf("select * from treatmentDataSingle where dose == 100"),
  columns = "treatmentTime",
  label = "treatment Z (100mg)",
  col = "green4",
  lwd = 3
)

# end of treatment event
EOTevent = EventDate(
  data = eventData,
  columns = "EOT",
  label = "End of treatment",
  col = "red"
)

##########################################################################
# The base plot just needs the patientKey, treatment and zeroDate

# Creat the the reference object
treatmentPlot = TreatmentPlot(patientData, "patid", "treatment","zeroDate")

# Prepare the plot area.
treatmentPlot$CreatePlot(xlim = c(0,50), numberRowsLegend = 1)
# Note: this function uses screens. It may be added to a current screen in a
# multiscreen plot. Unitentionally leaving screens open may cause problems.
# see ?split.screen for more details on screens

# add the event duration elements for treatments x and y
treatmentPlot$AddEventDuration(tDuration)
treatmentPlot$AddEventDuration(tDurationPause)
treatmentPlot$AddEventDuration(tDuration2)

# add the events for treatment z at both doses
treatmentPlot$AddEvent(singleData, ypos = c(0.5,1))
treatmentPlot$AddEvent(singleData2, ypos = c(0.5,1))

# add the end of treatment event
treatmentPlot$AddEvent(EOTevent)

# The following functions tidy up the plot
treatmentPlot$AddGrid(xDistance = 21, xSubDistance = 7)
treatmentPlot$AddTreatmentYAxis()
treatmentPlot$AddPatientKey()
treatmentPlot$AddXAxis()
treatmentPlot$AddXLabel("Time from Registration (days)")

# Add the legend
treatmentPlot$AddLegend(itemsOnRow = 4, xoffset = 0.05)
# legend items are stored everytime an event is added so we
# don't need to add extras here

# close the screens used by treatmentPlot
treatmentPlot$CloseScreens()
# Close all screens (not necesary for the example)
close.screen(all.screens = TRUE)
}
