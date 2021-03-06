% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_EventDuration.R
\docType{class}
\name{EventDuration-class}
\alias{EventDuration-class}
\alias{.eventDuration}
\title{\code{eventDuration-class}}
\value{
An object of class \code{\link{EventDuration}}
}
\description{
Stores event duration data and plot formatting information.
}
\section{Slots}{

\describe{
\item{\code{data}}{A data.frame containing the necessary data}

\item{\code{startDateColumns}}{A vector of columns contain the start of treatment times}

\item{\code{endDateColumns}}{A vector of columns contain the end of treatment times. These are paired with startDateColumns}

\item{\code{label}}{A short name for the treatment}

\item{\code{ypos}}{A vector of length 2. This specifies the vertical min and max to plot the rect in}

\item{\code{col}}{The colour to use for each treatment}
}}

