% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dependanciesData.R
\name{dbselect}
\alias{dbselect}
\title{dbselect}
\usage{
dbselect(
  db,
  dayNightSelected,
  frequenceSelected,
  siteSelected,
  variableSelected
)
}
\arguments{
\item{db}{data.table from mod_accessdata}

\item{dayNightSelected}{String of day, night or all the day selected in application ("day","night","day/night")}

\item{frequenceSelected}{: String of frequency (ex. "30 min", "hour", "day", "month")}

\item{siteSelected}{String of site/station code selected in application (ex. "lgt/ec1" or c("lgt/ec1","lgt/bm1"))}

\item{variableSelected}{String of variable code selected in application (ex. "FC" or c("FC","FCH4"))}
}
\value{
data.table with average of values by frequenceSelected
}
\description{
Calculate average of variable and site/station selected by frequency selected in application.
}
