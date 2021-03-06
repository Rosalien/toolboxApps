% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dependanciesData.R
\name{variableToVariableSimple}
\alias{variableToVariableSimple}
\title{variableToVariableSimple}
\usage{
variableToVariableSimple(x)
}
\arguments{
\item{x}{String of biometeorological variable code with (ex. "G_1_1_1" )}
}
\value{
String of variable code (ex. "G")
}
\description{
Transform biometeorological variable code into simple variable code (ex. "G_1_1_1" into "G")
}
\examples{
variableToVariableSimple("G_1_1_1")
}
