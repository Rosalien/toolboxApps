% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dependanciesQuery.R
\name{caracdata}
\alias{caracdata}
\title{caracdata}
\usage{
caracdata(pool, language)
}
\arguments{
\item{pool}{data base configuration (from confConnexion function)}

\item{language}{String of language ("fr" or "en")}
}
\value{
data.table of public.carac_data_sensor_method_prod
}
\description{
Sql query to extrat characteristics of variables and site for all datatype
}
