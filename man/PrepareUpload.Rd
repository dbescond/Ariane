% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PrepareUpload.r
\name{PrepareUpload}
\alias{PrepareUpload}
\title{Prepare dataframe in csv upload format}
\usage{
PrepareUpload(X, DELETE = FALSE, LABEL = FALSE)
}
\arguments{
\item{X}{data frame.}

\item{DELETE}{in deletion format, default is FALSE.}

\item{LABEL}{add label to the notes, default is FALSE.}
}
\description{
fix utf-8 encoding bug
}
\details{
Helper function to efficiently query data from ILOSTAT SDMX API.
}
\author{
ILO bescond
}
\keyword{ILO,}
\keyword{R}
