% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recodeChar.r
\name{recodeChar}
\alias{recodeChar}
\title{Query Data ILOSTAT}
\usage{
recodeChar(Str)
}
\arguments{
\item{Str}{a character vector with UTF-8 encoding errors.}
}
\description{
fix utf-8 encoding bug
}
\details{
Helper function to efficiently query data from ILOSTAT SDMX API.
}
\examples{
################################# use to identify correct 'DSD'		 
test <- c("emquÃªte", "MÃ©nage")
recodeChar(test)	
}
\author{
ILO bescond
}
\keyword{ILO,}
\keyword{R}
\keyword{SDMX,}
