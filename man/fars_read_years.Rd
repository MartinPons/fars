% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{List months and years of every accident from FARS}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{vector of four digit years either as numbers or as character strings.
The years must correspond to one file previously downloaded in the current
working directory, otherwise the function returns NULL}
}
\value{
a list of data.frames. Each element of the list references an specific year
and contains a log from accidents including only year and month information
}
\description{
Creates a list of data.frames with month and year of accidents
loaded from a collection of files from FARS.
}
\examples{

\dontrun{
fars_read_years(c(2013, 2014))
fars_read_years(c("2014"))
}

}

