% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Plot state accidents}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{a number of a US state either as a number or character string.
If the estate number provided is not a valid state number the function throws an error.}

\item{year}{four digit year either as a number or character string}
}
\value{
this function doesn't return anything. A map is drawn.
}
\description{
Draws a map with all the accidents in an specific USA state and year. If there
are no accidents for the data selected the function doesn't plot anything
and a message is shown in the console.
}
\examples{

\dontrun{
fars_map_state(20, 2013)
}

}

