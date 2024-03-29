% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_sliding_windows.R
\name{get_sliding_windows}
\alias{get_sliding_windows}
\title{Generate sliding windows}
\usage{
get_sliding_windows(from, to, n, size, intervals = TRUE)
}
\arguments{
\item{from, to}{The bounds of the range to slide across}

\item{n}{The number of bins in which to split the range}

\item{size}{The size of the sliding window, in number of bins}

\item{intervals}{See the "Value" section}
}
\value{
If \code{intervals = TRUE}, a list of window ranges. Otherwise,
a list of two vectors: a vector of lower bounds and a vector of upper bounds
for each window.
}
\description{
Generate sliding windows
}
\examples{

get_sliding_windows(0, 10, 10, 2)

}
