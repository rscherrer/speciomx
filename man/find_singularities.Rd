% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_singularities.R
\name{find_singularities}
\alias{find_singularities}
\title{Find evolutionary singularities}
\usage{
find_singularities(from, to, pars, init, ...)
}
\arguments{
\item{from, to}{The bounds of the range of trait values to explore}

\item{pars}{An unevaluated parameter-list (e.g. as returned by
\code{get_default_pars})}

\item{init}{A vector of two starting values for solving of the demographic
equilibrium}

\item{...}{Additional arguments to be passed to \code{find_roots}}
}
\value{
A vector of singularities (the output of \code{find_roots})
}
\description{
Finds trait values for which the selection gradient is zero.
}
\details{
This function uses a numerical root finding algorithm to find the
roots of the fitness gradient within a range of trait values
}
\examples{

pars <- get_default_pars()
find_singularities(-1, 1, pars, init = rep(1000, 2))

}
\seealso{
\code{get_gradient}, \code{find_roots}
}
