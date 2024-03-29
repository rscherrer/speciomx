% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_equilibrium_within.R
\name{find_equilibrium_within}
\alias{find_equilibrium_within}
\title{Find the within-habitat demographic equilibrium}
\usage{
find_equilibrium_within(xres, pars)
}
\arguments{
\item{xres}{The resident trait value}

\item{pars}{An unevaluated parameter-list (e.g. as returned by
\code{get_default_pars})}
}
\value{
A scalar, the equilibrium population size
}
\description{
Solves the demographic equilibrium equation for the population size
}
\details{
This function uses an analytical solution to the equation, which
is the greatest root of a second degree polynomial
}
\examples{

pars <- get_default_pars()
find_equilibrium_within(0, pars)

}
\seealso{
\code{find_equilibrium}, \code{find_equilibrium_burnin},
\code{find_equilibrium_di}
}
