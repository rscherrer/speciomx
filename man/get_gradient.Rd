% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_gradient.R
\name{get_gradient}
\alias{get_gradient}
\title{Selection gradient}
\usage{
get_gradient(xres, pars, init)
}
\arguments{
\item{xres}{The resident trait value}

\item{pars}{An unevaluated parameter-list (e.g. as returned by
\code{get_default_pars})}

\item{init}{A vector of two starting values for solving of the demographic
equilibrium}
}
\value{
The selection gradient
}
\description{
Computes the selection gradient given a resident value and model parameters.
}
\details{
The functions uses \code{find_equilibrium} to compute the
equilibrium population sizes (in the two habitats) given the resident trait
value, which are needed to compute the gradient.
}
\examples{

pars <- get_default_pars()
get_gradient(0, pars, init = rep(1000, 2))

}
\seealso{
\code{find_equilibrium}
}
