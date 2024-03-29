% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_convergent_within.R
\name{is_convergent_within}
\alias{is_convergent_within}
\title{Evaluate within-habitat convergence stability}
\usage{
is_convergent_within(xeq, pars, step = 1e-04)
}
\arguments{
\item{xeq}{Some equilibrium trait value}

\item{pars}{An unevaluated parameter-list (e.g. as returned by
\code{get_default_pars})}

\item{step}{How far from \code{xeq} the selection gradient should be computed}
}
\value{
A boolean
}
\description{
Assesses whether an equilibrium trait value is convergent-stable, or
attainable.
}
\details{
This functions computes the selection gradient on both sides of the
equilibrium \code{xeq} and looks at the sign of the difference between the
two. If the selection gradient goes down at \code{xeq} the equilibrium is
convergent, otherwise it is divergent.
}
\examples{

pars <- get_default_pars()
is_convergent_within(0, pars)

}
\seealso{
\code{is_stable_within}, \code{is_convergent}
}
