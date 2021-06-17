#' Find the burn-in demographic equilibrium
#'
#' Solves the burn-in demographic equilibrium equation for the population size,
#' given parameter values.
#'
#' @param xres The resident trait value
#' @param pars An unevaluated parameter-list (e.g. as returned by
#' \code{get_default_pars})
#'
#' @details This function uses an analytical expression for the non-extinct
#' solution to the equation (the other solution being when population size
#' is zero).
#'
#' @return The equilibrium population size
#'
#' @examples
#'
#' pars <- get_default_pars()
#' find_equilibrium_burnin(0, pars)
#'
#' @export

# Function to find the (non-extinct) burn-in demographic equilibrium
find_equilibrium_burnin <- function(xres, pars) {

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # Attack rate of the resident
  w <- w0 * exp(-s * (xres + psi)^2 / psi^2)

  # Compute equilibrium population size
  num <- w * iota - d * omicron
  denom <- d * w
  num / denom

}
