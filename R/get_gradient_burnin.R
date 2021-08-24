#' Burn-in selection gradient
#'
#' Computes the selection gradient during the burn-in period given parameter
#' values.
#'
#' @param xres The resident trait value
#' @param pars An unevaluated parameter-list (e.g. as returned by
#' \code{get_default_pars})
#'
#' @details The functions uses \code{find_equilibrium_burnin} to compute the
#' equilibrium population size given the resident trait value, which is needed
#' to compute the gradient.
#'
#' @return The selection gradient
#'
#' @seealso \code{find_equilibrium_burnin}
#'
#' @examples
#'
#' pars <- get_default_pars()
#' get_gradient_burnin(0, pars)
#'
#' @export

# Function to compute the burn-in selection gradient
get_gradient_burnin <- function(xres, pars) {

  # Evaluation is at the resident
  x <- xres

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # Find the demographic equilibrium
  N <- find_equilibrium_burnin(xres, pars)

  # Attack rate of the resident
  w <- w0 * exp(-s * (xres + psi)^2 / psi^2)

  # Equilibrium resource concentration
  R <- iota / (omicron + N * w)

  # Derivative of the attack rate
  dw <- -2 * s * (xres + psi) * w / psi ^2

  # Selection gradient
  G <- dw * R

  return(G)

}
