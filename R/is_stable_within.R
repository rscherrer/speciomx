#' Evaluate within-habitat evolutionary stability
#'
#' Assesses whether an equilibrium trait value is evolutionarily stable, or
#' invasion-proof.
#'
#' @param xeq Some equilibrium trait value
#' @param pars An unevaluated parameter-list (e.g. as returned by
#' \code{get_default_pars})
#' @param value Whether to return the actual value of the curvature of the
#' fitness function (defaults to \code{FALSE}, i.e. only returns whether this
#' value is negative)
#'
#' @details This functions computes an expression for the second derivative
#' of the invasion fitness function with respect to the mutant trait value,
#' evaluated at the resident trait value at equilibrium \code{xeq}. If this
#' value is negative then the equilibrium is a fitness maximum, i.e. it is
#' evolutionarily stable. It is unstable otherwise.
#'
#' @return A boolean
#'
#' @seealso \code{is_convergent_within}, \code{is_stable}
#'
#' @examples
#'
#' pars <- get_default_pars()
#' is_stable_within(0, pars)
#'
#' @export

# Evolutionary stability criterion
is_stable_within <- function(xeq, pars, value = FALSE) {

  # Evaluation is at equilibrium
  xres <- xeq
  x <- xres

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # Find the demographic equilibrium
  N <- find_equilibrium_within(xeq, pars)

  # Evaluate the model at equilibrium
  model <- get_model_within()
  for (i in seq(model)) eval(model[[i]])

  # Derivatives of the attack rates
  dw1 <- -2 * s * (x + psi) * w1 / psi^2
  dw2 <- -2 * s * (x - psi) * w2 / psi^2

  # Second derivatives of the attack rates
  ddw1 <- -2 * s * (w1 + (x + psi) * dw1) / psi^2
  ddw2 <- -2 * s * (w2 + (x - psi) * dw2) / psi^2

  # Second derivative of the reproductive success
  ddW <- R1 * ddw1 + R2 * ddw2

  # Invasibility
  inv <- ddW - a * W

  if (value) return(inv)

  return(inv < 0)

}
