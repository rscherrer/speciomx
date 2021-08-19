#' Evaluate evolutionary stability
#'
#' Assesses whether an equilibrium trait value is evolutionarily stable, or
#' invasion-proof.
#'
#' @param xeq Some equilibrium trait value
#' @param pars An unevaluated parameter-list (e.g. as returned by
#' \code{get_default_pars})
#' @param init A vector of two starting values for solving of the demographic
#' equilibrium
#' @param value Whether to return the actual value of the curvature of the
#' fitness function (defaults to \code{FALSE}, i.e. only returns whether this
#' value is positive or negative)
#'
#' @details This functions computes an expression for the second derivative
#' of the invasion fitness function with respect to the mutant trait value,
#' evaluated at the resident trait value at equilibrium \code{xeq}. If this
#' value is negative then the equilibrium is a fitness maximum, i.e. it is
#' evolutionarily stable. It is unstable otherwise.
#'
#' @return A boolean
#'
#' @seealso \code{is_convergent}
#'
#' @examples
#'
#' pars <- get_default_pars()
#' is_stable(0, pars, init = rep(1000, 2))
#'
#' @export

# Evolutionary stability criterion
is_stable <- function(xeq, pars, init, value = FALSE) {

  # Evaluation is at equilibrium
  xres <- xeq
  x <- xres

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # Find the demographic equilibrium
  N <- find_equilibrium(xeq, pars, init)

  # Evaluate the model at equilibrium
  model <- get_model()
  for (i in seq(model)) eval(model[[i]])

  # Derivatives of the attack rates
  dw1 <- -2 * s * w0 / psi * (x + psi) * w1
  dw2 <- -2 * s * w0 / psi * (x - psi) * w2

  # Derivatives of the reproductive success
  dW1 <- R11 * dw1 + R21 * dw2
  dW2 <- R12 * dw1 + R22 * dw2

  # Second derivatives of the attack rates
  ddw1 <- -2 * s * w0 / psi * (w1 + (x + psi) * dw1)
  ddw2 <- -2 * s * w0 / psi * (w2 + (x - psi) * dw2)

  # Second derivatives of the reproductive success
  ddW1 <- R11 * ddw1 + R21 * ddw2
  ddW2 <- R12 * ddw1 + R22 * ddw2

  # Compute the second derivative of the fitness function
  denom <- 1 + (m - 1) * r2 + m^2 * r1 * r2 + (m - 1)^2 * r2^2
  num <- (m - 1 + (2 - 4 * m + m^2) * r2 - (1 - 3 * m + 2 * m^2) * r2^2) * W1
  num <- num - m^2 * r1 * W2
  num <- a * num
  num <- num + (1 - m - (2 - 4 * m + m^2) * r2 + (1 - 3 * m + 2 * m^2) * r2^2) * ddW1
  num <- num + 2 * (2 * m - 1) * (1 + (m - 1) * r2) * dW1 * dW2
  num <- num + m^2 * r1 * ddW2

  if (value) return(num / denom)

  # Is the equilibrium stable?
  num / denom < 0

}
