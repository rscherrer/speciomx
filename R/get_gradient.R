#' Selection gradient
#'
#' Computes the selection gradient given a resident value and model parameters.
#'
#' @param xres The resident trait value
#' @param pars An unevaluated parameter-list (e.g. as returned by
#' \code{get_default_pars})
#' @param init A vector of two starting values for solving of the demographic
#' equilibrium
#'
#' @details The functions uses \code{find_equilibrium} to compute the
#' equilibrium population sizes (in the two habitats) given the resident trait
#' value, which are needed to compute the gradient.
#'
#' @return The selection gradient
#'
#' @seealso \code{find_equilibrium}
#'
#' @examples
#'
#' pars <- get_default_pars()
#' get_gradient(0, pars, init = rep(1000, 2))
#'
#' @export

# Function to compute the selection gradient
get_gradient <- function(xres, pars, init) {

  # Evaluation is at the resident
  x <- xres

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # Find the demographic equilibrium
  N <- find_equilibrium(xres, pars, init = init)

  # Evaluate the model at equilibrium
  model <- get_model()
  for (i in seq(model)) eval(model[[i]])

  # Derivatives of the attack rates
  dw1 <- -2 * s * w0 / psi * (x + psi) * w1
  dw2 <- -2 * s * w0 / psi * (x - psi) * w2

  # Derivatives of the reproductive success
  dW1 <- R11 * dw1 + R21 * dw2
  dW2 <- R12 * dw1 + R22 * dw2

  # Return the selection gradient
  denom <- (1 - r2 + m * r2)^2 + m^2 * r1 * r2
  num <- (1 - m - (2 - 4 * m + m^2) * r2 + (1 - 3 * m + 2 * m^2) * r2^2) * dW1 +
    m^2 * r1 * dW2
  num / denom

}
