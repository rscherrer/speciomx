#' Invasion fitness
#'
#' Computes the invasion fitness of a mutant in a resident population, given
#' trait values and model parameters.
#'
#' @param x The mutant trait value
#' @param xres The resident trait value
#' @param pars An unevaluated parameter-list (e.g. as returned by
#' \code{get_default_pars})
#' @param init A vector of two starting values for solving of the demographic
#' equilibrium
#' @param fast See the "Details" section
#'
#' @details
#'
#' This function computes an expression for the leading eigenvalue of
#' the transition matrix of the model, which is the invasion fitness of the
#' mutant.
#'
#' The function will find the demographic equilibrium of the resident (passing
#' \code{init} to \code{find_equilibrium}) only if \code{fast = FALSE}.
#' Otherwise, \code{init} is interpreted as a vector of equilibrium population
#' sizes (useful when computing the invasion fitness of many mutants in the
#' same resident population, e.g. in \code{plot_pip}).
#'
#' @return The invasion fitness of the mutant given the resident
#'
#' @seealso \code{plot_pip}, \code{find_equilibrium}
#'
#' @examples
#'
#' pars <- get_default_pars()
#' get_lambda(0.01, 0, pars, init = rep(1000, 2))
#'
#' @export

# Function to compute the invasion fitness
get_lambda <- function(x, xres, pars, init, fast = FALSE) {

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # Find the demographic equilibrium
  if (fast) N <- init else N <- find_equilibrium(xres, pars, init)

  # Evaluate the model at equilibrium
  model <- get_model()
  for (i in seq(model)) eval(model[[i]])

  # Compute invasion fitness
  lambda <- (1 - m)^2 * (r1 + r2)^2 - 4 * (1 - 2 * m) * r1 * r2
  lambda <- sqrt(lambda)
  lambda <- lambda + (1 - m) * (r1 + r2)
  lambda <- 0.5 * lambda

  return(lambda)

}
