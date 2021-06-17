#' Find the demographic equilibrium
#'
#' Solves the demographic equilibrium equation for the population sizes in the
#' two habitats
#'
#' @param xres The resident trait value
#' @param pars An unevaluated parameter-list (e.g. as returned by
#' \code{get_default_pars})
#' @param init A vector of two starting values for solving of the demographic
#' equilibrium
#'
#' @details This function uses a multivariate numerical root finding algorithm
#' (\code{?pracma::fsolve}) to find solutions to the equation.
#'
#' @return A vector of two equilibrium population sizes, one for each habitat.
#'
#' @examples
#'
#' pars <- get_default_pars()
#' find_equilibrium(0, pars, init = rep(1000, 2))
#'
#' @export

# Function to find the demographic equilibrium
find_equilibrium <- function(xres, pars, init) {

  # Of the resident population
  x <- xres

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # Model setup
  model <- get_model()

  # System of equations to solve
  f <- function(N) {

    for (i in seq(model)) eval(model[[i]])
    return(N - Lambda %*% N)

  }

  # Solve the demographic equilibrium
  N <- pracma::fsolve(f, init)$x

  return(N)

}
