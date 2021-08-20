#' Find the within-habitat demographic equilibrium
#'
#' Solves the demographic equilibrium equation for the population size
#'
#' @param xres The resident trait value
#' @param pars An unevaluated parameter-list (e.g. as returned by
#' \code{get_default_pars})
#'
#' @details This function uses an analytical solution to the equation, which
#' is the greatest root of a second degree polynomial
#'
#' @return A scalar, the equilibrium population size
#'
#' @examples
#'
#' pars <- get_default_pars()
#' find_equilibrium_within(0, pars)
#'
#' @seealso \code{find_equilibrium}, \code{find_equilibrium_burnin},
#' \code{find_equilibrium_di}
#'
#' @export

# Function to find the demographic equilibrium
find_equilibrium_within <- function(xres, pars) {

  # Of the resident population
  x <- xres

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # Model setup
  model <- get_model_within()

  for (i in 1:2) eval(model[[i]])

  # Compute demographic equilibrium
  denom <- 2 * d * w1 * w2
  num <- ((d * omicron - (1 + h) * iota * w2) * w1 + d * omicron * w2)^2
  num <- num + 4 * d * omicron * w1 * w2 * (iota * (w1 + h * w2) - d * omicron)
  num <- sqrt(num)
  num <- num + (1 + h) * iota * w1 * w2
  num <- num - d * omicron * (w1 + w2)

  N <- num / denom

  return(N)

}
