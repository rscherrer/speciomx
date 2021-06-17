#' Find the dimorphic demographic equilibrium
#'
#' Solves the demographic equilibrium equation for the population sizes in the
#' two morphs and the two habitats, after branching has occurred, given
#' parameter values.
#'
#' @param xres1 The resident trait value in the first morph
#' @param xres2 The resident trait value in the second morph
#' @param pars An unevaluated parameter-list (e.g. as returned by
#' \code{get_default_pars})
#' @param init A vector of four starting values for solving of the demographic
#' equilibrium
#'
#' @details This function uses a multivariate numerical root finding algorithm
#' (\code{?pracma::fsolve}) to find solutions to the equation.
#'
#' @return A vector of four equilibrium population sizes: morph 1 in habitat 1,
#' morph 1 in habitat 2, morph 2 in habitat 1 and morph 2 in habitat 2.
#'
#' @examples
#'
#' pars <- get_default_pars()
#' find_equilibrium_di(-0.001, 0.001, pars, init = rep(1000, 4))
#'
#' @export

# Function to find the dimorphic demographic equilibrium
find_equilibrium_di <- function(xres1, xres2, pars, init) {

  # Of the resident population
  x1 <- xres1
  x2 <- xres2

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # Model setup
  model <- get_model_di()

  # System of equations to solve
  f <- function(N) {

    # Decompose the vector of population sizes into the two species
    N1 <- N[1:2]
    N2 <- N[3:4]

    # Evaluate the model
    for (i in seq(model)) eval(model[[i]])

    # Assemble the two transition matrices into one
    Zeros <- matrix(0, 2, 2)
    Lambda <- rbind(cbind(Lambda1, Zeros), cbind(Zeros, Lambda2))

    return(N - Lambda %*% N)

  }

  # Solve the demographic equilibrium
  N <- pracma::fsolve(f, init)$x

  return(N)

}
