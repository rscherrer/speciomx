#' Simulate dimorphic evolution
#'
#' Simulates trait evolution after branching.
#'
#' @param xstart Starting trait value
#' @param ntimes The number of time steps to simulate for
#' @param pars An unevaluated parameter-list (e.g. as returned by
#' \code{get_default_pars})
#' @param init A vector of four starting values for solving of the demographic
#' equilibrium
#' @param mu The mutation rate of the trait (per time step)
#' @param sigma The (standard deviation of) mutational step size upon mutation
#' @param dodge How much to displace each morph from the branching point once
#' branching has occurred
#'
#' @details See \code{?simulate}.
#'
#' @return A tibble containing the trait values in the two morphs at each time
#' point.
#'
#' @seealso \code{find_equilibrium_di}, \code{get_gradient_di}
#'
#' @examples
#'
#' pars <- get_default_pars()
#' simulate_di(0, 10, pars, init = rep(1000, 4))
#'
#' @export

# Function to simulate evolution after branching
simulate_di <- function(
  xstart, ntimes, pars, init, mu = 0.01, sigma = 0.1, dodge = 0.001
) {

  # Dodge the starting value by a little amount to start two lineages
  x1 <- xstart - dodge
  x2 <- xstart + dodge

  # Initialize
  x1values <- x2values <- rep(NA, ntimes + 1)
  x1values[1] <- x1
  x2values[1] <- x2

  # At each time point...
  for (t in 1:ntimes) {

    # Compute the selection gradient and demographic equilibrium
    G <- get_gradient_di(x1, x2, pars, init)
    N <- find_equilibrium_di(x1, x2, pars, init)

    # Evolve the first species
    kappa1 <- 0.5 * sum(N[1:2]) * sigma^2 * mu
    dx1 <- kappa1 * G[1]
    x1 <- x1 + dx1

    # Evolve the second species
    kappa2 <- 0.5 * sum(N[3:4]) * sigma^2 * mu
    dx2 <- kappa2 * G[2]
    x2 <- x2 + dx2

    # Record
    x1values[t + 1] <- x1
    x2values[t + 1] <- x2

  }

  # Assemble into a table
  return(tibble::tibble(time = 0:ntimes, x1 = x1values, x2 = x2values))

}
