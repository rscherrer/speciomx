#' Simulate monomorphic evolution
#'
#' Simulates trait evolution in a monomorphic population.
#'
#' @param xstart Starting trait value
#' @param ntimes The number of time steps to simulate for
#' @param pars An unevaluated parameter-list (e.g. as returned by
#' \code{get_default_pars})
#' @param init A vector of two starting values for solving of the demographic
#' equilibrium
#' @param mu The mutation rate of the trait (per time step)
#' @param sigma The (standard deviation of) mutational step size upon mutation
#' @param branch Whether to end the simulation when a branching point has
#' been reached
#' @param tol The tolerance when assessing if the selection gradient is close
#' enough to zero and an equilibrium has been reached
#'
#' @details See \code{?simulate}.
#'
#' @return A tibble containing the trait value of the population at each time
#' point
#'
#' @seealso \code{find_equilibrium}, \code{get_gradient}
#'
#' @examples
#'
#' pars <- get_default_pars()
#' simulate_mono(-1, 10, pars, init = rep(1000, 2))
#'
#' @export

# Function to simulate evolution
simulate_mono <- function(
  xstart, ntimes, pars, init, mu = 0.01, sigma = 0.1, branch = TRUE,
  tol = 0.0001
) {

  # Initialize
  x <- xstart
  xvalues <- rep(NA, ntimes + 1)
  xvalues[1] <- x

  # At each time point...
  for (t in 1:ntimes) {

    # Compute the selection gradient and demographic equilibrium
    G <- get_gradient(x, pars, init)
    N <- find_equilibrium(x, pars, init)

    # Break if we have reached a branching point, if needed
    if (branch & G < tol & G > -tol & !is_stable(x, pars, init)) break

    # Evolve
    kappa <- 0.5 * sum(N) * sigma^2 * mu
    dx <- kappa * G
    x <- x + dx

    # Record
    xvalues[t + 1] <- x

  }

  xvalues <- xvalues[!is.na(xvalues)]

  # Assemble into a table
  return(tibble::tibble(time = seq(xvalues) - 1, x = xvalues))

}
