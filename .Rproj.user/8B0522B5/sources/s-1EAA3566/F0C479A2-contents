#' Simulate burn-in
#'
#' Simulates trait evolution during the burn-in period.
#'
#' @param xstart Starting trait value
#' @param ntimes The number of time steps to simulate for
#' @param pars An unevaluated parameter-list (e.g. as returned by
#' \code{get_default_pars})
#' @param mu The mutation rate of the trait (per time step)
#' @param sigma The (standard deviation of) mutational step size upon mutation
#'
#' @details See \code{?simulate}.
#'
#' @return A tibble containing the trait value of the population at each time
#' point
#'
#' @seealso \code{find_equilibrium_burnin}, \code{get_gradient_burnin}
#'
#' @examples
#'
#' pars <- get_default_pars()
#' simulate_burnin(0, 10, pars)
#'
#' @export

# Function to simulate the burn-in
simulate_burnin <- function(xstart, ntimes, pars, mu = 0.01, sigma = 0.1) {

  # Initialize
  x <- xstart
  xvalues <- rep(NA, ntimes + 1)
  xvalues[1] <- x

  # At each time point...
  for (t in 1:ntimes) {

    # Compute the selection gradient and demographic equilibrium
    G <- get_gradient_burnin(x, pars)
    N <- find_equilibrium_burnin(x, pars)

    # Evolve
    kappa <- 0.5 * N * sigma^2 * mu
    dx <- kappa * G
    x <- x + dx

    # Record
    xvalues[t + 1] <- x

  }

  # Assemble into a table
  return(tibble::tibble(time = seq(xvalues) - 1, x = xvalues))

}
