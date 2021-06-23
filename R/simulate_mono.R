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
#' @param branch Integer indicating whether branching may occur and how
#' convergence to the equilibrium is assessed: 0, branching is not allowed;
#' 1, branching is allowed and equilibrium is reached when the selection
#' gradient is close enough to zero; 2, branching happens when two mutants away
#' by \code{sigma} units can mutually invade each other.
#' @param tol The tolerance when assessing if the selection gradient is close
#' enough to zero and an equilibrium has been reached (if \code{branch = 1}).
#'
#' @details See \code{?simulate}.
#'
#' There are two ways to assess whether a branching point has been reached.
#' One (\code{branch = 1}) is to evaluate the curvature of the fitness function
#' when the selection gradient becomes zero (i.e. at a singularity, see
#' \code{?is_stable}). Another way (\code{branch = 2}) is to evaluate the
#' mutual invasibility of the current resident and a mutant that is some
#' mutationally reasonable phenotypic distance away (e.g. one mutational
#' standard deviation \code{sigma} away). If each of the two (when taken as
#' a resident) can be invaded by the other (taken as a mutant, see
#' \code{?get_lambda}) then a stable polymorphism can persist and we assume
#' that branching occurs. In practice, the branching point may be reached
#' quicker with \code{branch = 2} as it does not need to wait for the gradient
#' to completely converge to zero (within the limits imposed by \code{tol},
#' which can also be rather arbitrary).
#'
#' @return A tibble containing the trait value of the population at each time
#' point
#'
#' @note The argument \code{branch} will be reverted to 1 if the selection
#' gradient reaches the value of exactly zero, even if \code{branch = 2} was
#' set in the function call (this is to ensure symmetry of the branching event).
#'
#' @seealso \code{find_equilibrium}, \code{get_gradient}, \code{get_lambda}
#'
#' @examples
#'
#' pars <- get_default_pars()
#' simulate_mono(-1, 10, pars, init = rep(1000, 2))
#'
#' @export

# Function to simulate evolution
simulate_mono <- function(
  xstart, ntimes, pars, init, mu = 0.01, sigma = 0.1, branch = 1,
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

    # Type I branching if the gradient is exactly zero
    if (G == 0) branch <- 1

    # Break if we have reached a branching point, if needed
    if (branch == 1 & G < tol & G > -tol & !is_stable(x, pars, init)) break

    if (branch == 2) {

      # What is the sign of the fitness gradient?
      direction <- sign(G)

      # Pick a mutant one standard deviation away
      xmut <- x + direction * sigma

      # Can the mutant and the resident mutually invade each other?
      is_invaded <- get_lambda(xmut, x, pars, N, fast = TRUE) > 1
      invades <- get_lambda(x, xmut, pars, init, fast = FALSE) > 1

      # If so...
      if (is_invaded & invades) {

        # Assume the singularity is in the middle of the two
        x <- 0.5 * (x + xmut)

        # Replace the last recorded value with the singularity
        xvalues[t] <- x
        break

      }
    }

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
