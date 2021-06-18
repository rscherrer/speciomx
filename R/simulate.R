#' Simulate trait evolution
#'
#' Simulates the evolution of the ecological trait through time by computing
#' the trait value at the next generation as a function of the selection
#' gradient computed at the present generation.
#'
#' @param xstart Starting trait value
#' @param ntimes The number of time steps to simulate for
#' @param pars An unevaluated parameter-list (e.g. as returned by
#' \code{get_default_pars})
#' @param init A vector of four starting values for solving of the demographic
#' equilibrium
#' @param mu The mutation rate of the trait (per time step)
#' @param sigma The (standard deviation of) mutational step size upon mutation
#' @param tol The tolerance when assessing if the selection gradient is close
#' enough to zero and an equilibrium has been reached
#' @param dodge How much to displace each morph from the branching point once
#' branching has occurred
#' @param burnin The number of time steps in the burn-in period
#'
#' @details
#'
#' This function simulates evolution in three phases: (1) the burn-in
#' phase (where only one habitat and one resource are available), (2)
#' monomorphic evolution (in two habitats and with two resources) and (3)
#' dimorphic evolution once a branching point is reached (if it is ever
#' reached).
#'
#' At each time step the equilibrium population sizes must be solved
#' numerically, using the values in \code{init} as starting points. This is
#' done using \code{find_equilibrium_burnin}, \code{find_equilibrium} or
#' \code{find_equilibrium_di}, depending on the phase we are in. Before
#' branching there are only two population sizes (one for each habitat) and so
#' only the two first values in \code{init} are used. After branching, all
#' values are used, the first two corresponding to habitat 1 and 2,
#' respectively, for the first morph (the one with a more negative trait value),
#' and the last two for habitat 1 and 2, respectively, for the second morph.
#'
#' Parameters \code{mu} and \code{sigma} modulate the response to selection
#' at each time step (and so the rate of evolution). \code{mu} should be
#' interpreted as the probability that the trait mutates in a single time
#' step (\code{mu = 0.01} would be reasonable for a trait encoded by 100 loci
#' and with a 0.0001 mutation rate per locus). \code{sigma} is, strictly
#' speaking, the standard deviation of the distribution of mutational effects.
#'
#' An equilibrium is reached when the selection gradient falls within \code{tol} units
#' around zero. If this equilibrium is a branching point (assessed using
#' \code{is_stable}) then the dimorphic phase starts and the two morphs are
#' displaced by \code{dodge} units before undergoing separate evolutionary
#' trajectories with two different selection gradients (but still influencing
#' each other).
#'
#' @return A tibble containing the trait values in the two morphs at each time
#' point (with burn-in time points returned as negative integers).
#'
#' @note The trait values of the two morphs are equal before a branching point
#' has been reached.
#'
#' @seealso \code{simulate_burnin}, \code{simulate_mono}, \code{simulate_di},
#' \code{is_stable}
#'
#' @examples
#'
#' pars <- get_default_pars()
#' simulate(-1, ntimes = 10, pars, init = rep(1000, 4))
#'
#' @export

# Function to simulate evolution
simulate <- function(
  xstart, ntimes, pars, init, mu = 0.01, sigma = 0.1, tol = 0.0001,
  dodge = 0.001, burnin = 0
) {

  # If the burn-in must be simulated...
  if (burnin > 0) {

    # Simulate the burn-in
    data_burnin <- simulate_burnin(xstart, burnin, pars, mu, sigma)

    # Give burn-in time points negative values
    data_burnin <- data_burnin %>% dplyr::mutate(time = time - dplyr::n() + 1)

    # Record the end point of the burn-in
    xstart <- dplyr::last(data_burnin$x)

    # Remove the end point of the burn-in (it is the start of the next phase)
    data_burnin <- data_burnin[-nrow(data_burnin),]

    # Reformat the burn-in so it can be appended later
    data_burnin <- data_burnin %>%
      dplyr::mutate(x2 = x) %>%
      dplyr::rename(x1 = "x")

  }

  # Monomorphic evolution until potential branching point
  data <- simulate_mono(xstart, ntimes, pars, init[1:2], mu, sigma, tol = tol)

  # Reformat the dataset
  data <- data %>% dplyr::mutate(x2 = x) %>% dplyr::rename(x1 = "x")

  # Number of time steps left
  ntimes <- ntimes - nrow(data) + 1

  # If there is simulation time left...
  if (ntimes > 0) {

    # The endpoint of monomorphic evolution is the new starting point
    xstart <- dplyr::last(data$x1)

    # Simulate dimorphic evolution after the branching point
    data_di <- simulate_di(xstart, ntimes, pars, init, mu, sigma, dodge)

    # The first time step of dimorphic evolution is the last one from before
    data_di <- data_di[-1,]

    # Assemble data before and after the branching point
    data <- purrr::map_dfr(list(data, data_di), ~ .x) %>%
      dplyr::mutate(time = seq(dplyr::n()) - 1)

  }

  # Add the burn-in data if needed
  if (burnin > 0) data <- purrr::map_dfr(list(data_burnin, data), ~ .x)

  return(data)

}
