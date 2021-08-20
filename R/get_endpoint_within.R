#' Predict within-habitat evolutionary endpoint
#'
#' Predicts the evolutionary equilibrium reached given a starting point and
#' some parameter values, and if this equilibrium is stable.
#'
#' @param xstart The starting resident trait value
#' @param pars An unevaluated parameter-list (e.g. as returned by
#' \code{get_default_pars})
#' @param from,to Arguments to be passed to \code{find_singularities_within}
#' (only the bound towards which the initial selection gradient is pointing is
#' considered)
#' @param step Argument to be passed to \code{is_convergent_within}
#' @param ... Optional extra arguments for \code{find_singularities_within}
#'
#' @details The function computes the selection gradient at the starting point
#' to know in which direction evolution is leading, then finds the closest
#' singularity in that direction.
#'
#' @return A data frame with the singularity, whether it is attainable and
#' whether it is evolutionarily stable. If a singularity is attainable and
#' stable, it is an evolutionarily stable strategy (labelled "ESS"). If it is
#' attainable and unstable, it is a branching point ("BP"). If it is
#' unattainable, it is a repeller.
#'
#' @seealso \code{get_gradient_within}, \code{find_singularities_within},
#' \code{is_stable_within}, \code{is_convergent_within}
#'
#' @examples
#'
#' pars <- get_default_pars()
#' get_endpoint_within(0, pars)
#'
#' @export

# Predict the endpoint of evolution given a starting point
get_endpoint_within <- function(
  xstart, pars, from = xstart - 10, to = xstart + 10, step = 1e-4, ...
) {

  # Note that this function is not designed to accommodate cyclical dynamics

  # In which direction are we going?
  gdt <- get_gradient_within(xstart, pars)

  if (gdt > 0) from <- xstart
  if (gdt < 0) to <- xstart

  # What are the singularities in that direction?
  sings <- find_singularities_within(from, to, pars, ...)

  # Distance to each of them
  distances <- abs(sings - xstart)

  # What is the closest singularity?
  xend <- sings[distances == min(distances)]

  # Is it attainable?
  is_convergent <- is_convergent_within(xend, pars, step)

  # Is is invasion-proof?
  is_stable <- is_stable_within(xend, pars)

  # Prepare output
  out <- tibble::tibble(
    xend = xend,
    is_convergent = is_convergent,
    is_stable = is_stable,
    type = ifelse(!is_convergent, "Repeller", ifelse(is_stable, "ESS", "BP"))
  )

  return(out)

}
