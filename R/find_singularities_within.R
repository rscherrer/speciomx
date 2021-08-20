#' Find within-habitat evolutionary singularities
#'
#' Finds trait values for which the selection gradient is zero.
#'
#' @param from,to The bounds of the range of trait values to explore
#' @param pars An unevaluated parameter-list (e.g. as returned by
#' \code{get_default_pars})
#' @param ... Additional arguments to be passed to \code{find_roots}
#'
#' @details This function uses a numerical root finding algorithm to find the
#' roots of the fitness gradient within a range of trait values
#'
#' @return A vector of singularities (the output of \code{find_roots})
#'
#' @seealso \code{get_gradient_within}, \code{find_roots},
#' \code{find_singularities}
#'
#' @examples
#'
#' pars <- get_default_pars()
#' find_singularities_within(-1, 1, pars)
#'
#' @export

# Function to find the roots of the selection gradient
find_singularities_within <- function(from, to, pars, ...) {

  # Equation for which to find the root(s)
  f <- function(x) get_gradient_within(x, pars)

  # Find the roots of the selection gradient
  find_roots(f, from, to, ...)

}
