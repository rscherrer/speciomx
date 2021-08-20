#' Evaluate within-habitat convergence stability
#'
#' Assesses whether an equilibrium trait value is convergent-stable, or
#' attainable.
#'
#' @param xeq Some equilibrium trait value
#' @param pars An unevaluated parameter-list (e.g. as returned by
#' \code{get_default_pars})
#' @param step How far from \code{xeq} the selection gradient should be computed
#'
#' @details This functions computes the selection gradient on both sides of the
#' equilibrium \code{xeq} and looks at the sign of the difference between the
#' two. If the selection gradient goes down at \code{xeq} the equilibrium is
#' convergent, otherwise it is divergent.
#'
#' @return A boolean
#'
#' @seealso \code{is_stable_within}, \code{is_convergent}
#'
#' @examples
#'
#' pars <- get_default_pars()
#' is_convergent_within(0, pars)
#'
#' @export

# Convergence stability criterion
is_convergent_within <- function(xeq, pars, step = 0.0001) {

  # Compute the selection gradient around the equilibrium
  gdt_before <- get_gradient_within(xeq - step, pars)
  gdt_after <- get_gradient_within(xeq + step, pars)

  # Compute the difference
  delta <- gdt_after - gdt_before

  # Is the equilibrium convergence-stable?
  delta < 0

}

