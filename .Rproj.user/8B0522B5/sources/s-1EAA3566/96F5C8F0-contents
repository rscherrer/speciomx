#' Evaluate convergence stability
#'
#' Assesses whether an equilibrium trait value is convergent-stable, or
#' attainable.
#'
#' @param xeq Some equilibrium trait value
#' @param pars An unevaluated parameter-list (e.g. as returned by
#' \code{get_default_pars})
#' @param init A vector of two starting values for solving of the demographic
#' equilibrium
#' @param step How far from \code{xeq} the selection gradient should be computed
#'
#' @details This functions computes the selection gradient on both sides of the
#' equilibrium \code{xeq} and looks at the sign of the difference between the
#' two. If the selection gradient goes down at \code{xeq} the equilibrium is
#' convergent, otherwise it is divergent.
#'
#' @return A boolean
#'
#' @seealso \code{is_stable}
#'
#' @examples
#'
#' pars <- get_default_pars()
#' is_convergent(0, pars, init = rep(1000, 2))
#'
#' @export

# Convergence stability criterion
is_convergent <- function(xeq, pars, init, step = 0.0001) {

  # Compute the selection gradient around the equilibrium
  gdt_before <- get_gradient(xeq - step, pars, init)
  gdt_after <- get_gradient(xeq + step, pars, init)

  # Compute the difference
  delta <- gdt_after - gdt_before

  # Is the equilibrium convergence-stable?
  delta < 0

}
