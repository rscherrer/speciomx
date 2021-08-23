#' Within-habitat selection gradient
#'
#' Computes the selection gradient given a resident value and model parameters.
#'
#' @param xres The resident trait value
#' @param pars An unevaluated parameter-list (e.g. as returned by
#' \code{get_default_pars})
#'
#' @details The functions uses \code{find_equilibrium_within} to compute the
#' equilibrium population size given the resident trait value
#'
#' @return The selection gradient
#'
#' @seealso \code{find_equilibrium_within}, \code{get_gradient}
#'
#' @examples
#'
#' pars <- get_default_pars()
#' get_gradient_within(0, pars)
#'
#' @export

# Function to compute the selection gradient
get_gradient_within <- function(xres, pars) {

  # Evaluation is at the resident
  x <- xres

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # Find the demographic equilibrium
  N <- find_equilibrium_within(xres, pars)

  # Evaluate the model at equilibrium
  model <- get_model_within()
  for (i in seq(model)) eval(model[[i]])

  # Return the selection gradient
  gdt <- -2 * s * ((x + psi) * w1 * R1 + (x - psi) * w2 * R2) / psi^2

  return(gdt)

}
