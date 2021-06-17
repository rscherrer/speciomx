#' Dimorphic selection gradients
#'
#' Computes the selection gradients of the two morphs after branching has
#' occurred.
#'
#' @param xres1 The resident trait value in the first morph
#' @param xres2 The resident trait value in the second morph
#' @param pars An unevaluated parameter-list (e.g. as returned by
#' \code{get_default_pars})
#' @param init A vector of four starting values for solving of the demographic
#' equilibrium
#'
#' @details The functions uses \code{find_equilibrium_di} to compute the
#' equilibrium population sizes (in the two habitats and for the two morphs)
#' given the resident trait values, which are needed to compute the gradients.
#'
#' @return A vector of two selection gradients, one for each morph
#'
#' @seealso \code{find_equilibrium_di}
#'
#' @examples
#'
#' pars <- get_default_pars()
#' get_gradient_di(-0.001, 0.001, pars, init = rep(1000, 4))
#'
#' @export

# Function to compute the dimorphic selection gradient
get_gradient_di <- function(xres1, xres2, pars, init) {

  # Evaluation is at the resident
  x1 <- xres1
  x2 <- xres2

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # Find the demographic equilibrium
  N <- find_equilibrium_di(xres1, xres2, pars, init = init)

  # Separate population sizes for the two species
  N1 <- N[1:2]
  N2 <- N[3:4]

  # Evaluate the model at equilibrium
  model <- get_model_di()
  for (i in seq(model)) eval(model[[i]])

  # List of species-level values
  l <- list(
    x = c(x1, x2), # trait value
    w1 = c(w11, w12), # attack rate on resource 1
    w2 = c(w21, w22), # attack rate on resource 2
    r1 = c(r11, r12), # growth rate in habitat 1
    r2 = c(r21, r22) # growth rate in habitat 2
  )

  # For each species...
  G <- purrr::pmap_dbl(l, function(x, w1, w2, r1, r2) {

    # Derivatives of the attack rates
    dw1 <- -2 * s * w0 / psi * (x + psi) * w1
    dw2 <- -2 * s * w0 / psi * (x - psi) * w2

    # Derivatives of the reproductive success
    dW1 <- R11 * dw1 + R21 * dw2
    dW2 <- R12 * dw1 + R22 * dw2

    # Return the selection gradient
    denom <- (1 - r2 + m * r2)^2 + m^2 * r1 * r2
    num <- (
      1 - m - (2 - 4 * m + m^2) * r2 + (1 - 3 * m + 2 * m^2) * r2^2
    ) * dW1 + m^2 * r1 * dW2
    num / denom

  })

  return(G)

}
