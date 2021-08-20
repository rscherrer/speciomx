#' Within-habitat model set-up
#'
#' Generates an unevaluated list of model variable statements
#'
#' @return A list of expressions to run to evaluate the model
#'
#' @note Use \code{eval()} on an expression in the list to execute it.
#' Beware that the evaluation of the model assumes that parameters and their
#' values are present in the environment (see e.g. \code{?get_default_pars}) and
#' that variables \code{x} and \code{xres} exist.
#'
#' @seealso \code{get_model}, \code{get_model_di}
#'
#' @examples
#'
#' get_model_within()
#'
#' @export

# Set up the model, not evaluated
get_model_within <- function() {

  # x: mutant trait value
  # xres: resident trait value

  alist(

    # Attack rates of the mutant on each resource
    w1 <- w0 * exp(-s * (x + psi)^2 / psi^2),
    w2 <- w0 * exp(-s * (x - psi)^2 / psi^2),

    # Attack rates of the resident on each resource
    w1res <- w0 * exp(-s * (xres + psi)^2 / psi^2),
    w2res <- w0 * exp(-s * (xres - psi)^2 / psi^2),

    # Equilibrium resource concentrations of each resource
    R1 <- iota / (omicron + N * w1res),
    R2 <- h * iota / (omicron + N * w2res),

    # Mutant reproductive success
    W <- w1 * R1 + w2 * R2,

    # Attractiveness of the mutant to the resident
    A <- exp(-a * (x - xres)^2),

    # Geometric growth rate of the mutant
    r <- 1 - d + 0.5 * W * (1 + A)

  )

}
