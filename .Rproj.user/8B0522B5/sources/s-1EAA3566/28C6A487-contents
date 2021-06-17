#' Model set-up
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
#' @seealso \code{get_model_di}, \code{get_model_burnin}
#'
#' @examples
#'
#' get_model()
#'
#' @export

# Set up the model, not evaluated
get_model <- function() {

  # x: mutant trait value
  # xres: resident trait value

  alist(

    # Attack rates of the mutant on each resource
    w1 <- w0 * exp(-s * (x + psi)^2 / psi^2),
    w2 <- w0 * exp(-s * (x - psi)^2 / psi^2),

    # Attack rates of the resident on each resource
    w1res <- w0 * exp(-s * (xres + psi)^2 / psi^2),
    w2res <- w0 * exp(-s * (xres - psi)^2 / psi^2),

    # Equilibrium resource concentrations of each resource in each habitat
    R11 <- iota / (omicron + N[1] * w1res),
    R12 <- h * iota / (omicron + N[2] * w1res),
    R21 <- h * iota / (omicron + N[1] * w2res),
    R22 <- iota / (omicron + N[2] * w2res),

    # Mutant reproductive success in each habitat
    W1 <- w1 * R11 + w2  * R21,
    W2 <- w1 * R12 + w2  * R22,

    # Attractiveness of the mutant to the resident
    A <- exp(-a * (x - xres)^2),

    # Geometric growth rate of the mutant in each habitat
    r1 <- 1 - d + 0.5 * W1 * (1 + A),
    r2 <- 1 - d + 0.5 * W2 * (1 + A),

    # Migration matrix
    M <- matrix(c(1 - m, m, m, 1 - m), 2, 2, byrow = TRUE),

    # Geometric growth matrix
    Q <- matrix(c(r1, 0, 0, r2), 2, 2, byrow = TRUE),

    # Transition matrix
    Lambda <- M %*% Q

  )

}
