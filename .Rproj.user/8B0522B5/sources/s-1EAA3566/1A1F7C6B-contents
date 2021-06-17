#' Dimorphic model set-up
#'
#' Generates an unevaluated list of model variable statements
#'
#' @return A list of expressions to run to evaluate the model
#'
#' @note Use \code{eval()} on an expression in the list to execute it.
#' Beware that the evaluation of the model assumes that parameters and their
#' values are present in the environment (see e.g. \code{?get_default_pars}) and
#' that variables \code{x1}, \code{x2}, \code{xres1} and \code{xres2} exist.
#'
#' @seealso \code{get_model}, \code{get_model_burnin}
#'
#' @examples
#'
#' get_model_di()
#'
#' @export

# Dimorphic model (after branching)
get_model_di <- function() {

  # x1: mutant trait value in morph 1
  # x2: mutant trait value in morph 2
  # xres1: resident trait value in morph 1
  # xres2: resident trait vlaue in morph 2

  alist(

    # Mutant attack rate wik on resource i in morph k
    w11 <- w0 * exp(-s * (x1 + psi)^2 / psi^2),
    w21 <- w0 * exp(-s * (x1 - psi)^2 / psi^2),
    w12 <- w0 * exp(-s * (x2 + psi)^2 / psi^2),
    w22 <- w0 * exp(-s * (x2 - psi)^2 / psi^2),

    # Resident attack rate wiresk on resource i in morph k
    w1res1 <- w0 * exp(-s * (xres1 + psi)^2 / psi^2),
    w2res1 <- w0 * exp(-s * (xres1 - psi)^2 / psi^2),
    w1res2 <- w0 * exp(-s * (xres2 + psi)^2 / psi^2),
    w2res2 <- w0 * exp(-s * (xres2 - psi)^2 / psi^2),

    # Equilibrium concentration Rij of resource i in habitat j
    R11 <- iota / (omicron + N1[1] * w1res1 + N2[1] * w1res2),
    R12 <- h * iota / (omicron + N1[2] * w1res1 + N2[2] * w1res2),
    R21 <- h * iota / (omicron + N1[1] * w2res1 + N2[1] * w2res2),
    R22 <- iota / (omicron + N1[2] * w2res1 + N2[2] * w2res2),

    # Mutant reproductive success Wjk in habitat j of morph k
    W11 <- w11 * R11 + w21 * R21,
    W21 <- w11 * R12 + w21 * R22,
    W12 <- w12 * R11 + w22 * R21,
    W22 <- w12 * R12 + w22 * R22,

    # Mutant attractiveness Ak in morph k
    A1 <- exp(-a * (x1 - xres1)^2),
    A2 <- exp(-a * (x2 - xres2)^2),

    # Mutant geometric growth rate rjk in habitat j of morph k
    r11 <- 1 - d + 0.5 * W11 * (1 + A1),
    r21 <- 1 - d + 0.5 * W21 * (1 + A1),
    r12 <- 1 - d + 0.5 * W12 * (1 + A2),
    r22 <- 1 - d + 0.5 * W22 * (1 + A2),

    # Migration matrix
    M <- matrix(c(1 - m, m, m, 1 - m), 2, 2, byrow = TRUE),

    # Geometric growth matrices for each morph
    Q1 <- matrix(c(r11, 0, 0, r21), 2, 2, byrow = TRUE),
    Q2 <- matrix(c(r12, 0, 0, r22), 2, 2, byrow = TRUE),

    # Transition matrices for each morph
    Lambda1 <- M %*% Q1,
    Lambda2 <- M %*% Q2

  )

}
