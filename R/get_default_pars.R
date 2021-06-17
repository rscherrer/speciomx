#' Default parameter set
#'
#' Generates an unevaluated list of parameter setting statements
#'
#' @return A list of expressions
#'
#' @note Use \code{eval()} on an expression in the list to execute it.
#' Beware that evaluation will run each statement, thereby overriding any
#' variable that shares its name with one of the parameters.
#'
#' @seealso \code{get_example_pars}
#'
#' @examples
#'
#' get_default_pars()
#'
#' @export

# Generate default parameter values
get_default_pars <- function() {

  alist(
    m <- 0.1, # dispersal
    d <- 0.2, # death
    s <- 1, # ecological selection coefficient
    psi <- 1, # ecological scale
    w0 <- 1, # maximum attack rate
    iota <- 400, # resource inflow
    omicron <- 100, # resource outflow
    a <- 1, # sexual selection coefficient
    h <- 1 # habitat symmetry
  )

}
