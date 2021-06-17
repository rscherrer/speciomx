#' Example parameter set
#'
#' Generates an unevaluated list of parameter setting statements
#'
#' @return A list of expressions
#'
#' @note Use \code{eval()} on an expression in the list to execute it.
#' Beware that evaluation will run each statement, thereby overriding any
#' variable that shares its name with one of the parameters.
#'
#' @seealso \code{get_default_pars}
#'
#' @examples
#'
#' get_example_pars()
#'
#' @export

# Example parameter set
get_example_pars <- function() {

  alist(
    m <- 0.01, # dispersal
    d <- 0.2, # death
    s <- 1.4, # ecological selection coefficient
    psi <- 5, # ecological scale
    w0 <- 1, # maximum attack rate
    iota <- 400, # resource inflow
    omicron <- 100, # resource outflow
    a <- 0, # sexual selection coefficient
    h <- 1 # habitat symmetry
  )

}
