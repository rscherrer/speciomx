#' Root finding without error
#'
#' Runs the \code{uniroot} root finding algorithm and returns \code{NULL}
#' instead of erroring if no solution is found.
#'
#' @param f The function whose roots to find (must have one argument only)
#' @param interval The range within which to search for a root
#' @param ... Additional parameters for \code{uniroot}
#'
#' @details The \code{uniroot} function errors if no solution is found, which
#' is undesirable when looking for multiple roots across a range, or when
#' looking for roots as part of a bigger pipeline. This function catches the
#' error produced by \code{uniroot} if no solution is found and returns
#' \code{NULL} instead.
#'
#' @return A root, or \code{NULL}
#'
#' @seealso \code{uniroot}
#'
#' @examples
#'
#' f <- function(x) x^2 - 1
#' uniroot_noerr(f, c(-50, -30))
#'
#' @export

# Uniroot without errors
uniroot_noerr <- function(f, interval, ...) {

  tryCatch(

    uniroot(f, interval, ...),
    error = function(err) return(NULL)

  )

}
