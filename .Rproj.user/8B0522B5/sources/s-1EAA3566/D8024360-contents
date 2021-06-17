#' Numerically find roots of a function
#'
#' Root-finding algorithm using sliding windows to numerically find the roots
#' of a function within a range.
#'
#' @param f The function whose roots to find (must have one argument only)
#' @param from,to The bounds of the range of values to explore
#' @param n The number of bins in which to split the range
#' @param size The size of the sliding window, in number of bins
#' @param precis To what precision digit to round the solutions (no rounding if
#' \code{NULL})
#' @param ... Additional arguments to be passed to \code{uniroot_noerr}
#'
#' @details The function numerically looks for one root within each window, then
#' removes duplicate solutions and returns the output. Sometimes duplicate
#' solutions may not be recognized as such because they differ by a negligible
#' amount. Use the \code{precis} argument to round those solutions to a lower
#' precision (done using \code{round}) and recognize them as duplicates.
#'
#' @return A vector of solutions
#'
#' @seealso \code{get_sliding_windows}, \code{uniroot_noerr}
#'
#' @examples
#'
#' f <- function(x) x^2 - 1
#' find_roots(f, -10, 10)
#'
#' @export

# Function to find the roots of a function with one argument
find_roots <- function(f, from, to, n = 10, size = 1, precis = NULL, ...) {

  if (size > n) stop("size must be smaller or equal to n")

  # Sliding windows
  windows <- get_sliding_windows(from, to, n, size)

  # Look for roots in multiple windows
  roots <- purrr::map(windows, function(win) {

    uniroot_noerr(f, interval = win, ...)

  })

  # Remove nulls
  roots <- roots[!purrr::map_lgl(roots, is.null)]

  # Simplify
  roots <- purrr::map_dbl(roots, ~ .x[["root"]])

  # Remove duplicates (at a certain level of precision)
  if (!is.null(precis)) roots <- round(roots, precis)
  roots[!duplicated(roots)]

}
