#' Generate sliding windows
#'
#' @param from,to The bounds of the range to slide across
#' @param n The number of bins in which to split the range
#' @param size The size of the sliding window, in number of bins
#' @param intervals See the "Value" section
#'
#' @return If \code{intervals = TRUE}, a list of window ranges. Otherwise,
#' a list of two vectors: a vector of lower bounds and a vector of upper bounds
#' for each window.
#'
#' @examples
#'
#' get_sliding_windows(0, 10, 10, 2)
#'
#' @export

# Function to get starts and ends of sliding windows
get_sliding_windows <- function(from, to, n, size, intervals = TRUE) {

  # Sequence of breaks
  breaks <- seq(from, to, length.out = n + 1)

  # Pick the starts and ends of each window
  starts <- breaks[1:(length(breaks) - size)]
  ends <- breaks[(1 + size):length(breaks)]

  # Return a list of intervals
  if (intervals) return(purrr::map2(starts, ends, ~ c(.x, .y)))

  # Or return start and end points separated
  return(list(starts = starts, ends = ends))

}
