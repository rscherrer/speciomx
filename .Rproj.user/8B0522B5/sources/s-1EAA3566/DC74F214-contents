#' Pairwise invasibility plot
#'
#' Generates a PIP showing the combinations of mutant and resident trait values
#' for which the mutant invades the resident (i.e. has an invasion fitness
#' greater than one).
#'
#' @param xvalues A vector of trait values over which to compute the invasion
#' fitness. Each value will be taken both as a mutant and as a resident trait
#' value, so all pairwise combinations will be evaluated.
#' @param pars An unevaluated parameter-list (e.g. as returned by
#' \code{get_default_pars})
#' @param init A vector of two starting values for solving of the demographic
#' equilibrium
#' @param binary Whether or not to plot whether the mutant invades (\code{TRUE})
#' or the actual value of the invasion fitness (\code{FALSE})
#' @param plotit Whether to return a plot or the underlying tibble
#'
#' @details The function computes the invasion fitness for all pairwise
#' combinations of mutants and residents, but recycles equilibrium population
#' sizes computed for each resident to gain in speed (using \code{fast = TRUE}
#' in \code{get_lambda}).
#'
#' @return A \code{ggplot} object
#'
#' @seealso \code{get_lambda}, \code{find_equilibrium}
#'
#' @examples
#'
#' pars <- get_default_pars()
#' plot_pip(seq(-1, 1, 1), pars, init = rep(1000, 2))
#'
#' @export

# Function to plot a pairwise invasibility plot
plot_pip <- function(xvalues, pars, init, binary = TRUE, plotit = TRUE) {

  # Compute demographic equilibria of the different residents
  N <- purrr::map(xvalues, find_equilibrium, pars, init)

  # Create a matrix of trait values and population sizes
  data <- matrix(unlist(N), ncol = 2, byrow = TRUE)
  colnames(data) <- c("N1", "N2")
  data <- tibble::as_tibble(data)
  data <- data %>% tibble::add_column(xres = xvalues, .before = "N1")

  # Expand to all combinations of mutants and residents
  data <- data %>% tidyr::expand(x = xvalues, tidyr::nesting(xres, N1, N2))

  # Compute invasion fitness
  data <- data %>%
    dplyr::mutate(
      lambda = purrr::pmap_dbl(
        list(x, xres, N1, N2),
        ~ get_lambda(..1, ..2, pars, c(..3, ..4), fast = TRUE)
      ),
      invades = lambda > 1
    )

  # Early exit
  if (!plotit) return(data)

  # Pairwise invasibility plot
  if (binary) {

    # Invades or not
    plot <- data %>%
      ggplot2::ggplot(ggplot2::aes(x = xres, y = x, fill = invades)) +
      ggplot2::geom_tile() +
      ggplot2::xlab(parse(text = "'Resident trait value'~hat(x)")) +
      ggplot2::ylab(parse(text = "'Mutant trait value'~x")) +
      ggplot2::labs(fill = parse(text = "lambda(x,hat(x))>1")) +
      ggplot2::scale_fill_manual(values = c("gray20", "gray80"))

  } else {

    # Actual value of the invasion fitness
    plot <- data %>%
      ggplot2::ggplot(ggplot2::aes(x = xres, y = x, fill = lambda)) +
      ggplot2::geom_tile() +
      ggplot2::xlab(parse(text = "'Resident trait value'~hat(x)")) +
      ggplot2::ylab(parse(text = "'Mutant trait value'~x")) +
      ggplot2::labs(fill = parse(text = "lambda(x,hat(x))")) +
      ggplot2::scale_fill_continuous(type = "viridis", option = "magma")

  }

  return(plot)

}
