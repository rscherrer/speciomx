test_that("Pairwise invasibility plot", {

  xvalues <- seq(-1, 1, 1)
  pars <- get_default_pars()
  init <- c(1000, 1000)
  expect_true("ggplot" %in% class(plot_pip(xvalues, pars, init)))
  expect_true("ggplot" %in% class(plot_pip(xvalues, pars, init, binary = FALSE)))
  expect_true("tbl" %in% class(plot_pip(xvalues, pars, init, plotit = FALSE)))

})
