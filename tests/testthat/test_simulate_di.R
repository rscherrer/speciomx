test_that("Dimorphic simulation", {

  pars <- get_default_pars()
  init <- rep(1000, 4)
  data <- simulate_di(0, 10, pars, init)
  expect_true("tbl" %in% class(data))
  expect_equal(colnames(data), c("time", "x1", "x2"))

})
