test_that("Simulate the burn-in", {

  pars <- get_default_pars()
  data <- simulate_burnin(0, 10, pars)
  expect_true("tbl" %in% class(data))
  expect_equal(colnames(data), c("time", "x"))

})
