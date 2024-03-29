test_that("Monomorphic simulation", {

  pars <- get_default_pars()
  init <- rep(1000, 2)
  data <- simulate_mono(-1, 10, pars, init)
  expect_true("tbl" %in% class(data))
  expect_equal(colnames(data), c("time", "x"))

})

test_that("Monomorphic simulation stops when branching point is reached", {

  pars <- get_default_pars()
  init <- rep(1000, 2)
  data1 <- simulate_mono(-0.1, 20, pars, init, branch = 0)
  data2 <- simulate_mono(-0.1, 20, pars, init, tol = 0.01)
  expect_true(nrow(data2) < nrow(data1))

})

test_that("Monomorphic simulation with type II branching point", {

  pars <- get_default_pars()
  init <- rep(1000, 2)
  data1 <- simulate_mono(-0.1, 20, pars, init, tol = 0.01, branch = 1)
  data2 <- simulate_mono(-0.1, 20, pars, init, tol = 0.01, branch = 2)
  expect_true(nrow(data2) < nrow(data1))

})
