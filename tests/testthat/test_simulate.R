test_that("Evolution from monomorphic to dimorphic", {

  pars <- get_default_pars()
  init <- rep(1000, 4)
  data <- simulate(-0.1, 50, pars, init)
  expect_equal(nrow(data), 51)
  expect_equal(colnames(data), c("time", "x1", "x2"))
  expect_equal(data$x1[1], data$x2[1]) # should be one trait at the beginning
  expect_false(data$x1[nrow(data)] == data$x2[nrow(data)]) # two traits at the end

})

test_that("Full simulation with burn-in", {

  pars <- get_default_pars()
  init <- rep(1000, 4)
  data <- simulate(0, 10, pars, init, burnin = 10)
  expect_equal(nrow(data), 21)
  expect_true(data$time[1] < 0)

})
