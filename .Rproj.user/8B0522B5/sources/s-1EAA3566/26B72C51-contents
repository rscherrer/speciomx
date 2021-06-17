test_that("Invasion fitness of the resident is one", {

  init <- c(1000, 1000)
  expect_equal(get_lambda(0, 0, get_default_pars(), init), 1)
  expect_equal(get_lambda(-1, -1, get_default_pars(), init), 1)

})

test_that("Case where a mutant should invade", {

  init <- c(1000, 1000)
  lambda <- get_lambda(0.1, 0, get_default_pars(), init)
  expect_true(lambda > 1)

})
