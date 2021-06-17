test_that("Dimorphic demographic equilibrium", {

  init <- c(1000, 1000, 1000, 1000)
  N <- find_equilibrium_di(0, 0, get_default_pars(), init)
  expect_true(all(N > 0))

})
