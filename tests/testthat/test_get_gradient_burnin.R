test_that("Burn-in selection gradient should be negative", {

  pars <- get_default_pars()
  G <- get_gradient_burnin(0, pars)
  expect_true(G < 0)

})
