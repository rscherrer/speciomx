test_that("Dimorphic gradients should be opposites", {

  pars <- get_default_pars()
  init <- rep(1000, 4)
  G <- get_gradient_di(-1, 1, pars, init)
  expect_equal(G[1], -G[2])

})
