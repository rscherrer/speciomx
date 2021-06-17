test_that("Gradient is zero", {

  G <- get_gradient(0, get_default_pars(), c(1000, 1000))
  expect_equal(G, 0)

})
