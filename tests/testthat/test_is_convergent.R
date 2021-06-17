test_that("Zero should be convergent stable", {

  pars <- get_default_pars()
  expect_true(is_convergent(0, pars, init = c(1000, 1000)))

})
