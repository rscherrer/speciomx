test_that("Zero should be evolutionarily unstable", {

  pars <- get_default_pars()
  expect_false(is_stable(0, pars, init = c(1000, 1000)))

})
