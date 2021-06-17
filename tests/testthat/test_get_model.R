test_that("Model is loaded correctly", {

  model <- get_model()
  pars <- get_default_pars()
  x <- xres <- 1
  for (i in seq(pars)) eval(pars[[i]])
  N <- c(1000, 1000)
  for (i in seq(model)) eval(model[[i]])
  expect_true(exists("Lambda"))

})
