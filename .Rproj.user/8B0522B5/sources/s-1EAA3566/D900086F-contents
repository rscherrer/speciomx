test_that("Dimorphic model is loaded correctly", {

  model <- get_model_di()
  pars <- get_default_pars()
  x1 <- x2 <- xres1 <- xres2 <- 1
  for (i in seq(pars)) eval(pars[[i]])
  N1 <- N2 <- c(1000, 1000)
  for (i in seq(model)) eval(model[[i]])
  expect_true(exists("Lambda1"))
  expect_true(exists("Lambda2"))

})
