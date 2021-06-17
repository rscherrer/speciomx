test_that("Uniroot does not error when ends are on the same side", {

  # A function with two roots between -1 and 1
  f <- function(x) cos(x)^4 - 4*cos(x)^3 + 8*cos(x)^2 - 5*cos(x) + 1/2

  # f(x) is negative at the two ends, regular uniroot should error
  expect_error(uniroot(f, c(-1, 1)))
  expect_true(is.null(uniroot_noerr(f, c(-1, 1))))

})
