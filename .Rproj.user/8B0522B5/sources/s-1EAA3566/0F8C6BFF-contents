test_that("Finding multiple roots", {

  # A function with two roots between -1 and 1
  f <- function(x) cos(x)^4 - 4*cos(x)^3 + 8*cos(x)^2 - 5*cos(x) + 1/2

  # Three roots between -1 and 2
  expect_equal(length(find_roots(f, -1, 2)), 3)

})
