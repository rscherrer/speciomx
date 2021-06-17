test_that("Sliding windows", {

  windows <- get_sliding_windows(0, 1, 10, 2)
  expect_equal(windows[[1]], c(0, 0.2))
  expect_equal(windows[[length(windows)]], c(0.8, 1))

})
