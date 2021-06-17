test_that("Default does nothing", {

  data <- as_tibble_speciome(1:10)
  expect_equal(data[[1]], 1:10)

})

test_that("Rearrange data into multiple columns", {

  data <- as_tibble_speciome(1:10, ncol = 5)
  expect_true("tbl" %in% class(data))
  expect_equal(nrow(data), 2)
  expect_equal(ncol(data), 5)

})

test_that("Rearrange data into one duplicated column", {

  data <- as_tibble_speciome(1:10, ncol = -2)
  expect_equal(nrow(data), 20)
  expect_equal(ncol(data), 1)

})

test_that("Duplicates are in the right order", {

  data <- as_tibble_speciome(1:3, ncol = -2)
  expect_equal(data[[1]], c(1, 1, 2, 2, 3, 3))

})

test_that("Duplicate different elements different numbers of times", {

  data <- as_tibble_speciome(1:3, ncol = 1:3)
  expect_equal(data[[1]], c(1, 2, 2, 3, 3, 3))

})
