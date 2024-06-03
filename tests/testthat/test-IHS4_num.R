test_that("IHS4_num function works correctly", {
  nodules <- c(5, 3, 2)
  abscesses <- c(2, 1, 0)
  draining_tunnels <- c(1, 2, 3)
  expected_output <- c(13, 13, 14)

  result <- IHS4_num(nodules, abscesses, draining_tunnels)

  expect_equal(result, expected_output)
})

test_that("IHS4_num handles zeros correctly", {
  nodules <- c(0, 0, 0)
  abscesses <- c(0, 0, 0)
  draining_tunnels <- c(0, 0, 0)
  expected_output <- c(0, 0, 0)

  result <- IHS4_num(nodules, abscesses, draining_tunnels)

  expect_equal(result, expected_output)
})

test_that("IHS4_num handles large values correctly", {
  nodules <- c(100, 200, 300)
  abscesses <- c(50, 75, 100)
  draining_tunnels <- c(25, 50, 75)
  expected_output <- c(300, 550, 800)

  result <- IHS4_num(nodules, abscesses, draining_tunnels)

  expect_equal(result, expected_output)
})

test_that("IHS4_num handles very large values correctly", {
  nodules <- c(1e6, 2e6, 3e6)
  abscesses <- c(5e5, 1e6, 1.5e6)
  draining_tunnels <- c(2.5e5, 5e5, 7.5e5)
  expected_output <- c(3000000, 6000000, 9000000)

  result <- IHS4_num(nodules, abscesses, draining_tunnels)

  expect_equal(result, expected_output)
})

test_that("IHS4_num throws error for invalid input lengths", {
  nodules <- c(1, 2)
  abscesses <- c(1, 2, 3)
  draining_tunnels <- c(1, 2, 3)

  expect_error(IHS4_num(nodules, abscesses, draining_tunnels), "Must have length 2, but has length 3")
})

test_that("IHS4_num throws error for negative values", {
  nodules <- c(-1, 2, 3)
  abscesses <- c(1, 2, 3)
  draining_tunnels <- c(1, 2, 3)

  expect_error(IHS4_num(nodules, abscesses, draining_tunnels), "Element 1 is not >= 0")
})

test_that("IHS4_num throws error for NA values", {
  nodules <- c(1, 2, NA)
  abscesses <- c(1, 2, 3)
  draining_tunnels <- c(1, 2, 3)

  expect_equal(
    IHS4_num(nodules, abscesses, draining_tunnels),
    c(7, 14, NA)
  )
})

test_that("IHS4_num handles single values correctly", {
  nodules <- 2
  abscesses <- 1
  draining_tunnels <- 1
  expected_output <- 8

  result <- IHS4_num(nodules, abscesses, draining_tunnels)

  expect_equal(result, expected_output)
})
