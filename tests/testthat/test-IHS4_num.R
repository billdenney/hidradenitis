test_that("ihs4_num function works correctly", {
  # Nonzero inputs
  expect_equal(
    ihs4_num(
      nodules = c(5, 3, 2),
      abscesses = c(2, 1, 0),
      draining_tunnels = c(1, 2, 3)
    ),
    c(13, 13, 14)
  )

  # Zero inputs
  expect_equal(
    ihs4_num(
      nodules = c(0, 0, 0),
      abscesses = c(0, 0, 0),
      draining_tunnels = c(0, 0, 0)
    ),
    c(0, 0, 0)
  )

  # large values
  expect_equal(
    ihs4_num(
      nodules = c(100, 200, 300),
      abscesses = c(50, 75, 100),
      draining_tunnels = c(25, 50, 75)
    ),
    c(300, 550, 800)
  )

  # very large values
  expect_equal(
    ihs4_num(
      nodules = c(1e6, 2e6, 3e6),
      abscesses = c(5e5, 1e6, 1.5e6),
      draining_tunnels = c(2.5e5, 5e5, 7.5e5)
    ),
    c(3000000, 6000000, 9000000)
  )
})

test_that("ihs4_num throws error for invalid input lengths", {
  expect_error(
    ihs4_num(
      nodules = c(1, 2),
      abscesses = 1:3,
      draining_tunnels = 1:3
    ),
    regexp = "Must have length 2, but has length 3"
  )
})

test_that("ihs4_num throws error for negative values", {
  expect_error(
    ihs4_num(
      nodules = c(-1, 2, 3),
      abscesses = c(1, 2, 3),
      draining_tunnels = c(1, 2, 3)
    ),
    regexp = "Element 1 is not >= 0"
  )
})

test_that("ihs4_num allows NA values", {
  expect_equal(
    ihs4_num(
      nodules = c(1, 2, NA),
      abscesses = c(1, 2, 3),
      draining_tunnels = c(1, 2, 3)
    ),
    c(7, 14, NA)
  )
})

test_that("ihs4_num handles single values correctly", {
  expect_equal(
    ihs4_num(
      nodules = 2,
      abscesses = 1,
      draining_tunnels = 1
    ),
    8
  )
})
