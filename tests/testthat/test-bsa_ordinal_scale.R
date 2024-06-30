test_that("convert_bsa_to_ordinal NA - error", {
  expect_equal(
    hasi_bsa_to_ordinal(NA_real_),
    NA_integer_
  )
})

test_that("Test every value", {
  expect_equal(hasi_bsa_to_ordinal(0), 0)
  expect_equal(hasi_bsa_to_ordinal(1), 1)
  expect_equal(hasi_bsa_to_ordinal(3), 1)
  expect_equal(hasi_bsa_to_ordinal(4), 2)
  expect_equal(hasi_bsa_to_ordinal(9), 2)
  expect_equal(hasi_bsa_to_ordinal(10), 3)
  expect_equal(hasi_bsa_to_ordinal(20), 3)
  expect_equal(hasi_bsa_to_ordinal(21), 4)
  expect_equal(hasi_bsa_to_ordinal(29), 4)
  expect_equal(hasi_bsa_to_ordinal(30), 5)
  expect_equal(hasi_bsa_to_ordinal(50), 5)
  expect_equal(hasi_bsa_to_ordinal(51), 6)
  expect_equal(hasi_bsa_to_ordinal(100), 6)
})

test_that("Test for values within each range", {
  expect_equal(hasi_bsa_to_ordinal(2), 1)
  expect_equal(hasi_bsa_to_ordinal(6), 2)
  expect_equal(hasi_bsa_to_ordinal(15), 3)
  expect_equal(hasi_bsa_to_ordinal(26), 4)
  expect_equal(hasi_bsa_to_ordinal(40), 5)
  expect_equal(hasi_bsa_to_ordinal(75), 6)
})

test_that("Test for values on the boundary of multiple ranges", {
  expect_equal(hasi_bsa_to_ordinal(2.999), 1)
  expect_equal(hasi_bsa_to_ordinal(3.001), 2)
  expect_equal(hasi_bsa_to_ordinal(8.999), 2)
  expect_equal(hasi_bsa_to_ordinal(9.001), 3)
  expect_equal(hasi_bsa_to_ordinal(19.999), 3)
  expect_equal(hasi_bsa_to_ordinal(20.001), 4)
  expect_equal(hasi_bsa_to_ordinal(28.999), 4)
  expect_equal(hasi_bsa_to_ordinal(29.001), 5)
  expect_equal(hasi_bsa_to_ordinal(49.999), 5)
  expect_equal(hasi_bsa_to_ordinal(50.001), 6)
})

test_that("Test for vector input", {
  expect_equal(
    hasi_bsa_to_ordinal(c(0, 2, 5, 12, 25, 40, 75)),
    c(0, 1, 2, 3, 4, 5, 6)
  )
})

test_that("Test for invalid input (negative values)", {
  expect_error(
    hasi_bsa_to_ordinal(-1),
    regexp = "Element 1 is not >= 0"
  )
  expect_error(
    hasi_bsa_to_ordinal(c(-1, 10)),
    regexp = "Element 1 is not >= 0"
  )
})

test_that("Test for invalid input (values over 100)", {
  expect_error(
    hasi_bsa_to_ordinal(bsa_percent_within_site = 101),
    regexp = "Element 1 is not <= 100"
  )
  expect_error(
    hasi_bsa_to_ordinal(bsa_percent_within_site = c(50, 101)),
    regexp = "Element 2 is not <= 100"
  )
})

test_that("Test for non-numeric input", {
  expect_error(
    hasi_bsa_to_ordinal("string"),
    regexp = "Must be of type 'numeric', not 'character'"
  )
  expect_error(
    hasi_bsa_to_ordinal(c("string", 10)),
    regexp = "Must be of type 'numeric', not 'character'"
  )
})

test_that("Test for NA values", {
  expect_error(hasi_bsa_to_ordinal(NA), regexp = "'x' must be numeric")
})

test_that("Test for empty vector", {
  expect_equal(hasi_bsa_to_ordinal(numeric(0)), integer(0))
})
