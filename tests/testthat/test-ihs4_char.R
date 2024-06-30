test_that("ihs4_char function works correctly", {
  ihs4_scores <- 0:19
  expected_output <-
    c("Mild", "Mild", "Mild", "Mild",
      "Moderate", "Moderate", "Moderate", "Moderate", "Moderate", "Moderate",
      "Moderate",
      "Severe", "Severe", "Severe", "Severe", "Severe", "Severe", "Severe",
      "Severe", "Severe"
    )

  result <- ihs4_char(ihs4_scores)

  expect_equal(result, expected_output)
})

test_that("ihs4_char handles edge cases", {
  expect_equal(ihs4_char(0), "Mild")
  expect_equal(ihs4_char(3), "Mild")
  expect_equal(ihs4_char(4), "Moderate")
  expect_equal(ihs4_char(10), "Moderate")
  expect_equal(ihs4_char(11), "Severe")
  expect_equal(ihs4_char(NA_real_), NA_character_)
})

test_that("ihs4_char handles large vectors", {
  ihs4_scores <- rep(c(0, 3, 4, 10, 11), 1000)
  expected_output <-
    rep(c("Mild", "Mild", "Moderate", "Moderate", "Severe"), 1000)

  result <- ihs4_char(ihs4_scores)

  expect_equal(result, expected_output)
})

test_that("ihs4_char throws error for invalid input", {
  expect_error(ihs4_char(-1), "Element 1 is not >= 0")
  expect_error(
    ihs4_char("string"),
    regexp = "Must be of type 'integerish', not 'character'"
  )
})

test_that("ihs4_char throws error for negative values", {
  ihs4_scores <- c(1, 2, -1, 4)
  expect_error(ihs4_char(ihs4_scores), "Element 3 is not >= 0")
})

test_that("ihs4_char throws error for NA values", {
  expect_equal(ihs4_char(NA_real_), NA_character_)
  ihs4_scores <- c(1, 2, NA, 4)
  expect_equal(
    ihs4_char(ihs4_scores),
    c("Mild", "Mild", NA, "Moderate")
  )
})

test_that("ihs4_char handles single value correctly", {
  expect_equal(ihs4_char(2), "Mild")
})
