test_that("IHS4_char function works correctly", {
  IHS4_scores <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)
  expected_output <- c("Mild", "Mild", "Mild", "Mild", "Moderate", "Moderate", "Moderate", "Moderate",
                       "Moderate", "Moderate", "Moderate", "Severe", "Severe", "Severe", "Severe",
                       "Severe", "Severe", "Severe", "Severe", "Severe")
  
  result <- IHS4_char(IHS4_scores)
  
  expect_equal(result, expected_output)
})

test_that("IHS4_char handles edge cases", {
  expect_equal(IHS4_char(c(0)), "Mild")
  expect_equal(IHS4_char(c(3)), "Mild")
  expect_equal(IHS4_char(c(4)), "Moderate")
  expect_equal(IHS4_char(c(10)), "Moderate")
  expect_equal(IHS4_char(c(11)), "Severe")
})

test_that("IHS4_char handles large vectors", {
  IHS4_scores <- rep(c(0, 3, 4, 10, 11), 1000)
  expected_output <- rep(c("Mild", "Mild", "Moderate", "Moderate", "Severe"), 1000)
  
  result <- IHS4_char(IHS4_scores)
  
  expect_equal(result, expected_output)
})

test_that("IHS4_char throws error for invalid input", {
  expect_error(IHS4_char(c(-1)), "Element 1 is not >= 0")
  expect_error(IHS4_char(c(NA)), "Contains missing values")
  expect_error(IHS4_char("string"), "Must be of type 'integerish', not 'character'")
})

test_that("IHS4_char throws error for negative values", {
  IHS4_scores <- c(1, 2, -1, 4)
  expect_error(IHS4_char(IHS4_scores), "Element 3 is not >= 0")
})

test_that("IHS4_char throws error for NA values", {
  IHS4_scores <- c(1, 2, NA, 4)
  expect_error(IHS4_char(IHS4_scores), "Contains missing values")
})

test_that("IHS4_char handles single value correctly", {
  IHS4_scores <- 2
  expected_output <- "Mild"
  
  result <- IHS4_char(IHS4_scores)
  
  expect_equal(result, expected_output)
})




