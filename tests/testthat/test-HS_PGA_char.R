test_that("missing_values", {
  expect_equal(
    hs_pga_char(c(1, 2, NA, 4, 5, 6)),
    c("Clean", "Minimal", NA, "Moderate", "Severe", "Very Severe")
  )
})

test_that("expect_error_out_of_range_values", {
  expect_error(hs_pga_char(c(0, 2, 3, 4, 5, 7)))
})

test_that("check_single_value", {
  expect_equal(hs_pga_char(3), "Mild")
})

test_that("check_empty_input", {
  expect_equal(hs_pga_char(integer(0)), character(0))
})

test_that("check_input_type", {
  expect_error(hs_pga_char(c("1", "2", "3", "4", "5", "6")))
})

test_that("check_input_length", {
  expect_equal(length(hs_pga_char(c(1, 2, 3))), 3)
})

test_that("check_output_type", {
  expect_type(hs_pga_char(c(1, 2, 3)), "character")
})

test_that("check_duplicate_values", {
  expect_equal(hs_pga_char(c(1, 1, 2, 2, 3, 3)), c("Clean", "Clean", "Minimal", "Minimal", "Mild", "Mild"))
})

test_that("check_mixed_order", {
  expect_equal(hs_pga_char(c(3, 1, 4, 2, 6, 5)), c("Mild", "Clean", "Moderate", "Minimal", "Very Severe", "Severe"))
})

test_that("check_large_input", {
  large_input <- rep(1:6, each = 100)
  expected_output <- rep(c("Clean", "Minimal", "Mild", "Moderate", "Severe", "Very Severe"), each = 100)
  expect_equal(hs_pga_char(large_input), expected_output)
})

test_that("check_factor_input", {
  expect_error(hs_pga_char(factor(c(1, 2, 3, 4, 5, 6))))
})
