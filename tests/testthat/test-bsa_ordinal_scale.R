test_that("convert_bsa_to_ordinal NA", {
  expect_equal(
    convert_bsa_to_ordinal(NA_real_),
    NA_integer_
  )
})
