test_that("check_clean_score", {
  expect_identical(
    hs_pga_num(
      abscess_fistula = 0,
      inflammatory_nodule = 0,
      non_inflammatory_nodule = 0
    ),
    1L
  )
})

test_that("check_minimal_score", {
  expect_identical(
    hs_pga_num(
      abscess_fistula = 0,
      inflammatory_nodule = 0,
      non_inflammatory_nodule = 1
    ),
    2L
  )
  expect_identical(
    hs_pga_num(
      abscess_fistula = 0,
      inflammatory_nodule = 0,
      non_inflammatory_nodule = 5
    ),
    2L
  )
})

test_that("check_mild_score", {
  expect_identical(
    hs_pga_num(
      abscess_fistula = 0,
      inflammatory_nodule = 1,
      non_inflammatory_nodule = 0
    ),
    3L
  )
  expect_identical(
    hs_pga_num(
      abscess_fistula = 0,
      inflammatory_nodule = 4,
      non_inflammatory_nodule = 0
    ),
    3L
  )
  expect_identical(
    hs_pga_num(
      abscess_fistula = 1,
      inflammatory_nodule = 0,
      non_inflammatory_nodule = 0
    ),
    3L
  )
})

test_that("check_moderate_score", {
  expect_identical(
    hs_pga_num(
      abscess_fistula = 0,
      inflammatory_nodule = 5,
      non_inflammatory_nodule = 0
    ),
    4L
  )
  expect_identical(
    hs_pga_num(
      abscess_fistula = 1,
      inflammatory_nodule = 1,
      non_inflammatory_nodule = 0
    ),
    4L
  )
  expect_identical(
    hs_pga_num(
      abscess_fistula = 2,
      inflammatory_nodule = 0,
      non_inflammatory_nodule = 0
    ),
    4L
  )
  expect_identical(
    hs_pga_num(
      abscess_fistula = 5,
      inflammatory_nodule = 9,
      non_inflammatory_nodule = 0
    ),
    4L
  )
})

test_that("check_severe_scores", {
  expect_identical(
    hs_pga_num(
      abscess_fistula = 2,
      inflammatory_nodule = 10,
      non_inflammatory_nodule = 0
    ),
    5L
  )
  expect_identical(
    hs_pga_num(
      abscess_fistula = 5,
      inflammatory_nodule = 15,
      non_inflammatory_nodule = 0
    ),
    5L
  )
})

test_that("check_very_severe_score", {
  expect_identical(
    hs_pga_num(
      abscess_fistula = 6,
      inflammatory_nodule = 0,
      non_inflammatory_nodule = 0
    ),
    6L
  )
  expect_identical(
    hs_pga_num(
      abscess_fistula = 10,
      inflammatory_nodule = 5,
      non_inflammatory_nodule = 0
    ),
    6L
  )
})

test_that("check_input_length_consistency", {
  expect_error(
    hs_pga_num(
      abscess_fistula = c(0, 0),
      inflammatory_nodule = c(0, 0, 0),
      non_inflammatory_nodule = c(0, 0)
    ),
    regexp = "'inflammatory_nodule'.*Must have length 2, but has length 3."
  )
})

test_that("check_missing_values", {
  expect_identical(
    hs_pga_num(
      abscess_fistula = c(0, NA, 0),
      inflammatory_nodule = c(0, 0, 0),
      non_inflammatory_nodule = c(0, 0, 0)
    ),
    c(1L, NA, 1L)
  )
  expect_identical(
    hs_pga_num(
      abscess_fistula = c(0, 0, 0),
      inflammatory_nodule = c(0, NA, 0),
      non_inflammatory_nodule = c(0, 0, 0)
    ),
    c(1L, NA, 1L)
  )
  expect_identical(
    hs_pga_num(
      abscess_fistula = c(0, 0, 0),
      inflammatory_nodule = c(0, 0, 0),
      non_inflammatory_nodule = c(0, NA, 0)
    ),
    c(1L, NA, 1L)
  )
})

test_that("check_negative_values", {
  expect_error(
    hs_pga_num(
      abscess_fistula = c(-1, 0, 0),
      inflammatory_nodule = c(0, 0, 0),
      non_inflammatory_nodule = c(0, 0, 0)
    ),
    regexp = "'abscess_fistula' failed: Element 1 is not >= 0."
  )
  expect_error(
    hs_pga_num(
      abscess_fistula = c(0, 0, 0),
      inflammatory_nodule = c(-1, 0, 0),
      non_inflammatory_nodule = c(0, 0, 0)
    ),
    regexp = "'inflammatory_nodule' failed: Element 1 is not >= 0."
  )
  expect_error(
    hs_pga_num(
      abscess_fistula = c(0, 0, 0),
      inflammatory_nodule = c(0, 0, 0),
      non_inflammatory_nodule = c(-1, 0, 0)
    ),
    regexp = "'non_inflammatory_nodule' failed: Element 1 is not >= 0."
  )
})

test_that("check_non_integer_values", {
  expect_error(
    hs_pga_num(
      abscess_fistula = c(0.5, 0, 0),
      inflammatory_nodule = c(0, 0, 0),
      non_inflammatory_nodule = c(0, 0, 0)
    ),
    regexp = "'abscess_fistula' failed: Must be of type 'integerish'"
  )
  expect_error(
    hs_pga_num(
      abscess_fistula = c(0, 0, 0),
      inflammatory_nodule = c(0.5, 0, 0),
      non_inflammatory_nodule = c(0, 0, 0)
    ),
    regexp = "'inflammatory_nodule' failed: Must be of type 'integerish'"
  )
  expect_error(
    hs_pga_num(
      abscess_fistula = c(0, 0, 0),
      inflammatory_nodule = c(0, 0, 0),
      non_inflammatory_nodule = c(0.5, 0, 0)
    ),
    regexp = "'non_inflammatory_nodule' failed: Must be of type 'integerish'"
  )
})

test_that("check_edge_cases", {
  expect_identical(
    hs_pga_num(
      abscess_fistula = 0, inflammatory_nodule = 0, non_inflammatory_nodule = 1
    ),
    2L
  )
  expect_identical(
    hs_pga_num(
      abscess_fistula = 0, inflammatory_nodule = 1, non_inflammatory_nodule = 0
    ),
    3L
  )
  expect_identical(
    hs_pga_num(
      abscess_fistula = 1, inflammatory_nodule = 0, non_inflammatory_nodule = 0
    ),
    3L
  )
  expect_identical(
    hs_pga_num(
      abscess_fistula = 0, inflammatory_nodule = 5, non_inflammatory_nodule = 0
    ),
    4L
  )
  expect_identical(
    hs_pga_num(
      abscess_fistula = 2, inflammatory_nodule = 9, non_inflammatory_nodule = 0
    ),
    4L
  )
  expect_identical(
    hs_pga_num(
      abscess_fistula = 2, inflammatory_nodule = 10, non_inflammatory_nodule = 0
    ),
    5L
  )
  expect_identical(
    hs_pga_num(
      abscess_fistula = 6, inflammatory_nodule = 0, non_inflammatory_nodule = 0
    ),
    6L
  )
})

test_that("check_large_input_values", {
  expect_identical(
    hs_pga_num(
      abscess_fistula = 10,
      inflammatory_nodule = 20,
      non_inflammatory_nodule = 30
    ),
    6L
  )
  expect_identical(
    hs_pga_num(
      abscess_fistula = 0,
      inflammatory_nodule = 100,
      non_inflammatory_nodule = 200
    ),
    4L
  )
})

test_that("check_character_input", {
  expect_error(
    hs_pga_num(
      abscess_fistula = "0",
      inflammatory_nodule = "0",
      non_inflammatory_nodule = "0"
    ),
    regexp = "'abscess_fistula' failed: Must be of type 'integerish'"
  )
  expect_error(
    hs_pga_num(
      abscess_fistula = 0,
      inflammatory_nodule = "0",
      non_inflammatory_nodule = "0"
    ),
    regexp = "'inflammatory_nodule' failed: Must be of type 'integerish'"
  )
  expect_error(
    hs_pga_num(
      abscess_fistula = 0,
      inflammatory_nodule = 0,
      non_inflammatory_nodule = "0"
    ),
    regexp = "'non_inflammatory_nodule' failed: Must be of type 'integerish'"
  )
})
