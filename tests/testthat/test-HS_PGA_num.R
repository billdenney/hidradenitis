test_that("check_clean_score", {
  expect_equal(hs_pga_num(0, 0, 0), 1)
})

test_that("check_minimal_score", {
  expect_equal(hs_pga_num(0, 0, 1), 2)
  expect_equal(hs_pga_num(0, 0, 5), 2)
})

test_that("check_mild_score", {
  expect_equal(hs_pga_num(0, 1, 0), 3)
  expect_equal(hs_pga_num(0, 4, 0), 3)
  expect_equal(hs_pga_num(1, 0, 0), 3)
})

test_that("check_moderate_score", {
  expect_equal(hs_pga_num(0, 5, 0), 4)
  expect_equal(hs_pga_num(1, 1, 0), 4)
  expect_equal(hs_pga_num(2, 0, 0), 4)
  expect_equal(hs_pga_num(5, 9, 0), 4)
})

test_that("check_severe_scores", {
  expect_equal(hs_pga_num(2, 10, 0), 5)
  expect_equal(hs_pga_num(5, 15, 0), 5)
})

test_that("check_very_severe_score", {
  expect_equal(hs_pga_num(6, 0, 0), 6)
  expect_equal(hs_pga_num(10, 5, 0), 6)
})

test_that("check_input_length_consistency", {
  expect_error(hs_pga_num(c(0, 0), c(0, 0, 0), c(0, 0)))
})

test_that("check_missing_values", {
  expect_equal(
    hs_pga_num(c(0, NA, 0), c(0, 0, 0), c(0, 0, 0)),
    c(1, NA, 1)
  )
  expect_equal(
    hs_pga_num(c(0, 0, 0), c(0, NA, 0), c(0, 0, 0)),
    c(1, NA, 1)
  )
  expect_equal(
    hs_pga_num(c(0, 0, 0), c(0, 0, 0), c(0, NA, 0)),
    c(1, NA, 1)
  )
})

test_that("check_negative_values", {
  expect_error(hs_pga_num(c(-1, 0, 0), c(0, 0, 0), c(0, 0, 0)))
  expect_error(hs_pga_num(c(0, 0, 0), c(-1, 0, 0), c(0, 0, 0)))
  expect_error(hs_pga_num(c(0, 0, 0), c(0, 0, 0), c(-1, 0, 0)))
})

test_that("check_non_integer_values", {
  expect_error(hs_pga_num(c(0.5, 0, 0), c(0, 0, 0), c(0, 0, 0)))
  expect_error(hs_pga_num(c(0, 0, 0), c(0.5, 0, 0), c(0, 0, 0)))
  expect_error(hs_pga_num(c(0, 0, 0), c(0, 0, 0), c(0.5, 0, 0)))
})

test_that("check_edge_cases", {
  expect_equal(hs_pga_num(0, 0, 1), 2)
  expect_equal(hs_pga_num(0, 1, 0), 3)
  expect_equal(hs_pga_num(1, 0, 0), 3)
  expect_equal(hs_pga_num(0, 5, 0), 4)
  expect_equal(hs_pga_num(2, 9, 0), 4)
  expect_equal(hs_pga_num(2, 10, 0), 5)
  expect_equal(hs_pga_num(6, 0, 0), 6)
})

test_that("check_large_input_values", {
  expect_equal(hs_pga_num(10, 20, 30), 6)
  expect_equal(hs_pga_num(0, 100, 200), 4)
})

test_that("check_named_input_vectors", {
  expect_equal(hs_pga_num(abscess_fistula = 0, inflammatory_nodule = 0, non_inflammatory_nodule = 0), 1)
  expect_equal(hs_pga_num(abscess_fistula = 1, inflammatory_nodule = 0, non_inflammatory_nodule = 0), 3)
})

test_that("check_character_input", {
  expect_error(hs_pga_num("0", "0", "0"))
  expect_error(hs_pga_num("1", "0", "0"))
})

test_that("check_logical_input", {
  expect_error(hs_pga_num(FALSE, FALSE, FALSE))
  expect_error(hs_pga_num(TRUE, FALSE, FALSE))
})
