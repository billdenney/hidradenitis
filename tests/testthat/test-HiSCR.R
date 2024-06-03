test_that("check_abscess_count_increase", {
  expect_false(all(calculate_hiscr(c(3, 2, 4), c(5, 4, 6), c(2, 1, 3),
                                   c(4, 1, 2), c(2, 3, 2), c(2, 1, 3),
                                   50)))
})

test_that("check_fistula_count_increase", {
  expect_false(all(calculate_hiscr(c(3, 2, 4), c(5, 4, 6), c(2, 1, 3),
                                   c(1, 1, 2), c(2, 3, 2), c(3, 2, 4),
                                   50)))
})

test_that("check_input_length_consistency", {
  expect_error(calculate_hiscr(c(3, 2), c(5, 4, 6), c(2, 1, 3),
                               c(1, 1, 2), c(2, 3, 2), c(2, 1, 3),
                               50))
})

test_that("check_percentage_range", {
  expect_error(calculate_hiscr(c(3, 2, 4), c(5, 4, 6), c(2, 1, 3),
                               c(1, 1, 2), c(2, 3, 2), c(2, 1, 3),
                               120))
  expect_error(calculate_hiscr(c(3, 2, 4), c(5, 4, 6), c(2, 1, 3),
                               c(1, 1, 2), c(2, 3, 2), c(2, 1, 3),
                               30))
})

test_that("check_hiscr50_response", {
  expect_true(calculate_hiscr(3, 5, 2, 1, 2, 2, 50))
  expect_false(calculate_hiscr(3, 5, 2, 2, 3, 2, 50))
})

test_that("check_hiscr75_response", {
  expect_true(calculate_hiscr(4, 6, 3, 1, 1, 3, 75))
  expect_false(calculate_hiscr(4, 6, 3, 2, 2, 3, 75))
})

test_that("check_hiscr90_response", {
  expect_true(calculate_hiscr(4, 6, 3, 1, 0, 0, 90))
  expect_false(calculate_hiscr(4, 6, 3, 2, 2, 3, 90))
})

test_that("check_no_abscess_nodule_reduction", {
  expect_false(calculate_hiscr(3, 5, 2, 3, 5, 2, 50))
})

test_that("check_abscess_increase_nodule_decrease", {
  expect_false(calculate_hiscr(3, 5, 2, 4, 2, 2, 50))
})

test_that("check_nodule_increase_abscess_decrease", {
  expect_false(calculate_hiscr(3, 5, 2, 1, 8, 2, 50))
})

test_that("check_fistula_increase", {
  expect_false(calculate_hiscr(3, 5, 2, 1, 2, 3, 50))
})

test_that("check_complete_clearance", {
  expect_true(calculate_hiscr(3, 5, 2, 0, 0, 0, 50))
})