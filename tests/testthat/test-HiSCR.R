test_that("check_abscess_count_increase", {
  expect_false(all(hiscr(c(3, 2, 4), c(5, 4, 6), c(2, 1, 3),
                                   c(4, 1, 2), c(2, 3, 2), c(2, 1, 3),
                                   50)))
})

test_that("check_fistula_count_increase", {
  expect_false(all(hiscr(c(3, 2, 4), c(5, 4, 6), c(2, 1, 3),
                                   c(1, 1, 2), c(2, 3, 2), c(3, 2, 4),
                                   50)))
})

test_that("check_input_length_consistency", {
  expect_error(hiscr(c(3, 2), c(5, 4, 6), c(2, 1, 3),
                               c(1, 1, 2), c(2, 3, 2), c(2, 1, 3),
                               50))
})

test_that("check_percentage_range", {
  expect_error(hiscr(c(3, 2, 4), c(5, 4, 6), c(2, 1, 3),
                               c(1, 1, 2), c(2, 3, 2), c(2, 1, 3),
                               120))
  expect_error(hiscr(c(3, 2, 4), c(5, 4, 6), c(2, 1, 3),
                               c(1, 1, 2), c(2, 3, 2), c(2, 1, 3),
                               30))
})

test_that("check_hiscr50_response", {
  expect_true(hiscr(3, 5, 2, 1, 2, 2, 50))
  expect_false(hiscr(3, 5, 2, 2, 3, 2, 50))
})

test_that("check_hiscr75_response", {
  expect_true(hiscr(4, 6, 3, 1, 1, 3, 75))
  expect_false(hiscr(4, 6, 3, 2, 2, 3, 75))
})

test_that("check_hiscr90_response", {
  expect_true(hiscr(4, 6, 3, 1, 0, 0, 90))
  expect_false(hiscr(4, 6, 3, 2, 2, 3, 90))
})

test_that("check_no_abscess_nodule_reduction", {
  expect_false(hiscr(3, 5, 2, 3, 5, 2, 50))
})

test_that("check_abscess_increase_nodule_decrease", {
  expect_false(hiscr(3, 5, 2, 4, 2, 2, 50))
})

test_that("check_nodule_increase_abscess_decrease", {
  expect_false(hiscr(3, 5, 2, 1, 8, 2, 50))
})

test_that("check_fistula_increase", {
  expect_false(hiscr(3, 5, 2, 1, 2, 3, 50))
})

test_that("check_complete_clearance", {
  expect_true(hiscr(3, 5, 2, 0, 0, 0, 50))
})

test_that("hiscr NA values", {
  expect_equal(
    hiscr(
      baseline_abscess = NA_real_, baseline_nodule = 5, baseline_fistula = 2,
      timepoint_abscess = 0, timepoint_nodule = 0, timepoint_fistula = 0,
      percentage = 50
    ),
    NA
  )
  expect_equal(
    hiscr(
      baseline_abscess = 3, baseline_nodule = NA_real_, baseline_fistula = 2,
      timepoint_abscess = 0, timepoint_nodule = 0, timepoint_fistula = 0,
      percentage = 50
    ),
    NA
  )
  expect_equal(
    hiscr(
      baseline_abscess = 3, baseline_nodule = 5, baseline_fistula = NA_real_,
      timepoint_abscess = 0, timepoint_nodule = 0, timepoint_fistula = 0,
      percentage = 50
    ),
    NA
  )
  expect_equal(
    hiscr(
      baseline_abscess = 3, baseline_nodule = 5, baseline_fistula = 2,
      timepoint_abscess = NA_real_, timepoint_nodule = 0, timepoint_fistula = 0,
      percentage = 50
    ),
    NA
  )
  expect_equal(
    hiscr(
      baseline_abscess = 3, baseline_nodule = 5, baseline_fistula = 2,
      timepoint_abscess = 0, timepoint_nodule = NA_real_, timepoint_fistula = 0,
      percentage = 50
    ),
    NA
  )
  expect_equal(
    hiscr(
      baseline_abscess = 3, baseline_nodule = 5, baseline_fistula = 2,
      timepoint_abscess = 0, timepoint_nodule = 0, timepoint_fistula = NA_real_,
      percentage = 50
    ),
    NA
  )
})
