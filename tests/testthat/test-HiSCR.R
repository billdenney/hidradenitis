test_that("check_abscess_count_increase", {
  expect_equal(
    hiscr(
      baseline_abscess = c(3, 2, 4),
      baseline_nodule = c(5, 4, 6),
      baseline_fistula = c(2, 1, 3),
      timepoint_abscess = c(4, 1, 2),
      timepoint_nodule = c(2, 3, 2),
      timepoint_fistula = c(2, 1, 3),
      percentage = 50
    ),
    c(FALSE, FALSE, TRUE)
  )
})

test_that("check_fistula_count_increase", {
  expect_equal(
    hiscr(
      baseline_abscess = c(3, 2, 4),
      baseline_nodule = c(5, 4, 6),
      baseline_fistula = c(2, 1, 3),
      timepoint_abscess = c(1, 1, 2),
      timepoint_nodule = c(2, 3, 2),
      timepoint_fistula = c(3, 2, 4),
      percentage = 50
    ),
    c(FALSE, FALSE, FALSE)
  )
})

test_that("check_input_length_consistency", {
  expect_error(
    hiscr(
      c(3, 2), c(5, 4, 6), c(2, 1, 3),
      c(1, 1, 2), c(2, 3, 2), c(2, 1, 3),
      50
    ),
    regexp = "'baseline_nodule' failed: Must have length 2, but has length 3."
  )
})

test_that("check_percentage_range", {
  expect_error(
    hiscr(
      baseline_abscess = c(3, 2, 4),
      baseline_nodule = c(5, 4, 6),
      baseline_fistula = c(2, 1, 3),
      timepoint_abscess = c(1, 1, 2),
      timepoint_nodule = c(2, 3, 2),
      timepoint_fistula = c(2, 1, 3),
      percentage = 120
    ),
    regexp = "Assertion on 'percentage' failed: Element 1 is not <= 100."
  )
  expect_error(
    hiscr(
      baseline_abscess = c(3, 2, 4),
      baseline_nodule = c(5, 4, 6),
      baseline_fistula = c(2, 1, 3),
      timepoint_abscess = c(1, 1, 2),
      timepoint_nodule = c(2, 3, 2),
      timepoint_fistula = c(2, 1, 3),
      percentage = 30
    ),
    regexp = "Assertion on 'percentage' failed: Element 1 is not >= 50"
  )
})

test_that("check_hiscr50_response", {
  expect_true(
    hiscr(
      baseline_abscess = 3, baseline_nodule = 5, baseline_fistula = 2,
      timepoint_abscess = 1, timepoint_nodule = 2, timepoint_fistula = 2,
      percentage = 50
    )
  )
  expect_false(
    hiscr(
      baseline_abscess = 3, baseline_nodule = 5, baseline_fistula = 2,
      timepoint_abscess = 2, timepoint_nodule = 3, timepoint_fistula = 2,
      percentage = 50
    )
  )
})

test_that("check_hiscr75_response", {
  expect_true(
    hiscr(
      baseline_abscess = 4, baseline_nodule = 6, baseline_fistula = 3,
      timepoint_abscess = 1, timepoint_nodule = 1, timepoint_fistula = 3,
      percentage = 75
    )
  )
  expect_false(
    hiscr(
      baseline_abscess = 4, baseline_nodule = 6, baseline_fistula = 3,
      timepoint_abscess = 2, timepoint_nodule = 2, timepoint_fistula = 3,
      percentage = 75
    )
  )
})

test_that("check_hiscr90_response", {
  expect_true(
    hiscr(
      baseline_abscess = 4, baseline_nodule = 6, baseline_fistula = 3,
      timepoint_abscess = 1, timepoint_nodule = 0, timepoint_fistula = 0,
      percentage = 90
    )
  )
  expect_false(
    hiscr(
      baseline_abscess = 4, baseline_nodule = 6, baseline_fistula = 3,
      timepoint_abscess = 2, timepoint_nodule = 2, timepoint_fistula = 3,
      percentage = 90
    )
  )
})

test_that("check_no_abscess_nodule_reduction", {
  expect_false(
    hiscr(
      baseline_abscess = 3, baseline_nodule = 5, baseline_fistula = 2,
      timepoint_abscess = 3, timepoint_nodule = 5, timepoint_fistula = 2,
      percentage = 50
    )
  )
})

test_that("check_abscess_increase_nodule_decrease", {
  expect_false(
    hiscr(
      baseline_abscess = 3, baseline_nodule = 5, baseline_fistula = 2,
      timepoint_abscess = 4, timepoint_nodule = 2, timepoint_fistula = 2,
      percentage = 50
    )
  )
})

test_that("check_nodule_increase_abscess_decrease", {
  expect_false(
    hiscr(
      baseline_abscess = 3, baseline_nodule = 5, baseline_fistula = 2,
      timepoint_abscess = 1, timepoint_nodule = 8, timepoint_fistula = 2,
      percentage = 50
    )
  )
})

test_that("check_fistula_increase", {
  expect_false(
    hiscr(
      baseline_abscess = 3, baseline_nodule = 5, baseline_fistula = 2,
      timepoint_abscess = 1, timepoint_nodule = 2, timepoint_fistula = 3,
      percentage = 50
    )
  )
})

test_that("check_complete_clearance", {
  expect_true(
    hiscr(
      baseline_abscess = 3, baseline_nodule = 5, baseline_fistula = 2,
      timepoint_abscess = 0, timepoint_nodule = 0, timepoint_fistula = 0,
      percentage = 50
    )
  )
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
