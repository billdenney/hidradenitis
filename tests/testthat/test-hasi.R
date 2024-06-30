test_that("Error with lack of all bodysites", {
  bodysite <- "Right Axilla"
  bsa <- 1
  inflamm_color_chg <- 1
  induration <- 1
  open_skin_surface <- 1
  tunnels <- 1

  expect_error(
    hasi_r_num(
      bsa_percent_within_site = bsa,
      bodysite = bodysite,
      inflam_color_chg = inflamm_color_chg,
      induration = induration,
      open_skin_surface = open_skin_surface,
      tunnels = tunnels
    ),
    regexp = "Assertion on 'bodysite' failed: Must be permutation of"
  )
})

test_that("Error with Incorrect bodysite", {
  bodysite <- "Axillae"
  bsa <- 1
  inflamm_color_chg <- 1
  induration <- 1
  open_skin_surface <- 1
  tunnels <- 1

  expect_error(
    hasi_r_num(
      bsa_percent_within_site = bsa,
      bodysite = bodysite,
      inflam_color_chg = inflamm_color_chg,
      induration = induration,
      open_skin_surface = open_skin_surface,
      tunnels = tunnels
    ),
    regexp = "'bodysite' failed: Must be permutation of.*but is \\['Axillae'\\]"
  )
})

# Test with a full example containing all bodysites
test_that("hasi_r_num works with full example", {
  bodysite <-
    c(
      "Right Axilla", "Buttocks including Intergluteal Cleft", "Back",
      "Left Thigh", "Head & Neck", "Left Axilla", "Chest", "Pubis & Genitals",
      "Abdomen", "Right Thigh"
    )
  bsa <- c(12, 5, 12, 34, 55, 16, 22, 9, 23, 75)
  inflamm_color_chg <- rep(2, 10)
  induration <- rep(1, 10)
  open_skin_surface <- rep(3, 10)
  tunnels <- rep(0, 10)
  expect_identical(
    hasi_r_num(
      bsa_percent_within_site = bsa,
      bsa_percent_total_body = NULL,
      bsa_ordinal = NULL,
      bodysite = bodysite,
      inflam_color_chg = inflamm_color_chg,
      induration = induration,
      open_skin_surface = open_skin_surface,
      tunnels = tunnels
    ),
    228L
  )
})

test_that("hasi_r_num gives error with missing body site", {
  bodysite <-
    c(
      "Buttocks including Intergluteal Cleft", "Back", "Left Thigh",
      "Head & Neck", "Left Axilla", "Chest", "Pubis & Genitals", "Abdomen",
      "Right Thigh"
    )
  bsa <- c(8.5, 14, 8, 9, 1.5, 8, 1.5, 8, 8)
  inflamm_color_chg <- rep(2, 9)
  induration <- rep(1, 9)
  open_skin_surface <- rep(3, 9)
  tunnels <- rep(0, 9)
  for (idx in seq_along(bodysite)) {
    expect_error(
      hasi_r_num(
        bsa_percent_within_site = bsa[-idx],
        bodysite = bodysite[-idx],
        inflam_color_chg = inflamm_color_chg[-idx],
        induration = induration[-idx],
        open_skin_surface = open_skin_surface[-idx],
        tunnels = tunnels[-idx]
      ),
      regexp = "Assertion on 'bodysite' failed: Must be permutation of"
    )
  }
})

# Test with incorrect data type for BSA
test_that("hasi_r_num gives error with incorrect data type for BSA", {
  bodysite <-
    c(
      "Right Axilla", "Buttocks including Intergluteal Cleft", "Back",
      "Left Thigh", "Head & Neck", "Left Axilla", "Chest", "Pubis & Genitals",
      "Abdomen", "Right Thigh"
    )
  bsa <- as.character(c(1.5, 8.5, 14, 8, 9, 1.5, 8, 1.5, 8, 8))
  inflamm_color_chg <- rep(2, 10)
  induration <- rep(1, 10)
  open_skin_surface <- rep(3, 10)
  tunnels <- rep(0, 10)
  expect_error(
    hasi_r_num(
      bsa_percent_within_site = bsa,
      bodysite = bodysite,
      inflam_color_chg = inflamm_color_chg,
      induration = induration,
      open_skin_surface = open_skin_surface,
      tunnels = tunnels
    ),
    regexp = "'bsa_percent_within_site'.*Must be .*'numeric', not 'character'"
  )
})

# Test with missing argument inflamm_color_chg
test_that("hasi_r_num gives error with missing inflamm_color_chg", {
  bodysite <-
    c(
      "Right Axilla", "Buttocks including Intergluteal Cleft", "Back",
      "Left Thigh", "Head & Neck", "Left Axilla", "Chest", "Pubis & Genitals",
      "Abdomen", "Right Thigh"
    )
  bsa <- c(12, 5, 12, 34, 55, 16, 22, 9, 23, 75)
  inflamm_color_chg <- rep(2, 10)
  induration <- rep(1, 10)
  open_skin_surface <- rep(3, 10)
  tunnels <- rep(0, 10)
  expect_error(
    hasi_r_num(
      bsa_percent_within_site = bsa,
      bsa_percent_total_body = NULL,
      bsa_ordinal = NULL,
      bodysite = bodysite,
      # exclude inflam_color_chg = inflamm_color_chg,
      induration = induration,
      open_skin_surface = open_skin_surface,
      tunnels = tunnels
    ),
    regexp = "\"inflam_color_chg\" is missing"
  )
})

test_that("hasi_r_num incorrect data type for inflamm_color_chg", {
  bodysite <-
    c(
      "Right Axilla", "Buttocks including Intergluteal Cleft", "Back",
      "Left Thigh", "Head & Neck", "Left Axilla", "Chest", "Pubis & Genitals",
      "Abdomen", "Right Thigh"
    )
  bsa <- c(12, 5, 12, 34, 55, 16, 22, 9, 23, 75)
  inflamm_color_chg <- rep(2, 10)
  induration <- rep(1, 10)
  open_skin_surface <- rep(3, 10)
  tunnels <- rep(0, 10)
  expect_error(
    hasi_r_num(
      bsa_percent_within_site = bsa,
      bsa_percent_total_body = NULL,
      bsa_ordinal = NULL,
      bodysite = bodysite,
      inflam_color_chg = as.character(inflamm_color_chg),
      induration = induration,
      open_skin_surface = open_skin_surface,
      tunnels = tunnels
    ),
    regexp = "'inflam_color_chg'.* 'integerish', not 'character'."
  )
})

test_that("hasi_r_num() verifies all inputs (#32)", {
  expect_error(
    hasi_r_num(
      bsa_ordinal = c(0, 0, 0, 0, 5, 1, 4.3, 1.2, 6.8, 7.2),
      bodysite =
        c("Right Axilla", "Buttocks including Intergluteal Cleft",
          "Back", "Left Thigh", "Head & Neck", "Left Axilla",
          "Chest", "Pubis & Genitals", "Abdomen", "Right Thigh"),
      inflam_color_chg = c(0, 0, 0, 0, 2, 3, 1, 3, 2, 0),
      induration = c(0, 0, 0, 0, 2, 3, 1, 3, 2, 0),
      open_skin_surface = c(0, 0, 0, 0, 2, 3, 1, 3, 2, 0),
      tunnels = c(0, 0, 0, 0, 2, 3, 1, 3, 2, 0)
    ),
    regexp = "Must be of type 'integerish'"
  )

  expect_error(
    hasi_r_num(
      bsa_percent_total_body = c(0, 0, 0, 0, 5, 1, 4.3, 1.2, 6.8, 105),
      bodysite =
        c("Right Axilla", "Buttocks including Intergluteal Cleft",
          "Back", "Left Thigh", "Head & Neck", "Left Axilla",
          "Chest", "Pubis & Genitals", "Abdomen", "Right Thigh"),
      inflam_color_chg = c(0, 0, 0, 0, 2, 3, 1, 3, 2, 0),
      induration = c(0, 0, 0, 0, 2, 3, 1, 3, 2, 0),
      open_skin_surface = c(0, 0, 0, 0, 2, 3, 1, 3, 2, 0),
      tunnels = c(0, 0, 0, 0, 2, 3, 1, 3, 2, 0)
    ),
    regexp = "Element 1 is not <= 9"
  )
})
