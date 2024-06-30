test_that("Error with lack of all BodySites", {
  BodySite <- c("Right Axilla")
  BSA <- c(1)
  InflammColorChg <- c(1)
  Induration <- c(1)
  OpenSkinSurface <- c(1)
  Tunnels <- c(1)

  expect_error(hasi_r_num(bsa_percent_within_site = BSA,
                          bsa_percent_total_body = NULL,
                          bsa_ordinal = NULL,
                          bodysite = BodySite,
                          inflam_color_chg = InflammColorChg,
                          induration = Induration,
                          open_skin_surface = OpenSkinSurface,
                          tunnels = Tunnels))
})

test_that("Error with Incorrect BodySite", {
  BodySite <- c("Axillae")
  BSA <- c(1)
  InflammColorChg <- c(1)
  Induration <- c(1)
  OpenSkinSurface <- c(1)
  Tunnels <- c(1)

  expect_error(hasi_r_num(bsa_percent_within_site = BSA,
                          bsa_percent_total_body = NULL,
                          bsa_ordinal = NULL,
                          bodysite = BodySite,
                          inflam_color_chg = InflammColorChg,
                          induration = Induration,
                          open_skin_surface = OpenSkinSurface,
                          tunnels = Tunnels))
})

# Test with a full example containing all BodySites
test_that("hasi_r_num works with full example", {
  BodySite <-
    c(
      "Right Axilla", "Buttocks including Intergluteal Cleft", "Back",
      "Left Thigh", "Head & Neck", "Left Axilla", "Chest", "Pubis & Genitals",
      "Abdomen", "Right Thigh"
    )
  BSA <- c(12, 5, 12, 34, 55, 16, 22, 9, 23, 75)
  InflammColorChg <- rep(2, 10)
  Induration <- rep(1, 10)
  OpenSkinSurface <- rep(3, 10)
  Tunnels <- rep(0, 10)
  expect_equal(
    hasi_r_num(
      bsa_percent_within_site = BSA,
      bsa_percent_total_body = NULL,
      bsa_ordinal = NULL,
      bodysite = BodySite,
      inflam_color_chg = InflammColorChg,
      induration = Induration,
      open_skin_surface = OpenSkinSurface,
      tunnels = Tunnels),
    228
  )
})

# Test with missing BodySite: "Right Axilla"
test_that("hasi_r_num gives error with missing Right Axilla", {
  BodySite <-
    c(
      "Buttocks including Intergluteal Cleft", "Back", "Left Thigh",
      "Head & Neck", "Left Axilla", "Chest", "Pubis & Genitals", "Abdomen",
      "Right Thigh"
    )
  BSA <- c(8.5, 14, 8, 9, 1.5, 8, 1.5, 8, 8)
  InflammColorChg <- rep(2, 9)
  Induration <- rep(1, 9)
  OpenSkinSurface <- rep(3, 9)
  Tunnels <- rep(0, 9)
  expect_error(hasi_r_num(bsa_percent_within_site = BSA,
                          bsa_percent_total_body = NULL,
                          bsa_ordinal = NULL,
                          bodysite = BodySite,
                          inflam_color_chg = InflammColorChg,
                          induration = Induration,
                          open_skin_surface = OpenSkinSurface,
                          tunnels = Tunnels))
})

# Test with incorrect data type for BSA
test_that("hasi_r_num gives error with incorrect data type for BSA", {
  BodySite <-
    c(
      "Right Axilla", "Buttocks including Intergluteal Cleft", "Back",
      "Left Thigh", "Head & Neck", "Left Axilla", "Chest", "Pubis & Genitals",
      "Abdomen", "Right Thigh"
    )
  BSA <- as.character(c(1.5, 8.5, 14, 8, 9, 1.5, 8, 1.5, 8, 8))
  InflammColorChg <- rep(2, 10)
  Induration <- rep(1, 10)
  OpenSkinSurface <- rep(3, 10)
  Tunnels <- rep(0, 10)
  expect_error(hasi_r_num(bsa_percent_within_site = BSA,
                          bsa_percent_total_body = NULL,
                          bsa_ordinal = NULL,
                          bodysite = BodySite,
                          inflam_color_chg = InflammColorChg,
                          induration = Induration,
                          open_skin_surface = OpenSkinSurface,
                          tunnels = Tunnels))
})

# Test with missing BodySite: "Head & Neck"
test_that("hasi_r_num gives error with missing Head & Neck", {
  BodySite <-
    c(
      "Right Axilla", "Buttocks including Intergluteal Cleft", "Back",
      "Left Thigh", "Left Axilla", "Chest", "Pubis & Genitals", "Abdomen",
      "Right Thigh"
    )
  BSA <- c(1.5, 8.5, 14, 8, 1.5, 8, 1.5, 8, 8)
  InflammColorChg <- rep(2, 9)
  Induration <- rep(1, 9)
  OpenSkinSurface <- rep(3, 9)
  Tunnels <- rep(0, 9)
  expect_error(hasi_r_num(bsa_percent_within_site = BSA,
                          bsa_percent_total_body = NULL,
                          bsa_ordinal = NULL,
                          bodysite = BodySite,
                          inflam_color_chg = InflammColorChg,
                          induration = Induration,
                          open_skin_surface = OpenSkinSurface,
                          tunnels = Tunnels))
})

# Test with missing argument InflammColorChg
test_that("hasi_r_num gives error with missing InflammColorChg", {
  BodySite <-
    c(
      "Right Axilla", "Buttocks including Intergluteal Cleft", "Back",
      "Left Thigh", "Head & Neck", "Left Axilla", "Chest", "Pubis & Genitals",
      "Abdomen", "Right Thigh"
    )
  BSA <- c(12, 5, 12, 34, 55, 16, 22, 9, 23, 75)
  InflammColorChg <- rep(2, 10)
  Induration <- rep(1, 10)
  OpenSkinSurface <- rep(3, 10)
  Tunnels <- rep(0, 10)
  expect_error(
    hasi_r_num(
      bsa_percent_within_site = BSA,
      bsa_percent_total_body = NULL,
      bsa_ordinal = NULL,
      bodysite = BodySite,
      #inflam_color_chg = InflammColorChg,
      induration = Induration,
      open_skin_surface = OpenSkinSurface,
      tunnels = Tunnels),
    regexp = "\"inflam_color_chg\" is missing"
  )
})

test_that("hasi_r_num gives error with incorrect data type for InflammColorChg", {
  BodySite <-
    c(
      "Right Axilla", "Buttocks including Intergluteal Cleft", "Back",
      "Left Thigh", "Head & Neck", "Left Axilla", "Chest", "Pubis & Genitals",
      "Abdomen", "Right Thigh"
    )
  BSA <- c(12, 5, 12, 34, 55, 16, 22, 9, 23, 75)
  InflammColorChg <- rep(2, 10)
  Induration <- rep(1, 10)
  OpenSkinSurface <- rep(3, 10)
  Tunnels <- rep(0, 10)
  expect_error(
    hasi_r_num(
      bsa_percent_within_site = BSA,
      bsa_percent_total_body = NULL,
      bsa_ordinal = NULL,
      bodysite = BodySite,
      inflam_color_chg = as.character(InflammColorChg),
      induration = Induration,
      open_skin_surface = OpenSkinSurface,
      tunnels = Tunnels),
    regexp = "Assertion on 'inflam_color_chg' failed: Must be of type 'integerish', not 'character'."
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
