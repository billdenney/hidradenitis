test_that("Error with lack of all BodySites", {
  patientID <- c("000-001")
  visitDY <- c(1)
  BodySite <- c("Right Axilla")
  BSA <- c(1)
  InflammColorChg <- c(1)
  Induration <- c(1)
  OpenSkinSurface <- c(1)
  Tunnels <- c(1)
  
  expect_error(hasi_r_num(patientID, visitDY, BodySite, BSA, InflammColorChg, Induration, OpenSkinSurface, Tunnels))
})

test_that("Error with Incorrect BodySite", {
  patientID <- c("000-001")
  visitDY <- c(1)
  BodySite <- c("Axillae")
  BSA <- c(1)
  InflammColorChg <- c(1)
  Induration <- c(1)
  OpenSkinSurface <- c(1)
  Tunnels <- c(1)
  
  expect_error(hasi_r_num(patientID, visitDY, BodySite, BSA, InflammColorChg, Induration, OpenSkinSurface, Tunnels))
})

# Test with a full example containing all BodySites
test_that("hasi_r_num works with full example", {
  patientID <- c("000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001")
  visitDY <- rep(1, 10)
  BodySite <- c("Right Axilla", "Buttocks including Intergluteal Cleft", "Back", "Left Thigh", "Head & Neck", "Left Axilla", "Chest", "Pubis & Genitals", "Abdomen", "Right Thigh")
  BSA <- c(1.5, 8.5, 14, 8, 9, 1.5, 8, 1.5, 8, 8)
  InflammColorChg <- rep(2, 10)
  Induration <- rep(1, 10)
  OpenSkinSurface <- rep(3, 10)
  Tunnels <- rep(0, 10)
  result <- hasi_r_num(patientID, visitDY, BodySite, BSA, InflammColorChg, Induration, OpenSkinSurface, Tunnels)
  expect_true(is.data.frame(result))
  expect_true("HASI_R_Score" %in% colnames(result))
})

# Test with missing BodySite: "Right Axilla"
test_that("hasi_r_num gives error with missing Right Axilla", {
  patientID <- c("000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001")
  visitDY <- rep(1, 9)
  BodySite <- c("Buttocks including Intergluteal Cleft", "Back", "Left Thigh", "Head & Neck", "Left Axilla", "Chest", "Pubis & Genitals", "Abdomen", "Right Thigh")
  BSA <- c(8.5, 14, 8, 9, 1.5, 8, 1.5, 8, 8)
  InflammColorChg <- rep(2, 9)
  Induration <- rep(1, 9)
  OpenSkinSurface <- rep(3, 9)
  Tunnels <- rep(0, 9)
  expect_error(hasi_r_num(patientID, visitDY, BodySite, BSA, InflammColorChg, Induration, OpenSkinSurface, Tunnels))
})

# Test with missing argument patientID
test_that("hasi_r_num gives error with missing patientID", {
  visitDY <- rep(1, 10)
  BodySite <- c("Right Axilla", "Buttocks including Intergluteal Cleft", "Back", "Left Thigh", "Head & Neck", "Left Axilla", "Chest", "Pubis & Genitals", "Abdomen", "Right Thigh")
  BSA <- c(1.5, 8.5, 14, 8, 9, 1.5, 8, 1.5, 8, 8)
  InflammColorChg <- rep(2, 10)
  Induration <- rep(1, 10)
  OpenSkinSurface <- rep(3, 10)
  Tunnels <- rep(0, 10)
  expect_error(hasi_r_num(visitDY, BodySite, BSA, InflammColorChg, Induration, OpenSkinSurface, Tunnels))
})

# Test with incorrect data type for BSA
test_that("hasi_r_num gives error with incorrect data type for BSA", {
  patientID <- c("000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001")
  visitDY <- rep(1, 10)
  BodySite <- c("Right Axilla", "Buttocks including Intergluteal Cleft", "Back", "Left Thigh", "Head & Neck", "Left Axilla", "Chest", "Pubis & Genitals", "Abdomen", "Right Thigh")
  BSA <- as.character(c(1.5, 8.5, 14, 8, 9, 1.5, 8, 1.5, 8, 8))
  InflammColorChg <- rep(2, 10)
  Induration <- rep(1, 10)
  OpenSkinSurface <- rep(3, 10)
  Tunnels <- rep(0, 10)
  expect_error(hasi_r_num(patientID, visitDY, BodySite, BSA, InflammColorChg, Induration, OpenSkinSurface, Tunnels))
})

# Test with missing BodySite: "Head & Neck"
test_that("hasi_r_num gives error with missing Head & Neck", {
  patientID <- c("000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001")
  visitDY <- rep(1, 9)
  BodySite <- c("Right Axilla", "Buttocks including Intergluteal Cleft", "Back", "Left Thigh", "Left Axilla", "Chest", "Pubis & Genitals", "Abdomen", "Right Thigh")
  BSA <- c(1.5, 8.5, 14, 8, 1.5, 8, 1.5, 8, 8)
  InflammColorChg <- rep(2, 9)
  Induration <- rep(1, 9)
  OpenSkinSurface <- rep(3, 9)
  Tunnels <- rep(0, 9)
  expect_error(hasi_r_num(patientID, visitDY, BodySite, BSA, InflammColorChg, Induration, OpenSkinSurface, Tunnels))
})

# Test with missing argument visitDY
test_that("hasi_r_num gives error with missing visitDY", {
  patientID <- c("000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001")
  BodySite <- c("Right Axilla", "Buttocks including Intergluteal Cleft", "Back", "Left Thigh", "Head & Neck", "Left Axilla", "Chest", "Pubis & Genitals", "Abdomen", "Right Thigh")
  BSA <- c(1.5, 8.5, 14, 8, 9, 1.5, 8, 1.5, 8, 8)
  InflammColorChg <- rep(2, 10)
  Induration <- rep(1, 10)
  OpenSkinSurface <- rep(3, 10)
  Tunnels <- rep(0, 10)
  expect_error(hasi_r_num(patientID, BodySite,BSA , InflammColorChg, Induration, OpenSkinSurface, Tunnels))
})

# Test with incorrect data type for patientID
test_that("hasi_r_num gives error with incorrect data type for patientID", {
  patientID <- as.integer(rep(1, 10))
  visitDY <- rep(1, 10)
  BodySite <- c("Right Axilla", "Buttocks including Intergluteal Cleft", "Back", "Left Thigh", "Head & Neck", "Left Axilla", "Chest", "Pubis & Genitals", "Abdomen", "Right Thigh")
  BSA <- c(1.5, 8.5, 14, 8, 9, 1.5, 8, 1.5, 8, 8)
  InflammColorChg <- rep(2, 10)
  Induration <- rep(1, 10)
  OpenSkinSurface <- rep(3, 10)
  Tunnels <- rep(0, 10)
  expect_error(hasi_r_num(patientID, visitDY, BodySite, BSA, InflammColorChg, Induration, OpenSkinSurface, Tunnels))
})

# Test with missing argument InflammColorChg
test_that("hasi_r_num gives error with missing InflammColorChg", {
  patientID <- c("000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001")
  visitDY <- rep(1, 10)
  BodySite <- c("Right Axilla", "Buttocks including Intergluteal Cleft", "Back", "Left Thigh", "Head & Neck", "Left Axilla", "Chest", "Pubis & Genitals", "Abdomen", "Right Thigh")
  BSA <- c(1.5, 8.5, 14, 8, 9, 1.5, 8, 1.5, 8, 8)
  Induration <- rep(1, 10)
  OpenSkinSurface <- rep(3, 10)
  Tunnels <- rep(0, 10)
  expect_error(hasi_r_num(patientID, visitDY, BodySite, BSA, Induration, OpenSkinSurface, Tunnels))
})

# Test with incorrect data type for visitDY
test_that("hasi_r_num gives error with incorrect data type for visitDY", {
  patientID <- c("000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001")
  visitDY <- as.character(rep(1, 10))
  BodySite <- c("Right Axilla", "Buttocks including Intergluteal Cleft", "Back", "Left Thigh", "Head & Neck", "Left Axilla", "Chest", "Pubis & Genitals", "Abdomen", "Right Thigh")
  BSA <- c(1.5, 8.5, 14, 8, 9, 1.5, 8, 1.5, 8, 8)
  InflammColorChg <- rep(2, 10)
  Induration <- rep(1, 10)
  OpenSkinSurface <- rep(3, 10)
  Tunnels <- rep(0, 10)
  expect_error(hasi_r_num(patientID, visitDY, BodySite, BSA, InflammColorChg, Induration, OpenSkinSurface, Tunnels))
})

# Test with incorrect data type for InflammColorChg
test_that("hasi_r_num gives error with incorrect data type for InflammColorChg", {
  patientID <- c("000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001", "000-001")
  visitDY <- rep(1, 10)
  BodySite <- c("Right Axilla", "Buttocks including Intergluteal Cleft", "Back", "Left Thigh", "Head & Neck", "Left Axilla", "Chest", "Pubis & Genitals", "Abdomen", "Right Thigh")
  BSA <- c(1.5, 8.5, 14, 8, 9, 1.5, 8, 1.5, 8, 8)
  InflammColorChg <- as.character(rep(2, 10))
  Induration <- rep(1, 10)
  OpenSkinSurface <- rep(3, 10)
  Tunnels <- rep(0, 10)
  expect_error(hasi_r_num(patientID, visitDY, BodySite, BSA, InflammColorChg, Induration, OpenSkinSurface, Tunnels))
})





