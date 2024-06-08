test_that("Simple case with one patient, one visit, and one body site", {
  patientID <- c("000-001")
  visitDY <- c(1)
  BodySite <- c("Right Axilla")
  BSA <- c(1)
  InflammColorChg <- c(1)
  Induration <- c(1)
  OpenSkinSurface <- c(1)
  Tunnels <- c(1)
  
  expected <- data.frame(patientID = "000-001", visitDY = 1, HASI_R_Score = 4)
  result <- hasi_r_num(patientID, visitDY, BodySite, BSA, InflammColorChg, Induration, OpenSkinSurface, Tunnels)
  
  expect_equal(result, expected)
})

test_that("Simple case with one patient, one visit, and one INCORRECT body site", {
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
