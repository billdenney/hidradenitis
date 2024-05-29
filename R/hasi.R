#' Calculates the HASI-R score for each patient and visit.
#'
#' @param patientID Character vector representing patient IDs.
#' @param visitDY Integer vector representing visit days.
#' @param BodySite Character vector representing body sites.
#' @param InflammColorChg Integer vector representing inflammatory color change scores (0-3).
#' @param Induration Integer vector representing induration scores (0-3).
#' @param OpenSkinSurface Integer vector representing open skin surface scores (0-3).
#' @param Tunnels Integer vector representing tunnels scores (0-3).
#' @return A data frame with patientID, visitDY, and the calculated HASI-R score.
#' @export
#' @examples
#' 
#' patientID <- c("000-001", "000-001", "000-001", "000-001", "000-002", "000-002", "000-002", "000-002")
#' visitDY <- c(1, 1, 2, 2, 1, 1, 2, 2)
#' BodySite <- c("Axillae", "Groin", "Axillae", "Groin", "Axillae", "Groin", "Axillae", "Groin")
#' InflammColorChg <- c(1, 2, 1, 2, 2, 1, 2, 1)
#' Induration <- c(2, 2, 1, 1, 1, 1, 1, 1)
#' OpenSkinSurface <- c(1, 1, 2, 3, 2, 3, 2, 3)
#' Tunnels <- c(0, 1, 2, 0, 2, 0, 2, 0)
#' 
#' hasi_r_scores <- calculate_hasi_r(patientID, visitDY, BodySite, InflammColorChg, Induration, OpenSkinSurface, Tunnels)
#' hasi_r_scores
#' 

# Rethinking ideas due to https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8573730/

calculate_hasi_r <- function(patientID, visitDY, BodySite, InflammColorChg, Induration, OpenSkinSurface, Tunnels) {
  checkmate::assert_character(patientID, any.missing = FALSE, null.ok = FALSE)
  checkmate::assert_character(visitDY, any.missing = FALSE, null.ok = FALSE)
  checkmate::assert_character(BodySite, any.missing = FALSE, null.ok = FALSE)
  
  # The trick to these is that the current data will need a separation of numeric and character indicators.
  checkmate::assert_integerish(InflammColorChg, lower = 0, upper = 3, any.missing = FALSE, null.ok = FALSE)
  checkmate::assert_integerish(Induration, lower = 0, upper = 3, any.missing = FALSE, null.ok = FALSE)
  checkmate::assert_integerish(OpenSkinSurface, lower = 0, upper = 3, any.missing = FALSE, null.ok = FALSE)
  checkmate::assert_integerish(Tunnels, lower = 0, upper = 3, any.missing = FALSE, null.ok = FALSE)
  
  # Combine input vectors into a data frame
  data <- data.frame(patientID, visitDY, BodySite, InflammColorChg, Induration, OpenSkinSurface, Tunnels)
  
  # Calculate the sum of scores for each component for each body site
  data$site_score <- rowSums(data[, c("InflammColorChg", "Induration", "OpenSkinSurface", "Tunnels")])
  
  # Calculate the total HASI-R score for each patient and visit
  hasi_r_scores <- aggregate(site_score ~ patientID + visitDY, data, sum)
  
  # Rename
  colnames(hasi_r_scores)[colnames(hasi_r_scores) == "site_score"] <- "HASI_R_Score"
  
  hasi_r_scores
}

# Note: HASI-R is unique for each Patient across each Visit.