#' Calculates the HASI-R score for each patient and visit.
#'
#'
#' @references Goldfarb N, Lowes MA, Butt M, King T, Alavi A, Kirby JS.
#'   Hidradenitis Suppurativa Area and Severity Index Revised (HASI-R):
#'   psychometric property assessment. Br J Dermatol. 2021 May;184(5):905-912.
#'   doi: 10.1111/bjd.19565. Epub 2020 Dec 30. PMID: 32969027; PMCID:
#'   PMC8573730.
#' 
#' @inheritParams hasi_bsa_to_ordinal
#' @param inflam_color_chg Integer vector representing inflammatory color change scores (0-3).
#' @param induration Integer vector representing induration scores (0-3).
#' @param open_skin_surface Integer vector representing open skin surface scores (0-3).
#' @param tunnels Integer vector representing tunnels scores (0-3).
#' @return A data frame with patientID, visitDY, and the calculated HASI-R score.
#' @export
#' @examples
#'
#' patientID <-
#'   c(
#'     "000-001", "000-001", "000-001", "000-001",
#'     "000-002", "000-002", "000-002", "000-002"
#'   )
#' visitDY <- c(1, 1, 2, 2, 1, 1, 2, 2)
#' BodySite <-
#'   c(
#'     "Left Axilla", "Chest", "Left Axilla", "Chest",
#'     "Left Axilla", "Chest", "Left Axilla", "Chest"
#'   )
#' BSA <- c(1, 2, 1, 2, 2, 1, 2, 1)
#' InflammColorChg <- c(1, 2, 1, 2, 2, 1, 2, 1)
#' Induration <- c(2, 2, 1, 1, 1, 1, 1, 1)
#' OpenSkinSurface <- c(1, 1, 2, 3, 2, 3, 2, 3)
#' Tunnels <- c(0, 1, 2, 0, 2, 0, 2, 0)
#'
#' # hasi_r_scores <-
#' #   hasi_r_num(
#' #     patientID, visitDY, BodySite, BSA,
#' #     InflammColorChg, Induration, OpenSkinSurface,
#' #     Tunnels
#' #   )
#' # hasi_r_scores
#'

# Rethinking ideas due to https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8573730/

hasi_r_num <- function(bsa_percent_within_site = NULL, bsa_percent_total_body = NULL,
                       bsa_ordinal = NULL, bodysite = NULL, inflam_color_chg,
                       induration, open_skin_surface, tunnels) {
  
  has_bsa_percent_within_site <- !is.null(bsa_percent_within_site)
  has_bsa_percent_total_body <- !is.null(bsa_percent_total_body)
  has_bsa_ordinal <- !is.null(bsa_ordinal)
  
  if (has_bsa_percent_within_site){
  bsa_ordinal <- hasi_bsa_to_ordinal(bsa_percent_within_site)
  }

  # The trick to these is that the current data will need a separation of numeric and character indicators.
  checkmate::assert_integerish(InflammColorChg, lower = 0, upper = 3, any.missing = TRUE, null.ok = FALSE)
  checkmate::assert_integerish(Induration, lower = 0, upper = 3, any.missing = TRUE, null.ok = FALSE)
  checkmate::assert_integerish(OpenSkinSurface, lower = 0, upper = 3, any.missing = TRUE, null.ok = FALSE)
  checkmate::assert_integerish(Tunnels, lower = 0, upper = 3, any.missing = TRUE, null.ok = FALSE)

  # Calculate the sum of scores for each component for each body site
  severity_index <- InflammColorChg + Induration + OpenSkinSurface + Tunnels

  # Multiply severity index by BSA - Main Calculation
  site_score <- severity_index * bsa_ordinal

  # Splitting data
  hasi_r_scores <- sum(site_score)

  hasi_r_scores
}



# Note: HASI-R is unique for each Patient across each Visit.
