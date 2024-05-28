#' Calculates the HASI-R score for Hidradenitis Suppurativa severity.
#'
#' @param nodules List of integer vectors representing the count of inflammatory nodules in each body region.
#' @param abscesses List of integer vectors representing the count of abscesses in each body region.
#' @param fistulas List of integer vectors representing the count of draining fistulas in each body region.
#' @param region ?
#' @return A numeric value representing the HASI-R score.
#' @export
#' @examples
#' 
#' nodules <- list(axillae = 3, groin = 2, buttocks = 0)
#' abscesses <- list(axillae = 1, groin = 1, buttocks = 0)
#' fistulas <- list(axillae = 1, groin = 0, buttocks = 1)
#' hasi_r_score <- hs_hasi_r(nodules, abscesses, fistulas)
#' 
#' @import
#' 

hs_hasi_r <- function(nodules, abscesses, fistulas) {
  
  # Ensure input lists have the same length and names
  checkmate::assert_list(nodules, types = "integerish", any.missing = FALSE, null.ok = FALSE)
  checkmate::assert_list(abscesses, types = "integerish", len = length(nodules), names = names(nodules), any.missing = FALSE, null.ok = FALSE)
  checkmate::assert_list(fistulas, types = "integerish", len = length(nodules), names = names(nodules), any.missing = FALSE, null.ok = FALSE)
  
  # Weights for different lesion types
  weights <- list(nodule = 1, abscess = 2, fistula = 4)
  
  # Initialize HASI-R score and regions
  regions <- names(nodules)
  hasi_r_score <- 0
  
  # Iterate over each body region to calculate the weighted score
  for (region in regions) {
    hasi_r_score <- hasi_r_score +
      nodules[[region]] * weights$nodule +
      abscesses[[region]] * weights$abscess +
      fistulas[[region]] * weights$fistula
  }
  
  return(hasi_r_score)
}