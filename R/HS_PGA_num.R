#' Calculates HS-PGA scores based on abscess, draining fistula, and inflammatory nodule counts.
#'
#' @param abscess_fistula Integer vector representing the sum of abscess and draining fistula counts.
#' @param inflammatory_nodule Integer vector representing the inflammatory nodule count.
#' @param non_inflammatory_nodule Integer vector representing the non-inflammatory nodule count.
#' @return An integer vector representing the HS-PGA scores (1: Clean, 2: Minimal, 3: Mild, 4: Moderate, 5: Severe, 6: Very Severe).
#' @export
#' @examples
#' 
#' abscess_fistula <- c(0, 0, 1, 0, 1, 2, 6)
#' inflammatory_nodule <- c(0, 0, 0, 3, 5, 8, 12)
#' non_inflammatory_nodule <- c(0, 1, 0, 0, 0, 0, 0)
#'
#' hs_pga_num(abscess_fistula, inflammatory_nodule, non_inflammatory_nodule)
#'

hs_pga_num <- function(abscess_fistula, inflammatory_nodule, non_inflammatory_nodule) {
  
  # Assertions using checkmate package
  checkmate::assert_integerish(abscess_fistula, lower = 0, any.missing = FALSE, null.ok = FALSE)
  checkmate::assert_integerish(inflammatory_nodule, lower = 0, len = length(abscess_fistula), any.missing = FALSE, null.ok = FALSE)
  checkmate::assert_integerish(non_inflammatory_nodule, lower = 0, len = length(abscess_fistula), any.missing = FALSE, null.ok = FALSE)
  
  # Initialize the HS-PGA scores vector for use in conditionals
  hs_pga_scores <- integer(length(abscess_fistula))
  
  # Calculate HS-PGA scores based on criteria
  hs_pga_scores[abscess_fistula == 0 & inflammatory_nodule == 0 & non_inflammatory_nodule == 0] <- 1
  hs_pga_scores[abscess_fistula == 0 & inflammatory_nodule == 0 & non_inflammatory_nodule > 0] <- 2
  hs_pga_scores[abscess_fistula == 0 & inflammatory_nodule >= 1 & inflammatory_nodule <= 4] <- 3
  hs_pga_scores[abscess_fistula == 1 & inflammatory_nodule == 0] <- 3
  hs_pga_scores[abscess_fistula == 0 & inflammatory_nodule >= 5] <- 4
  hs_pga_scores[abscess_fistula == 1 & inflammatory_nodule >= 1] <- 4
  hs_pga_scores[abscess_fistula >= 2 & abscess_fistula <= 5 & inflammatory_nodule < 10] <- 4
  hs_pga_scores[abscess_fistula >= 2 & abscess_fistula <= 5 & inflammatory_nodule >= 10] <- 5
  hs_pga_scores[abscess_fistula > 5] <- 6
  
  hs_pga_scores
}