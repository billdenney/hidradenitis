#' Converts HS-PGA numerical scores to character values.
#'
#' @param hs_pga_scores Integer vector representing the HS-PGA scores (1: Clean, 2: Minimal, 3: Mild, 4: Moderate, 5: Severe, 6: Very Severe).
#' @return A character vector representing the corresponding HS-PGA categories.
#' @export
#' @examples
#' hs_pga_scores <- c(1, 2, 3, 4, 5, 6)
#'
#' hs_pga_char(hs_pga_scores)
#'
#' @import
#'
hs_pga_char <- function(hs_pga_scores) {
  
  # Assertions using checkmate package
  checkmate::assert_integerish(hs_pga_scores, lower = 1, upper = 6, any.missing = FALSE, null.ok = FALSE)
  
  # Define the HS-PGA categories
  hs_pga_categories <- c("Clean", "Minimal", "Mild", "Moderate", "Severe", "Very Severe")
  
  # Convert HS-PGA scores to character values
  hs_pga_char <- hs_pga_categories[hs_pga_scores]
  
  return(hs_pga_char)
}