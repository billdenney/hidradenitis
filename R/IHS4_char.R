#' Converts IHS4 numerical scores to character values.
#'
#' @param hs_pga_scores An integer vector representing the IHS4 scores (0-3: Mild, 4-10: Moderate, 11- : Severe).
#' @return A character vector representing the corresponding IHS4 categories.
#' @export
#' @examples
#' IHS4_scores <- c(2, 5, 12)
#'
#' IHS4_char(IHS4_scores)
#'

IHS4_char <- function(IHS4_scores) {
  
  # Assertions using checkmate package
  checkmate::assert_integerish(IHS4_scores, lower = 0, any.missing = FALSE, null.ok = FALSE)
  
  # Define the IHS4 categories
  IHS4_categories <- cut(IHS4_scores, 
                         breaks = c(-Inf, 3, 10, Inf), 
                         labels = c("Mild", "Moderate", "Severe"),
                         right = TRUE)
  
  # Convert IHS4 scores to character values
  IHS4_char_ret <- as.character(IHS4_categories)
  
  IHS4_char_ret
}