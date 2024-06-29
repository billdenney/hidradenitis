#' Converts HS-PGA numerical scores to character values.
#'
#' @references Marzano, A V et al. “Creation of a severity index for
#'   hidradenitis suppurativa that includes a validated quality-of-life
#'   measure: the HIDRAscore.” Journal of the European Academy of Dermatology
#'   and Venereology : JEADV vol. 34,8 (2020): 1815-1821. doi:10.1111/jdv.16328
#'
#' @param hs_pga_scores Integer vector representing the HS-PGA scores (1: Clean,
#'   2: Minimal, 3: Mild, 4: Moderate, 5: Severe, 6: Very Severe).
#' @return A character vector representing the corresponding HS-PGA categories.
#' @export
#' @examples
#' hs_pga_char(c(1, 2, 3, 4, 5, 6))
hs_pga_char <- function(hs_pga_scores) {

  # Assertions using checkmate package
  checkmate::assert_integerish(hs_pga_scores, lower = 1, upper = 6, null.ok = FALSE)

  # Define the HS-PGA categories
  hs_pga_categories <- c("Clean", "Minimal", "Mild", "Moderate", "Severe", "Very Severe")

  # Convert HS-PGA scores to character values, keep the original names
  hs_pga_char <- stats::setNames(hs_pga_categories[hs_pga_scores], names(hs_pga_scores))

  hs_pga_char
}
