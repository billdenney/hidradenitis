#' Converts IHS4 numerical scores to character values.
#'
#' @references Zouboulis, C C et al. “Development and validation of the
#'   International Hidradenitis Suppurativa Severity Score System (IHS4),
#'   a novel dynamic scoring system to assess HS severity.” The British
#'   journal of dermatology vol. 177,5 (2017): 1401-1409. doi:10.1111/bjd.15748
#'
#' @param ihs4_scores Integer vector representing the IHS4 scores (0-3: Mild,
#'   4-10: Moderate, >=11: Severe).
#' @return A character vector representing the corresponding IHS4 categories
#' @export
#' @family IHS4
#' @examples
#' ihs4_char(c(2, 5, 12))

ihs4_char <- function(ihs4_scores) {
  # Assertions using checkmate package
  checkmate::assert_integerish(ihs4_scores, lower = 0, null.ok = FALSE)

  # Define the IHS4 categories
  ihs4_categories <-
    cut(
      ihs4_scores,
      breaks = c(-Inf, 3, 10, Inf),
      labels = c("Mild", "Moderate", "Severe"),
      right = TRUE
    )

  as.character(ihs4_categories)
}
