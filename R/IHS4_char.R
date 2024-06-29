#' Converts IHS4 numerical scores to character values.
#'
#' @references Zouboulis, C C et al. “Development and validation of the
#'   International Hidradenitis Suppurativa Severity Score System (IHS4),
#'   a novel dynamic scoring system to assess HS severity.” The British
#'   journal of dermatology vol. 177,5 (2017): 1401-1409. doi:10.1111/bjd.15748
#'
#' @param IHS4_scores An integer vector representing the IHS4 scores (0-3: Mild, 4-10: Moderate, >=11: Severe).
#' @return A character vector representing the corresponding IHS4 categories.
#' @export
#' @examples
#' IHS4_char(c(2, 5, 12))

IHS4_char <- function(IHS4_scores) {
  # Assertions using checkmate package
  checkmate::assert_integerish(IHS4_scores, lower = 0, null.ok = FALSE)

  # Define the IHS4 categories
  IHS4_categories <- cut(IHS4_scores,
                         breaks = c(-Inf, 3, 10, Inf),
                         labels = c("Mild", "Moderate", "Severe"),
                         right = TRUE)

  # Convert IHS4 scores to character values
  IHS4_char_ret <- as.character(IHS4_categories)

  IHS4_char_ret
}
