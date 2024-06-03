#' Calculates IHS4 scores based on abscess, draining fistula, and inflammatory nodule counts.
#'
#' @param nodules Integer vector representing the nodule count.
#' @param abscesses Integer vector representing the abscess count.
#' @param draining_tunnels Integer vector representing the draining tunnel count.
#' @return An integer vector representing the IHS4 scores (0-3: Mild, 4-10: Moderate, 11- : Severe).
#' @export
#' @examples
#'
#' nodules <- c(5, 3, 2)
#' abscesses <- c(2, 1, 0)
#' draining_tunnels <- c(1, 2, 3)
#'
#' IHS4_num(nodules, abscesses, draining_tunnels)
#'

IHS4_num <- function(nodules, abscesses, draining_tunnels) {

  # Assertions using checkmate package
  checkmate::assert_integerish(nodules, lower = 0, null.ok = FALSE)
  checkmate::assert_integerish(abscesses, lower = 0, len = length(nodules), null.ok = FALSE)
  checkmate::assert_integerish(draining_tunnels, lower = 0, len = length(nodules), null.ok = FALSE)

  # Calculate IHS4 scores based on the criteria
  ihs4_score <- (nodules * 1) + (abscesses * 2) + (draining_tunnels * 4)

  ihs4_score
}
