#' Calculates IHS4 scores based on abscess, draining fistula, and inflammatory
#' nodule counts.
#'
#' @references Zouboulis, C C et al. “Development and validation of the
#'   International Hidradenitis Suppurativa Severity Score System (IHS4),
#'   a novel dynamic scoring system to assess HS severity.” The British
#'   journal of dermatology vol. 177,5 (2017): 1401-1409. doi:10.1111/bjd.15748
#'
#' @param nodules Integer vector representing the nodule count
#' @param abscesses Integer vector representing the abscess count
#' @param draining_tunnels Integer vector representing the draining tunnel count
#' @return An integer vector representing the IHS4 scores (0-3: Mild,
#'   4-10: Moderate, 11- : Severe).
#' @export
#' @family IHS4
#' @examples
#' ihs4_num(
#'   nodules = c(5, 3, 2),
#'   abscesses = c(2, 1, 0),
#'   draining_tunnels = c(1, 2, 3)
#' )

ihs4_num <- function(nodules, abscesses, draining_tunnels) {
  # Assertions using checkmate package
  checkmate::assert_integerish(nodules, lower = 0, null.ok = FALSE)
  checkmate::assert_integerish(
    abscesses, lower = 0, len = length(nodules), null.ok = FALSE
  )
  checkmate::assert_integerish(
    draining_tunnels, lower = 0, len = length(nodules), null.ok = FALSE
  )

  # Calculate IHS4 scores based on the criteria
  (nodules * 1) + (abscesses * 2) + (draining_tunnels * 4)
}
