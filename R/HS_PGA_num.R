#' Calculates HS-PGA scores based on abscess, draining fistula, and inflammatory nodule counts.
#'
#' @references Marzano, A V et al. “Creation of a severity index for
#'   hidradenitis suppurativa that includes a validated quality-of-life
#'   measure: the HIDRAscore.” Journal of the European Academy of Dermatology
#'   and Venereology : JEADV vol. 34,8 (2020): 1815-1821. doi:10.1111/jdv.16328
#'
#' @param abscess_fistula Integer vector representing the sum of abscess and draining fistula counts.
#' @param inflammatory_nodule Integer vector representing the inflammatory nodule count.
#' @param non_inflammatory_nodule Integer vector representing the non-inflammatory nodule count.
#' @return An integer vector representing the HS-PGA scores (1: Clean, 2: Minimal, 3: Mild, 4: Moderate, 5: Severe, 6: Very Severe).
#' @export
#' @examples
#'
#' hs_pga_num(
#'   abscess_fistula = c(0, 0, 1, 0, 1, 2, 6),
#'   inflammatory_nodule = c(0, 0, 0, 3, 5, 8, 12),
#'   non_inflammatory_nodule = c(0, 1, 0, 0, 0, 0, 0)
#' )

hs_pga_num <- function(abscess_fistula, inflammatory_nodule, non_inflammatory_nodule) {

  # Assertions using checkmate package
  checkmate::assert_integerish(abscess_fistula, lower = 0, null.ok = FALSE)
  checkmate::assert_integerish(inflammatory_nodule, lower = 0, len = length(abscess_fistula), null.ok = FALSE)
  checkmate::assert_integerish(non_inflammatory_nodule, lower = 0, len = length(abscess_fistula), null.ok = FALSE)

  # Initialize the HS-PGA scores vector for use in conditionals
  hs_pga_scores <- rep(NA_integer_, length(abscess_fistula))

  # Calculate HS-PGA scores based on criteria
  hs_pga_scores[abscess_fistula == 0 & inflammatory_nodule == 0 & non_inflammatory_nodule == 0] <- 1L
  hs_pga_scores[abscess_fistula == 0 & inflammatory_nodule == 0 & non_inflammatory_nodule > 0] <- 2L
  hs_pga_scores[abscess_fistula == 0 & inflammatory_nodule >= 1 & inflammatory_nodule <= 4] <- 3L
  hs_pga_scores[abscess_fistula == 1 & inflammatory_nodule == 0] <- 3L
  hs_pga_scores[abscess_fistula == 0 & inflammatory_nodule >= 5] <- 4L
  hs_pga_scores[abscess_fistula == 1 & inflammatory_nodule >= 1] <- 4L
  hs_pga_scores[abscess_fistula >= 2 & abscess_fistula <= 5 & inflammatory_nodule < 10] <- 4L
  hs_pga_scores[abscess_fistula >= 2 & abscess_fistula <= 5 & inflammatory_nodule >= 10] <- 5L
  hs_pga_scores[abscess_fistula > 5] <- 6L

  hs_pga_scores
}
