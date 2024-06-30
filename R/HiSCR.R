#' Calculates HiSCR scores given a percentage parameter.
#'
#' @references Kimball, A B et al. “HiSCR (Hidradenitis Suppurativa Clinical
#'   Response): a novel clinical endpoint to evaluate therapeutic outcomes in
#'   patients with hidradenitis suppurativa from the placebo-controlled portion
#'   of a phase 2 adalimumab study.” Journal of the European Academy of
#'   Dermatology and Venereology : JEADV vol. 30,6 (2016): 989-94.
#'   doi:10.1111/jdv.13216
#'
#' @param baseline_abscess Integer vector representing the abscess count at
#'   baseline.
#' @param baseline_nodule Integer vector representing the inflammatory nodule
#'   count at baseline.
#' @param baseline_fistula Integer vector representing the draining fistula
#'   count at baseline.
#' @param timepoint_abscess Integer vector representing the abscess count at the
#'   time point of interest.
#' @param timepoint_nodule Integer vector representing the inflammatory nodule
#'   count at the time point of interest.
#' @param timepoint_fistula Integer vector representing the draining fistula
#'   count at the time point of interest.
#' @param percentage Numeric value specifying the percentage required for HiSCR
#'   (e.g., 50 for HiSCR50, 75 for HiSCR75, etc.).
#' @return A logical vector indicating whether the HiSCR response is achieved
#'   for each set of input parameters.
#' @export
#' @examples
#' hiscr(
#'   baseline_abscess = c(3, 2, 4),
#'   baseline_nodule = c(5, 4, 6),
#'   baseline_fistula = c(2, 1, 3),
#'   timepoint_abscess = c(1, 1, 2),
#'   timepoint_nodule = c(2, 3, 2),
#'   timepoint_fistula = c(2, 1, 3),
#'   percentage = 50
#' )

hiscr <- function(baseline_abscess, baseline_nodule, baseline_fistula,
                  timepoint_abscess, timepoint_nodule, timepoint_fistula,
                  percentage) {

  checkmate::assert_integerish(baseline_abscess, lower = 0, null.ok = FALSE)
  checkmate::assert_integerish(
    baseline_nodule, lower = 0, len = length(baseline_abscess), null.ok = FALSE
  )
  checkmate::assert_integerish(
    baseline_fistula, lower = 0, len = length(baseline_abscess), null.ok = FALSE
  )
  checkmate::assert_integerish(
    timepoint_abscess,
    lower = 0, len = length(baseline_abscess), null.ok = FALSE
  )
  checkmate::assert_integerish(
    timepoint_nodule, lower = 0, len = length(baseline_abscess), null.ok = FALSE
  )
  checkmate::assert_integerish(
    timepoint_fistula,
    lower = 0, len = length(baseline_abscess), null.ok = FALSE
  )
  checkmate::assert_number(percentage, lower = 50, upper = 100, null.ok = FALSE)

  # Calculate total abscess and inflammatory nodule count at baseline and time
  # point
  baseline_total <- baseline_abscess + baseline_nodule
  timepoint_total <- timepoint_abscess + timepoint_nodule

  # Calculate the percentage reduction in total abscess and inflammatory nodule
  # count
  reduction_percentage <-
    (baseline_total - timepoint_total) / baseline_total * 100

  # Check if the redction percentage meets the specified threshold
  reduction_criteria <- reduction_percentage >= percentage

  # Check if there is no increase in abscess count
  abscess_criteria <- timepoint_abscess <= baseline_abscess

  # Check if there is no increase in draining fistula count
  fistula_criteria <- timepoint_fistula <= baseline_fistula

  # Determine if HiSCR response is achieved based on all criteria
  hiscr_response <- reduction_criteria & abscess_criteria & fistula_criteria

  hiscr_response
}
