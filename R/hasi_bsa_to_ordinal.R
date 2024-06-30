#' Converts BSA percentage to the ordinal scale for HASI-R scoring.
#'
#' @references Goldfarb N, Lowes MA, Butt M, King T, Alavi A, Kirby JS.
#'   Hidradenitis Suppurativa Area and Severity Index Revised (HASI-R):
#'   psychometric property assessment. Br J Dermatol. 2021 May;184(5):905-912.
#'   doi: 10.1111/bjd.19565. Epub 2020 Dec 30. PMID: 32969027; PMCID:
#'   PMC8573730.
#'
#' @param bsa_percent_within_site Numeric vector representing BSA percentages. (0-100)
#' @param bsa_percent_total_body Numeric vector representing BSA percentages. (0-15, depending on site)
#' @param bsa_ordinal Numeric vector representing BSA values. (0-6)
#' @param bodysite Optional character vector representing body sites.
#' @return An integer vector representing the ordinal scale values.
#' @export
#' @family HASI
#' @examples
#' hasi_bsa_to_ordinal(c(0, 2, 5, 12, 25, 40, 75))

hasi_bsa_to_ordinal <- function(bsa_percent_within_site = NULL,
                                bsa_percent_total_body = NULL,
                                bsa_ordinal = NULL,
                                bodysite = NULL) {
  has_bsa_percent_within_site <- !is.null(bsa_percent_within_site)
  has_bsa_percent_total_body <- !is.null(bsa_percent_total_body)
  has_bsa_ordinal <- !is.null(bsa_ordinal)
  has_bodysite <- !is.null(bodysite)

  if ((has_bsa_percent_total_body + has_bsa_percent_within_site + has_bsa_ordinal) != 1) {
    stop("Multiple types of BSA values found")
  }

  required_bodysites <-
    c(
      "Right Axilla" = 2, "Buttocks including Intergluteal Cleft" = 9,
      "Back" = 15, "Left Thigh" = 9, "Head & Neck" = 10, "Left Axilla" = 2,
      "Chest" = 9, "Pubis & Genitals" = 2, "Abdomen" = 9, "Right Thigh" = 9
    )


  if (has_bsa_percent_within_site) {
    checkmate::assert_numeric(
      bsa_percent_within_site, lower = 0, upper = 100, null.ok = FALSE
    )
  }

  if (has_bodysite) {
    checkmate::assert_permutation(
      bodysite, names(required_bodysites), na.ok = TRUE
    )
  }

  if (has_bsa_percent_total_body) {
    bsa_percent_within_site <- rep(NA_real_, length(required_bodysites))
    for (current_bodysite in names(required_bodysites)){
      checkmate::assert_numeric(
        bsa_percent_total_body[bodysite == current_bodysite],
        lower = 0, upper = required_bodysites[current_bodysite],
        any.missing = TRUE,
        .var.name = current_bodysite
      )

      bsa_percent_within_site[bodysite == current_bodysite] <-
        100*bsa_percent_total_body[bodysite == current_bodysite]/
        required_bodysites[current_bodysite]
    }
  }

  if (!is.null(bsa_percent_within_site)) {
    checkmate::assert_numeric(
      bsa_percent_within_site, lower = 0, upper = 100, any.missing = TRUE
    )
    bsa_ordinal_ret <- cut(bsa_percent_within_site,
                       breaks = c(-Inf, 0, 3, 9, 20, 29, 50, Inf),
                       labels = c(0, 1, 2, 3, 4, 5, 6),
                       right = TRUE)

    bsa_ordinal <- as.integer(as.character(bsa_ordinal_ret))
  }
  checkmate::assert_integerish(bsa_ordinal, lower = 0, upper = 6, any.missing = TRUE)
  bsa_ordinal
}
