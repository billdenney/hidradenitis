#' Converts BSA percentage to the ordinal scale for HASI-R scoring.
#'
#' DEscription -
#'
#' @param bsa_percent Numeric vector representing BSA percentages.
#' @return An integer vector representing the ordinal scale values.
#' @export
#' @examples
#' 
#' bsa_percent <- c(0, 2, 5, 12, 25, 40, 75)
#' bsa_ordinal <- convert_bsa_to_ordinal(bsa_percent)
#' bsa_ordinal
#' 
#' test <- data.frame(BSA = c(0,2,5,12,25,40,75))
#' test |> mutate(BSA_ordinal = convert_bsa_to_ordinal(BSA))
#' 

convert_bsa_to_ordinal <- function(bsa_percent) {
  checkmate::assert_numeric(bsa_percent, lower = 0, upper = 100, any.missing = FALSE, null.ok = FALSE)
  
  bsa_ordinal <- cut(bsa_percent, 
                     breaks = c(-Inf, 0, 3, 9, 20, 29, 50, Inf), 
                     labels = c(0, 1, 2, 3, 4, 5, 6), 
                     right = TRUE)
  
  as.integer(as.character(bsa_ordinal))
}
