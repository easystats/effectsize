#' Checks if character is of a supported effect size
#'
#' For use by other functions and packages.
#'
#' @param x A character, or a vactor / list of ones.
#' @export
is_effectsize_name <- function(x) {
  if (length(x) > 1) {
    sapply(x, is_effectsize_name)
  } else {
    x %in% unlist(es_names)
  }
}


#' List of effect size names
#' @keywords internal
es_names <- list(
  onetail = c(
    "Eta_Sq",
    "Eta_Sq_partial",
    "Epsilon_Sq",
    "Epsilon_Sq_partial",
    "Omega_Sq",
    "Omega_Sq_partial",
    "Cohens_f",
    "Cohens_f_partial",
    "cramers_v",
    "cramers_v_adjusted",
    "phi",
    "phi_adjusted"
  ),
  twotail = c("d", "r", "Cohens_d", "Hedges_g", "Glass_delta")
)
