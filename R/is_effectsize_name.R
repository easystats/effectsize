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
    x %in% es_names_unlisted
  }
}


#' List of effect size names
#' @keywords internal
es_names <- list(
  onetail = c(
    "Eta2" = "Eta_Sq",
    "Eta2 (partial)" = "Eta_Sq_partial",
    "Epsilon2" = "Epsilon_Sq",
    "Epsilon2 (partial)" = "Epsilon_Sq_partial",
    "Omega2" = "Omega_Sq",
    "Omega2 (partial)" = "Omega_Sq_partial",
    "Cohen's f" = "Cohens_f",
    "Cohen's f (partial)" = "Cohens_f_partial",
    "Cramer's V" = "cramers_v",
    "Cramer's V (adj.)" = "cramers_v_adjusted",
    "Phi" = "phi",
    "Phi (adj.)" = "phi_adjusted"
  ),
  twotail = c(
    d = "d",
    r = "r",
    "Cohen's d" = "Cohens_d",
    "Hedge's g" = "Hedges_g",
    "Glass' delta" = "Glass_delta",
    "Coefficient (std.)" = "Std_Coefficient"
  )
)

#' @keywords internal
es_names_unlisted <- c(es_names$onetail, es_names$twotail)