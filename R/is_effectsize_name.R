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
    x %in% es_info$name
  }
}


#' List of effect size names
#'
#' Can always add more info here if need be...
#'
#' @keywords internal
es_info <- data.frame(
  name = c(
    "Eta2", "Eta2_partial", "Eta2_generalized",
    "Epsilon2", "Epsilon2_partial",
    "Omega2", "Omega2_partial",
    "Cohens_f", "Cohens_f_partial", "Cohens_f2", "Cohens_f2_partial",
    "Cramers_v", "Cramers_v_adjusted", "phi", "phi_adjusted", "Cohens_g",
    "Odds_ratio", "log_Odds_ratio", "Risk_ratio", "log_Risk_ratio",
    "r", "d", "Cohens_d", "Hedges_g", "Glass_delta",
    "Std_Coefficient", "Std_Odds_ratio", "Std_Risk_ratio", "Std_IRR",
    "Std_Median", "Std_Mean", "Std_MAP"
  ),
  label = c(
    "Eta2", "Eta2 (partial)", "Eta2 (generalized)",
    "Epsilon2", "Epsilon2 (partial)",
    "Omega2", "Omega2 (partial)",
    "Cohen's f", "Cohen's f (partial)", "Cohen's f2", "Cohen's f2 (partial)",
    "Cramer's V", "Cramer's V (adj.)", "Phi", "Phi (adj.)", "Cohen's g",
    "Odds ratio", "log(Odds ratio)", "Risk ratio", "log(Risk ratio)",
    "r", "d", "Cohen's d", "Hedge's g", "Glass' delta",
    "Coefficient (std.)", "Odds Ratio (std.)", "Risk Ratio (std.)", "IRR (std.)",
    "Median (std.)", "Mean (std.)", "MAP (std.)"
  ),
  direction = c(
    "onetail", "onetail", "onetail",
    "onetail", "onetail",
    "onetail", "onetail",
    "onetail", "onetail", "onetail", "onetail",
    "onetail", "onetail", "onetail", "onetail", "onetail",
    "twotail", "twotail", "twotail", "twotail",
    "twotail", "twotail", "twotail", "twotail", "twotail",
    "twotail", "twotail", "twotail", "twotail",
    "twotail", "twotail", "twotail"
  ),
  stringsAsFactors = FALSE
)
