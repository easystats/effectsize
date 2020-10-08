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
  name = c("Eta_Sq", "Eta_Sq_partial",
           "Epsilon_Sq", "Epsilon_Sq_partial",
           "Omega_Sq", "Omega_Sq_partial",
           "Cohens_f", "Cohens_f_partial", "Cohens_f2", "Cohens_f2_partial",
           "cramers_v", "cramers_v_adjusted", "phi", "phi_adjusted",
           "r", "d", "Cohens_d", "Hedges_g", "Glass_delta",
           "Std_Coefficient", 'Std_Median', 'Std_Mean', 'Std_MAP'),
  label = c("Eta2", "Eta2 (partial)",
            "Epsilon2", "Epsilon2 (partial)",
            "Omega2", "Omega2 (partial)",
            "Cohen's f", "Cohen's f (partial)", "Cohen's f2", "Cohen's f2 (partial)",
            "Cramer's V", "Cramer's V (adj.)", "Phi", "Phi (adj.)",
            "r", "d", "Cohen's d", "Hedge's g", "Glass' delta",
            "Coefficient (std.)", 'Median (std.)', 'Mean (std.)', "MAP (std.)"),
  direction = c("onetail", "onetail",
                "onetail", "onetail",
                "onetail", "onetail",
                "onetail", "onetail", "onetail", "onetail",
                "onetail", "onetail", "onetail", "onetail",
                "twotail", "twotail", "twotail", "twotail", "twotail",
                "twotail", "twotail", "twotail","twotail"),
  stringsAsFactors = FALSE
)