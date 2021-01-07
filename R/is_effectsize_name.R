#' Checks if character is of a supported effect size
#'
#' For use by other functions and packages.
#'
#' @param x A character, or a vector.
#' @param ignore_case Should case of input be ignored?
#' @export
is_effectsize_name <- function(x, ignore_case = TRUE) {
  x_comp <- es_info$name

  if (ignore_case) {
    x <- tolower(x)
    x_comp <- tolower(x_comp)
  }

  x %in% x_comp
}

#' @export
#' @rdname is_effectsize_name
get_effectsize_label <- function(x, ignore_case = TRUE) {
  x_comp <- es_info$name

  if (ignore_case) {
    x <- tolower(x)
    x_comp <- tolower(x_comp)
  }

  idx <- match(x, x_comp)
  es_info$label[idx]
}

#' List of effect size names
#'
#' Can always add more info here if need be...
#'
#' @keywords internal
es_info <- as.data.frame(matrix(c(
  ## Diffs
  "Cohens_d", "Cohen's d", "twotail",
  "Hedges_g", "Hedges' g", "twotail",
  "Glass_delta", "Glass' delta", "twotail",

  ## xtab
  "Cramers_v", "Cramer's V", "onetail",
  "Cramers_v_adjusted", "Cramer's V (adj.)", "onetail",
  "phi", "Phi", "onetail",
  "phi_adjusted", "Phi (adj.)", "onetail",
  "Cohens_g", "Cohen's g", "onetail",
  "Cohens_h", "Cohen's h", "onetail",
  "Odds_ratio", "Odds ratio", "twotail",
  "log_Odds_ratio", "log(Odds ratio)", "twotail",
  "Risk_ratio", "Risk ratio", "twotail",
  "log_Risk_ratio", "log(Risk ratio)", "twotail",

  ## ANOVA
  "Eta2", "Eta2", "onetail",
  "Eta2_partial", "Eta2 (partial)", "onetail",
  "Eta2_generalized", "Eta2 (generalized)", "onetail",
  "Epsilon2", "Epsilon2", "onetail",
  "Epsilon2_partial", "Epsilon2 (partial)", "onetail",
  "Omega2", "Omega2", "onetail",
  "Omega2_partial", "Omega2 (partial)", "onetail",
  "Cohens_f", "Cohen's f", "onetail",
  "Cohens_f_partial", "Cohen's f (partial)", "onetail",
  "Cohens_f2", "Cohen's f2", "onetail",
  "Cohens_f2_partial", "Cohen's f2 (partial)", "onetail",

  ## Rank
  "r_rank_biserial", "r (rank biserial)", "twotail",
  "Kendalls_W", "Kendall's W", "onetail",
  "rank_epsilon_squared", "Epsilon2 (rank)", "onetail",

  ## Std Coefficient
  "Std_Coefficient", "Coefficient (std.)", "twotail",
  "Std_Odds_ratio", "Odds Ratio (std.)", "twotail",
  "Std_Risk_ratio", "Risk Ratio (std.)", "twotail",
  "Std_IRR", "IRR (std.)", "twotail",
  "Std_Median", "Median (std.)", "twotail",
  "Std_Mean", "Mean (std.)", "twotail",
  "Std_MAP", "MAP (std.)", "twotail",

  ## Other
  "r", "r", "twotail",
  "d", "d", "twotail"),
  ncol = 3, byrow = TRUE,
  dimnames = list(NULL, c("name", "label", "direction"))),
  stringsAsFactors = FALSE)
