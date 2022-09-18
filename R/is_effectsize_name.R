#' Checks for a Valid Effect Size Name
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
get_effectsize_name <- function(x, ignore_case = TRUE) {
  x[is_effectsize_name(x, ignore_case = ignore_case)]
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
es_info <- matrix(
  c(
    ## Diffs
    "Cohens_d", "Cohen's d", "twotail", -Inf, Inf, 0,
    "Hedges_g", "Hedges' g", "twotail", -Inf, Inf, 0,
    "Glass_delta", "Glass' delta", "twotail", -Inf, Inf, 0,
    "Mahalanobis_D", "Mahalanobis' D", "onetail", 0, Inf, 0,

    ## xtab
    "Cramers_v", "Cramer's V", "onetail", 0, 1, 0,
    "Cramers_v_adjusted", "Cramer's V (adj.)", "onetail", 0, 1, 0,
    "phi", "Phi", "onetail", 0, 1, 0,
    "phi_adjusted", "Phi (adj.)", "onetail", 0, 1, 0,
    "Pearsons_c", "Pearson's C", "onetail", 0, 1, 0,
    "Cohens_w", "Cohen's w", "onetail", 0, Inf, 0,
    "Fei", "Fei", "onetail", 0, 1, 0,
    "Cohens_g", "Cohen's g", "onetail", -0.5, 0.5, 0,
    "Cohens_h", "Cohen's h", "twotail", -pi, pi, 0,
    "Odds_ratio", "Odds ratio", "twotail", 0, Inf, 1,
    "log_Odds_ratio", "log(Odds ratio)", "twotail", -Inf, Inf, 0,
    "Risk_ratio", "Risk ratio", "twotail", 0, Inf, 1,
    "log_Risk_ratio", "log(Risk ratio)", "twotail", -Inf, Inf, 0,

    ## ANOVA
    "Eta2", "Eta2", "onetail", 0, 1, 0,
    "Eta2_partial", "Eta2 (partial)", "onetail", 0, 1, 0,
    "Eta2_generalized", "Eta2 (generalized)", "onetail", 0, 1, 0,
    "Epsilon2", "Epsilon2", "onetail", 0, 1, 0,
    "Epsilon2_partial", "Epsilon2 (partial)", "onetail", 0, 1, 0,
    "Omega2", "Omega2", "onetail", 0, 1, 0,
    "Omega2_partial", "Omega2 (partial)", "onetail", 0, 1, 0,
    "Cohens_f", "Cohen's f", "onetail", 0, Inf, 0,
    "Cohens_f_partial", "Cohen's f (partial)", "onetail", 0, Inf, 0,
    "Cohens_f2", "Cohen's f2", "onetail", 0, Inf, 0,
    "Cohens_f2_partial", "Cohen's f2 (partial)", "onetail", 0, Inf, 0,

    ## Rank
    "r_rank_biserial", "r (rank biserial)", "twotail", -1, 1, 0,
    "Kendalls_W", "Kendall's W", "onetail", 0, 1, 0,
    "rank_epsilon_squared", "Epsilon2 (rank)", "onetail", 0, 1, 0,

    ## Std Coefficient
    "Std_Coefficient", "Coefficient (std.)", "twotail", -Inf, Inf, 0,
    "Std_Odds_ratio", "Odds Ratio (std.)", "twotail", 0, Inf, 1,
    "Std_Risk_ratio", "Risk Ratio (std.)", "twotail", 0, Inf, 1,
    "Std_IRR", "IRR (std.)", "twotail", 0, Inf, 1,
    "Std_Median", "Median (std.)", "twotail", -Inf, Inf, 0,
    "Std_Mean", "Mean (std.)", "twotail", -Inf, Inf, 0,
    "Std_MAP", "MAP (std.)", "twotail", -Inf, Inf, 0,

    ## CLES
    "p_superiority", "Pr(superiority)", "twotail", 0, 1, 0.5,
    "Cohens_U1", "Cohen's U1", "onetail", 0, 1, 0,
    "Cohens_U2", "Cohen's U2", "onetail", 0.5, 1, 0.5,
    "Cohens_U3", "Cohen's U3", "twotail", 0, 1, 0.5,
    "overlap", "Overlap", "onetail", 0, 1, 1,

    ## Other
    "r", "r", "twotail", -1, 1, 0,
    "d", "d", "twotail", -Inf, Inf, 0
  ),
  ncol = 6, byrow = TRUE,
  dimnames = list(NULL, c("name", "label", "direction", "lb", "ub", "null"))
)
es_info <- as.data.frame(es_info, stringsAsFactors = FALSE)
es_info$lb <- as.numeric(es_info$lb)
es_info$ub <- as.numeric(es_info$ub)
es_info$null <- as.numeric(es_info$null)
rownames(es_info) <- es_info$name
