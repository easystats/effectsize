#' Checks for a Valid Effect Size Name
#'
#' For use by other functions and packages.
#'
#' @param x A character, or a vector.
#' @param ignore_case Should case of input be ignored?
#' @param use_symbols Should proper symbols be printed (`TRUE`) instead of
#'   transliterated effect size names (`FALSE`). See [effectsize_options].
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
get_effectsize_label <- function(x, ignore_case = TRUE, use_symbols = getOption("es.use_symbols", FALSE)) {
  x_comp <- es_info$name
  use_symbols <- .resolve_use_symbols(use_symbols)

  if (ignore_case) {
    x <- tolower(x)
    x_comp <- tolower(x_comp)
  }

  idx <- match(x, x_comp)
  es_info[idx, ifelse(use_symbols, "symbol", "label")]
}


# es_info -----------------------------------------------------------------


#' List of effect size names
#'
#' Can always add more info here if need be...
#'
#' @keywords internal
es_info <- matrix(
  c(
    ## Diffs
    "Cohens_d", "Cohen's d", NA, "twotail", -Inf, Inf, 0,
    "Hedges_g", "Hedges' g", NA, "twotail", -Inf, Inf, 0,
    "Glass_delta", "Glass' delta", "Glass' \u0394", "twotail", -Inf, Inf, 0,
    "Mahalanobis_D", "Mahalanobis' D", NA, "onetail", 0, Inf, 0,

    ## xtab cor
    "Cramers_v", "Cramer's V", NA, "onetail", 0, 1, 0,
    "Cramers_v_adjusted", "Cramer's V (adj.)", NA, "onetail", 0, 1, 0,
    "phi", "Phi", "\u03D5", "onetail", 0, 1, 0,
    "phi_adjusted", "Phi (adj.)", "\u03D5 (adj.)", "onetail", 0, 1, 0,
    "Pearsons_c", "Pearson's C", NA, "onetail", 0, 1, 0,
    "Cohens_w", "Cohen's w", NA, "onetail", 0, Inf, 0,
    "Fei", "Fei", "\u05E4\u200E", "onetail", 0, 1, 0,

    ## xtab 2x2
    "Cohens_h", "Cohen's h", NA, "twotail", -pi, pi, 0,
    "Odds_ratio", "Odds ratio", NA, "twotail", 0, Inf, 1,
    "log_Odds_ratio", "log(Odds ratio)", NA, "twotail", -Inf, Inf, 0,
    "Risk_ratio", "Risk ratio", NA, "twotail", 0, Inf, 1,
    "log_Risk_ratio", "log(Risk ratio)", NA, "twotail", -Inf, Inf, 0,

    ## xtab dep
    "Cohens_g", "Cohen's g", NA, "onetail", -0.5, 0.5, 0,

    ## ANOVA
    "Eta2", "Eta2", "\u03B7\u00b2", "onetail", 0, 1, 0,
    "Eta2_partial", "Eta2 (partial)", "\u03B7\u00b2 (partial)", "onetail", 0, 1, 0,
    "Eta2_generalized", "Eta2 (generalized)", "\u03B7\u00b2 (generalized)", "onetail", 0, 1, 0,
    "Epsilon2", "Epsilon2", "\u03F5\u00b2", "onetail", 0, 1, 0,
    "Epsilon2_partial", "Epsilon2 (partial)", "\u03F5\u00b2 (partial)", "onetail", 0, 1, 0,
    "Omega2", "Omega2", "\u03C9\u00b2", "onetail", 0, 1, 0,
    "Omega2_partial", "Omega2 (partial)", "\u03C9\u00b2 (partial)", "onetail", 0, 1, 0,
    "Cohens_f", "Cohen's f", NA, "onetail", 0, Inf, 0,
    "Cohens_f_partial", "Cohen's f (partial)", NA, "onetail", 0, Inf, 0,
    "Cohens_f2", "Cohen's f2", "Cohen's f\u00b2", "onetail", 0, Inf, 0,
    "Cohens_f2_partial", "Cohen's f2 (partial)", "Cohen's f\u00b2 (partial)", "onetail", 0, Inf, 0,

    ## Rank
    "r_rank_biserial", "r (rank biserial)", NA, "twotail", -1, 1, 0,
    "Kendalls_W", "Kendall's W", NA, "onetail", 0, 1, 0,
    "rank_epsilon_squared", "Epsilon2 (rank)", "\u03B5\u00b2(R)", "onetail", 0, 1, 0,
    "rank_eta_squared", "Eta2 (rank)", "\u03B7\u00b2(H)", "onetail", 0, 1, 0,

    ## CLES
    "p_superiority", "Pr(superiority)", NA, "twotail", 0, 1, 0.5,
    "Cohens_U1", "Cohen's U1", NA,  "onetail", 0, 1, 0,
    "Cohens_U2", "Cohen's U2", NA,  "onetail", 0.5, 1, 0.5,
    "Cohens_U3", "Cohen's U3", NA,  "twotail", 0, 1, 0.5,
    "overlap", "Overlap", NA,  "onetail", 0, 1, 1,

    ## Other
    "r", "r", NA,  "twotail", -1, 1, 0,
    "d", "d", NA,  "twotail", -Inf, Inf, 0,

    ## Std Coefficient
    "Std_Coefficient", "Coefficient (std.)", NA,  "twotail", -Inf, Inf, 0,
    "Std_Odds_ratio", "Odds Ratio (std.)", NA,  "twotail", 0, Inf, 1,
    "Std_Risk_ratio", "Risk Ratio (std.)", NA,  "twotail", 0, Inf, 1,
    "Std_IRR", "IRR (std.)", "twotail", NA,  0, Inf, 1,
    "Std_Median", "Median (std.)", NA,  "twotail", -Inf, Inf, 0,
    "Std_Mean", "Mean (std.)", NA,  "twotail", -Inf, Inf, 0,
    "Std_MAP", "MAP (std.)", NA,  "twotail", -Inf, Inf, 0
  ),
  ncol = 7, byrow = TRUE,
  dimnames = list(NULL, c("name", "label", "symbol", "direction", "lb", "ub", "null"))
)
es_info <- as.data.frame(es_info, stringsAsFactors = FALSE)
es_info$lb <- as.numeric(es_info$lb)
es_info$ub <- as.numeric(es_info$ub)
es_info$null <- as.numeric(es_info$null)
es_info$symbol[is.na(es_info$symbol)] <- es_info$label[is.na(es_info$symbol)]
rownames(es_info) <- es_info$name


# Utils -------------------------------------------------------------------


#' @keywords internal
#' @importFrom utils packageVersion
.resolve_use_symbols <- function(use_symbols) {
  use_symbols &&
    utils::packageVersion("base") >= package_version("4.2") &&
    Sys.info()["sysname"] != "windows"
}
