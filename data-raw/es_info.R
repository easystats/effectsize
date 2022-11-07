es_info <- tibble::tribble(
  ~name, ~label, ~symbol, ~direction, ~lb, ~ub, ~null,
  ## Std. diffs
  "Cohens_d", "Cohen's d", NA, "twotail", -Inf, Inf, 0,
  "Hedges_g", "Hedges' g", NA, "twotail", -Inf, Inf, 0,
  "Glass_delta", "Glass' delta", "Glass' \u0394", "twotail", -Inf, Inf, 0,
  "Mahalanobis_D", "Mahalanobis' D", NA, "onetail", 0, Inf, 0,
  "Means_ratio", "Means Ratio", NA, "twotail", 0, Inf, 1,
  "Means_ratio_adjusted", "Means Ratio (adj.)", NA, "twotail", 0, Inf, 1,
  "log_Means_ratio", "log(Means Ratio)", NA, "twotail", 0, Inf, 1,
  "log_Means_ratio_adjusted", "log(Means Ratio, adj.)", NA, "twotail", 0, Inf, 1,

  ## xtab cor
  "Cramers_v", "Cramer's V", NA, "onetail", 0, 1, 0,
  "Cramers_v_adjusted", "Cramer's V (adj.)", NA, "onetail", 0, 1, 0,
  "Tschuprows_t", "Tschuprow's T", NA, "onetail", 0, 1, 0,
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
  "Epsilon2", "Epsilon2", "\u03B5\u00b2", "onetail", 0, 1, 0,
  "Epsilon2_partial", "Epsilon2 (partial)", "\u03B5\u00b2 (partial)", "onetail", 0, 1, 0,
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
  "WMW_odds", "WMW Odds", NA, "twotail", 0, Inf, 1,
  "Cohens_U1", "Cohen's U1", NA, "onetail", 0, 1, 0,
  "Cohens_U2", "Cohen's U2", NA, "onetail", 0.5, 1, 0.5,
  "Cohens_U3", "Cohen's U3", NA, "twotail", 0, 1, 0.5,
  "overlap", "Overlap", NA, "onetail", 0, 1, 1,

  ## Other
  "r", "r", NA, "twotail", -1, 1, 0,
  "d", "d", NA, "twotail", -Inf, Inf, 0,

  ## Std Coefficient
  "Std_Coefficient", "Coefficient (std.)", NA, "twotail", -Inf, Inf, 0,
  "Std_Odds_ratio", "Odds Ratio (std.)", NA, "twotail", 0, Inf, 1,
  "Std_Risk_ratio", "Risk Ratio (std.)", NA, "twotail", 0, Inf, 1,
  "Std_IRR", "IRR (std.)", "twotail", NA, 0, Inf, 1,
  "Std_Median", "Median (std.)", NA, "twotail", -Inf, Inf, 0,
  "Std_Mean", "Mean (std.)", NA, "twotail", -Inf, Inf, 0,
  "Std_MAP", "MAP (std.)", NA, "twotail", -Inf, Inf, 0
)

es_info <- as.data.frame(es_info)
es_info[is.na(es_info[["symbol"]]), "symbol"] <- es_info[is.na(es_info[["symbol"]]), "label"]
rownames(es_info) <- es_info[["name"]]
usethis::use_data(es_info, internal = TRUE, overwrite = TRUE)
