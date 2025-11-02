# Package index

## Indices of Effect Size

- [`effectsize()`](https://easystats.github.io/effectsize/reference/effectsize.md)
  : Effect Sizes

### Standardized differences

Effect sizes for comparing two groups

- [`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md)
  [`hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.md)
  [`glass_delta()`](https://easystats.github.io/effectsize/reference/cohens_d.md)
  :

  Cohen's *d* and Other Standardized Differences

- [`repeated_measures_d()`](https://easystats.github.io/effectsize/reference/repeated_measures_d.md)
  [`rm_d()`](https://easystats.github.io/effectsize/reference/repeated_measures_d.md)
  : Standardized Mean Differences for Repeated Measures

- [`mahalanobis_d()`](https://easystats.github.io/effectsize/reference/mahalanobis_d.md)
  :

  Mahalanobis' *D* (a multivariate Cohen's *d*)

- [`rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.md)
  [`cliffs_delta()`](https://easystats.github.io/effectsize/reference/rank_biserial.md)
  : Dominance Effect Sizes for Rank Based Differences

- [`p_superiority()`](https://easystats.github.io/effectsize/reference/p_superiority.md)
  [`cohens_u1()`](https://easystats.github.io/effectsize/reference/p_superiority.md)
  [`cohens_u2()`](https://easystats.github.io/effectsize/reference/p_superiority.md)
  [`cohens_u3()`](https://easystats.github.io/effectsize/reference/p_superiority.md)
  [`p_overlap()`](https://easystats.github.io/effectsize/reference/p_superiority.md)
  [`vd_a()`](https://easystats.github.io/effectsize/reference/p_superiority.md)
  [`wmw_odds()`](https://easystats.github.io/effectsize/reference/p_superiority.md)
  :

  Cohen's *U*s and Other Common Language Effect Sizes (CLES)

- [`means_ratio()`](https://easystats.github.io/effectsize/reference/means_ratio.md)
  : Ratio of Means

### For contingency tables

Effect sizes for categorical outcomes

- [`phi()`](https://easystats.github.io/effectsize/reference/phi.md)
  [`cramers_v()`](https://easystats.github.io/effectsize/reference/phi.md)
  [`tschuprows_t()`](https://easystats.github.io/effectsize/reference/phi.md)
  [`cohens_w()`](https://easystats.github.io/effectsize/reference/phi.md)
  [`fei()`](https://easystats.github.io/effectsize/reference/phi.md)
  [`pearsons_c()`](https://easystats.github.io/effectsize/reference/phi.md)
  : \\\phi\\ and Other Contingency Tables Correlations
- [`oddsratio()`](https://easystats.github.io/effectsize/reference/oddsratio.md)
  [`riskratio()`](https://easystats.github.io/effectsize/reference/oddsratio.md)
  [`cohens_h()`](https://easystats.github.io/effectsize/reference/oddsratio.md)
  [`arr()`](https://easystats.github.io/effectsize/reference/oddsratio.md)
  [`nnt()`](https://easystats.github.io/effectsize/reference/oddsratio.md)
  : Odds Ratios, Risk Ratios and Other Effect Sizes for 2-by-2
  Contingency Tables
- [`cohens_g()`](https://easystats.github.io/effectsize/reference/cohens_g.md)
  : Effect Size for Paired Contingency Tables

### Comparing multiple groups

Effect sizes for ANOVA

- [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  [`omega_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  [`epsilon_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  [`cohens_f()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  [`cohens_f_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  [`eta_squared_posterior()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  : \\\eta^2\\ and Other Effect Size for ANOVA
- [`rank_epsilon_squared()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.md)
  [`rank_eta_squared()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.md)
  [`kendalls_w()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.md)
  : Effect Size for Rank Based ANOVA

### Standardized parameters

Functions from other packages relating to parameter standardization and
perdictor dominance

- [`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html)
  [`standardize_posteriors()`](https://easystats.github.io/parameters/reference/standardize_parameters.html)
  : Parameters standardization (from parameters)
- [`standardize(`*`<default>`*`)`](https://easystats.github.io/datawizard/reference/standardize.default.html)
  : Re-fit a model with standardized data (from datawizard)
- [`standardize_info()`](https://easystats.github.io/parameters/reference/standardize_info.html)
  : Get Standardization Information (from parameters)
- [`dominance_analysis()`](https://easystats.github.io/parameters/reference/dominance_analysis.html)
  : Dominance Analysis (from parameters)
- [`r2_semipartial()`](https://easystats.github.io/effectsize/reference/r2_semipartial.md)
  : Semi-Partial (Part) Correlation Squared (\\\Delta R^2\\)

### Correlations

Correlations are a standardized effect size of association

- [`correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
  : Correlation Analysis (from correlation)
- [`cor_test()`](https://easystats.github.io/correlation/reference/cor_test.html)
  : Correlation test (from correlation)

## Effect Size Conversion

### From Test Statistics

Extract approximate effect sizes from their commonly associated test
statistics

- [`t_to_d()`](https://easystats.github.io/effectsize/reference/t_to_r.md)
  [`z_to_d()`](https://easystats.github.io/effectsize/reference/t_to_r.md)
  [`F_to_d()`](https://easystats.github.io/effectsize/reference/t_to_r.md)
  [`t_to_r()`](https://easystats.github.io/effectsize/reference/t_to_r.md)
  [`z_to_r()`](https://easystats.github.io/effectsize/reference/t_to_r.md)
  [`F_to_r()`](https://easystats.github.io/effectsize/reference/t_to_r.md)
  :

  Convert *t*, *z*, and *F* to Cohen's *d* or **partial**-*r*

- [`F_to_eta2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)
  [`t_to_eta2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)
  [`F_to_epsilon2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)
  [`t_to_epsilon2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)
  [`F_to_eta2_adj()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)
  [`t_to_eta2_adj()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)
  [`F_to_omega2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)
  [`t_to_omega2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)
  [`F_to_f()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)
  [`t_to_f()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)
  [`F_to_f2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)
  [`t_to_f2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)
  :

  Convert *F* and *t* Statistics to **partial**-\\\eta^2\\ and Other
  ANOVA Effect Sizes

- [`chisq_to_phi()`](https://easystats.github.io/effectsize/reference/chisq_to_phi.md)
  [`chisq_to_cohens_w()`](https://easystats.github.io/effectsize/reference/chisq_to_phi.md)
  [`chisq_to_cramers_v()`](https://easystats.github.io/effectsize/reference/chisq_to_phi.md)
  [`chisq_to_tschuprows_t()`](https://easystats.github.io/effectsize/reference/chisq_to_phi.md)
  [`chisq_to_fei()`](https://easystats.github.io/effectsize/reference/chisq_to_phi.md)
  [`chisq_to_pearsons_c()`](https://easystats.github.io/effectsize/reference/chisq_to_phi.md)
  [`phi_to_chisq()`](https://easystats.github.io/effectsize/reference/chisq_to_phi.md)
  : Convert \\\chi^2\\ to \\\phi\\ and Other Correlation-like Effect
  Sizes

### Between Effect Sizes

Approximate effect sizes by converting between other related effect
sizes

- [`d_to_p_superiority()`](https://easystats.github.io/effectsize/reference/diff_to_cles.md)
  [`rb_to_p_superiority()`](https://easystats.github.io/effectsize/reference/diff_to_cles.md)
  [`rb_to_vda()`](https://easystats.github.io/effectsize/reference/diff_to_cles.md)
  [`d_to_u2()`](https://easystats.github.io/effectsize/reference/diff_to_cles.md)
  [`d_to_u1()`](https://easystats.github.io/effectsize/reference/diff_to_cles.md)
  [`d_to_u3()`](https://easystats.github.io/effectsize/reference/diff_to_cles.md)
  [`d_to_overlap()`](https://easystats.github.io/effectsize/reference/diff_to_cles.md)
  [`rb_to_wmw_odds()`](https://easystats.github.io/effectsize/reference/diff_to_cles.md)
  : Convert Standardized Differences to Common Language Effect Sizes

- [`eta2_to_f2()`](https://easystats.github.io/effectsize/reference/eta2_to_f2.md)
  [`eta2_to_f()`](https://easystats.github.io/effectsize/reference/eta2_to_f2.md)
  [`f2_to_eta2()`](https://easystats.github.io/effectsize/reference/eta2_to_f2.md)
  [`f_to_eta2()`](https://easystats.github.io/effectsize/reference/eta2_to_f2.md)
  : Convert Between ANOVA Effect Sizes

- [`w_to_fei()`](https://easystats.github.io/effectsize/reference/w_to_fei.md)
  [`w_to_v()`](https://easystats.github.io/effectsize/reference/w_to_fei.md)
  [`w_to_t()`](https://easystats.github.io/effectsize/reference/w_to_fei.md)
  [`w_to_c()`](https://easystats.github.io/effectsize/reference/w_to_fei.md)
  [`fei_to_w()`](https://easystats.github.io/effectsize/reference/w_to_fei.md)
  [`v_to_w()`](https://easystats.github.io/effectsize/reference/w_to_fei.md)
  [`t_to_w()`](https://easystats.github.io/effectsize/reference/w_to_fei.md)
  [`c_to_w()`](https://easystats.github.io/effectsize/reference/w_to_fei.md)
  [`v_to_t()`](https://easystats.github.io/effectsize/reference/w_to_fei.md)
  [`t_to_v()`](https://easystats.github.io/effectsize/reference/w_to_fei.md)
  : Convert Between Effect Sizes for Contingency Tables Correlations

- [`d_to_r()`](https://easystats.github.io/effectsize/reference/d_to_r.md)
  [`r_to_d()`](https://easystats.github.io/effectsize/reference/d_to_r.md)
  [`oddsratio_to_d()`](https://easystats.github.io/effectsize/reference/d_to_r.md)
  [`logoddsratio_to_d()`](https://easystats.github.io/effectsize/reference/d_to_r.md)
  [`d_to_oddsratio()`](https://easystats.github.io/effectsize/reference/d_to_r.md)
  [`d_to_logoddsratio()`](https://easystats.github.io/effectsize/reference/d_to_r.md)
  [`oddsratio_to_r()`](https://easystats.github.io/effectsize/reference/d_to_r.md)
  [`logoddsratio_to_r()`](https://easystats.github.io/effectsize/reference/d_to_r.md)
  [`r_to_oddsratio()`](https://easystats.github.io/effectsize/reference/d_to_r.md)
  [`r_to_logoddsratio()`](https://easystats.github.io/effectsize/reference/d_to_r.md)
  :

  Convert Between *d*, *r*, and Odds Ratio

- [`oddsratio_to_riskratio()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
  [`oddsratio_to_arr()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
  [`oddsratio_to_nnt()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
  [`logoddsratio_to_riskratio()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
  [`logoddsratio_to_arr()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
  [`logoddsratio_to_nnt()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
  [`riskratio_to_oddsratio()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
  [`riskratio_to_arr()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
  [`riskratio_to_logoddsratio()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
  [`riskratio_to_nnt()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
  [`arr_to_riskratio()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
  [`arr_to_oddsratio()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
  [`arr_to_logoddsratio()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
  [`arr_to_nnt()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
  [`nnt_to_oddsratio()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
  [`nnt_to_logoddsratio()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
  [`nnt_to_riskratio()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
  [`nnt_to_arr()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
  : Convert Between Odds Ratios, Risk Ratios and Other Metrics of Change
  in Probabilities

- [`oddsratio_to_probs()`](https://easystats.github.io/effectsize/reference/oddsratio_to_probs.md)
  [`logoddsratio_to_probs()`](https://easystats.github.io/effectsize/reference/oddsratio_to_probs.md)
  [`riskratio_to_probs()`](https://easystats.github.io/effectsize/reference/oddsratio_to_probs.md)
  [`arr_to_probs()`](https://easystats.github.io/effectsize/reference/oddsratio_to_probs.md)
  [`nnt_to_probs()`](https://easystats.github.io/effectsize/reference/oddsratio_to_probs.md)
  : Convert Between Metrics of Change in Probabilities and Probabilities

- [`odds_to_probs()`](https://easystats.github.io/effectsize/reference/odds_to_probs.md)
  [`probs_to_odds()`](https://easystats.github.io/effectsize/reference/odds_to_probs.md)
  : Convert Between Odds and Probabilities

## Interpretation

- [`equivalence_test(`*`<effectsize_table>`*`)`](https://easystats.github.io/effectsize/reference/equivalence_test.effectsize_table.md)
  : Test Effect Size for Practical Equivalence to the Null

- [`interpret()`](https://easystats.github.io/effectsize/reference/interpret.md)
  : Generic Function for Interpretation

- [`rules()`](https://easystats.github.io/effectsize/reference/rules.md)
  [`is.rules()`](https://easystats.github.io/effectsize/reference/rules.md)
  : Create an Interpretation Grid

- [`interpret_bf()`](https://easystats.github.io/effectsize/reference/interpret_bf.md)
  : Interpret Bayes Factor (BF)

- [`interpret_cohens_d()`](https://easystats.github.io/effectsize/reference/interpret_cohens_d.md)
  [`interpret_hedges_g()`](https://easystats.github.io/effectsize/reference/interpret_cohens_d.md)
  [`interpret_glass_delta()`](https://easystats.github.io/effectsize/reference/interpret_cohens_d.md)
  : Interpret Standardized Differences

- [`interpret_cohens_g()`](https://easystats.github.io/effectsize/reference/interpret_cohens_g.md)
  :

  Interpret Cohen's *g*

- [`interpret_direction()`](https://easystats.github.io/effectsize/reference/interpret_direction.md)
  : Interpret Direction

- [`interpret_ess()`](https://easystats.github.io/effectsize/reference/interpret_ess.md)
  [`interpret_rhat()`](https://easystats.github.io/effectsize/reference/interpret_ess.md)
  : Interpret Bayesian Diagnostic Indices

- [`interpret_gfi()`](https://easystats.github.io/effectsize/reference/interpret_gfi.md)
  [`interpret_agfi()`](https://easystats.github.io/effectsize/reference/interpret_gfi.md)
  [`interpret_nfi()`](https://easystats.github.io/effectsize/reference/interpret_gfi.md)
  [`interpret_nnfi()`](https://easystats.github.io/effectsize/reference/interpret_gfi.md)
  [`interpret_cfi()`](https://easystats.github.io/effectsize/reference/interpret_gfi.md)
  [`interpret_rfi()`](https://easystats.github.io/effectsize/reference/interpret_gfi.md)
  [`interpret_ifi()`](https://easystats.github.io/effectsize/reference/interpret_gfi.md)
  [`interpret_pnfi()`](https://easystats.github.io/effectsize/reference/interpret_gfi.md)
  [`interpret_rmsea()`](https://easystats.github.io/effectsize/reference/interpret_gfi.md)
  [`interpret_srmr()`](https://easystats.github.io/effectsize/reference/interpret_gfi.md)
  [`interpret(`*`<lavaan>`*`)`](https://easystats.github.io/effectsize/reference/interpret_gfi.md)
  [`interpret(`*`<performance_lavaan>`*`)`](https://easystats.github.io/effectsize/reference/interpret_gfi.md)
  : Interpret of CFA / SEM Indices of Goodness of Fit

- [`interpret_icc()`](https://easystats.github.io/effectsize/reference/interpret_icc.md)
  : Interpret Intraclass Correlation Coefficient (ICC)

- [`interpret_kendalls_w()`](https://easystats.github.io/effectsize/reference/interpret_kendalls_w.md)
  :

  Interpret Kendall's Coefficient of Concordance *W*

- [`interpret_oddsratio()`](https://easystats.github.io/effectsize/reference/interpret_oddsratio.md)
  : Interpret Odds Ratio

- [`interpret_omega_squared()`](https://easystats.github.io/effectsize/reference/interpret_omega_squared.md)
  [`interpret_eta_squared()`](https://easystats.github.io/effectsize/reference/interpret_omega_squared.md)
  [`interpret_epsilon_squared()`](https://easystats.github.io/effectsize/reference/interpret_omega_squared.md)
  [`interpret_r2_semipartial()`](https://easystats.github.io/effectsize/reference/interpret_omega_squared.md)
  : Interpret ANOVA Effect Sizes

- [`interpret_p()`](https://easystats.github.io/effectsize/reference/interpret_p.md)
  :

  Interpret *p*-Values

- [`interpret_pd()`](https://easystats.github.io/effectsize/reference/interpret_pd.md)
  : Interpret Probability of Direction (pd)

- [`interpret_r()`](https://easystats.github.io/effectsize/reference/interpret_r.md)
  [`interpret_phi()`](https://easystats.github.io/effectsize/reference/interpret_r.md)
  [`interpret_cramers_v()`](https://easystats.github.io/effectsize/reference/interpret_r.md)
  [`interpret_rank_biserial()`](https://easystats.github.io/effectsize/reference/interpret_r.md)
  [`interpret_fei()`](https://easystats.github.io/effectsize/reference/interpret_r.md)
  : Interpret Correlation Coefficient

- [`interpret_r2()`](https://easystats.github.io/effectsize/reference/interpret_r2.md)
  : Interpret Coefficient of Determination (\\R^2\\)

- [`interpret_rope()`](https://easystats.github.io/effectsize/reference/interpret_rope.md)
  : Interpret Bayesian Posterior Percentage in ROPE.

- [`interpret_vif()`](https://easystats.github.io/effectsize/reference/interpret_vif.md)
  : Interpret the Variance Inflation Factor (VIF)

## Miscellaneous

- [`sd_pooled()`](https://easystats.github.io/effectsize/reference/sd_pooled.md)
  [`mad_pooled()`](https://easystats.github.io/effectsize/reference/sd_pooled.md)
  [`cov_pooled()`](https://easystats.github.io/effectsize/reference/sd_pooled.md)
  : Pooled Indices of (Co)Deviation

- [`is_effectsize_name()`](https://easystats.github.io/effectsize/reference/is_effectsize_name.md)
  [`get_effectsize_name()`](https://easystats.github.io/effectsize/reference/is_effectsize_name.md)
  [`get_effectsize_label()`](https://easystats.github.io/effectsize/reference/is_effectsize_name.md)
  : Checks for a Valid Effect Size Name

- [`format_standardize()`](https://easystats.github.io/effectsize/reference/format_standardize.md)
  : Format a Standardized Vector

- [`plot(`*`<effectsize_table>`*`)`](https://easystats.github.io/effectsize/reference/print.effectsize_table.md)
  [`print(`*`<effectsize_table>`*`)`](https://easystats.github.io/effectsize/reference/print.effectsize_table.md)
  [`print_md(`*`<effectsize_table>`*`)`](https://easystats.github.io/effectsize/reference/print.effectsize_table.md)
  [`print_html(`*`<effectsize_table>`*`)`](https://easystats.github.io/effectsize/reference/print.effectsize_table.md)
  [`format(`*`<effectsize_table>`*`)`](https://easystats.github.io/effectsize/reference/print.effectsize_table.md)
  [`print(`*`<effectsize_difference>`*`)`](https://easystats.github.io/effectsize/reference/print.effectsize_table.md)
  :

  Methods for [effectsize](https://easystats.github.io/effectsize/)
  Tables

- [`effectsize_options`](https://easystats.github.io/effectsize/reference/effectsize_options.md)
  :

  `effectsize` options

## Datasets

- [`hardlyworking`](https://easystats.github.io/effectsize/reference/hardlyworking.md)
  : Workers' Salary and Other Information
- [`rouder2016`](https://easystats.github.io/effectsize/reference/rouder2016.md)
  : Jeff Rouder's Example Dataset for Repeated Measures
- [`screening_test`](https://easystats.github.io/effectsize/reference/screening_test.md)
  : Results from 2 Screening Tests
- [`RCT_table`](https://easystats.github.io/effectsize/reference/RCT_table.md)
  : Fictional Results from a Workers' Randomized Control Trial
- [`Music_preferences`](https://easystats.github.io/effectsize/reference/Music_preferences.md)
  : Music Preference by College Major
- [`Music_preferences2`](https://easystats.github.io/effectsize/reference/Music_preferences2.md)
  : Music Preference by College Major
- [`Smoking_FASD`](https://easystats.github.io/effectsize/reference/Smoking_FASD.md)
  : Frequency of FASD for Smoking Mothers
- [`food_class`](https://easystats.github.io/effectsize/reference/food_class.md)
  : Classification of Foods
