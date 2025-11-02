# Changelog

## effectsize 1.0.x

### New features

- `oddsratio_to_*()` and `riskratio_to_*()` can now convert binomial
  models with logit- or log-links (respectively) to any of RR, OR, ARR,
  NNT.
- New functions to convert between measures of change in probabilities
  and probabilities. See
  [`?oddsratio_to_probs`](https://easystats.github.io/effectsize/reference/oddsratio_to_probs.md)
  for all available functions.
- `effetsize()` and friends support
  [`datawizard::data_tabulate()`](https://easystats.github.io/datawizard/reference/data_tabulate.html)
  objects as inputs.

### Changes

`riskratio_to_*()` now returns `NA` if the expected risk is larger
than 1. This results when impossible combinations of risk ratio `RR` and
baseline risk `p0` are provided.

### Breaking Changes

- `probs_to_odds(<data.frame>)` and `odds_to_probs(<data.frame>)`
  methods has been deprecated.
- `riskratio(log=)` argument has been deprecated.
- `convert_*()` aliases, deprecated since March 2023, have been removed.

## effectsize 1.0.1

CRAN release: 2025-05-27

### New features

- [`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md),
  [`p_superiority()`](https://easystats.github.io/effectsize/reference/p_superiority.md),
  [`rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.md)
  and their relatives gain a `reference=` argument to control which
  level of the group variable should be treated as the reference (thanks
  [@profandyfield](https://github.com/profandyfield) for the
  suggestion).

### Changes

- Fixed failing tests related to the recent update of the *parameters*
  package.

## effectsize 1.0.0

CRAN release: 2024-12-10

***First stable release of
[effectsize](https://easystats.github.io/effectsize/)!***

### New features

- [`oddsratio_to_d()`](https://easystats.github.io/effectsize/reference/d_to_r.md)
  and related functions gain a `p0` argument for exact conversion
  between odds ratios and Cohen’s *d* (thanks
  [@KohlRaphael](https://github.com/KohlRaphael) for the suggestion).
- `interpret*()` now accept (and return) matrices and arrays.

### Breaking Changes

- [`interpret_oddsratio()`](https://easystats.github.io/effectsize/reference/interpret_oddsratio.md)
  drops the default `"chen2010"` as it was used incorrectly (thanks to
  [@KohlRaphael](https://github.com/KohlRaphael)).
- Functions that have been deprecated since *September 2022* have been
  removed.

## effectsize 0.8.9

CRAN release: 2024-07-03

### Bug fixes

- `interpret(<effectsize_table>)` no longer returns transformed effect
  sizes ( [\#640](https://github.com/easystats/effectsize/issues/640) )

## effectsize 0.8.8

CRAN release: 2024-05-12

### Bug fixes

- [`hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.md),
  [`vd_a()`](https://easystats.github.io/effectsize/reference/p_superiority.md),
  [`wmw_odds()`](https://easystats.github.io/effectsize/reference/p_superiority.md),
  and
  [`cliffs_delta()`](https://easystats.github.io/effectsize/reference/rank_biserial.md)
  no longer require
  [effectsize](https://easystats.github.io/effectsize/) to be loaded to
  work ( [\#636](https://github.com/easystats/effectsize/issues/636) ).

### New features

- `effectsize(<t.test>)` now accepts a `data=` argument for when the
  `t.test(<formula>)` method was used.

## effectsize 0.8.7

CRAN release: 2024-04-01

- This release changes the licensing model of
  [effectsize](https://easystats.github.io/effectsize/) to an MIT
  license.

### New features

- [`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md)
  and
  [`glass_delta()`](https://easystats.github.io/effectsize/reference/cohens_d.md)
  gain an `adjust` argument for applying Hedges’ small-sample bias
  correction
  ([`hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.md)
  is now an alias for `cohens_d(adjust = TRUE)`).
- [`repeated_measures_d()`](https://easystats.github.io/effectsize/reference/repeated_measures_d.md)
  to compute standardized mean differences (SMD) for repeated measures
  data.
  - Also supported in `effectsize(<t.test(paired = TRUE)>)`
- New function:
  [`interpret_fei()`](https://easystats.github.io/effectsize/reference/interpret_r.md)

### Bug fixes

- Minor stability fix to ncp-based CI methods (
  [\#628](https://github.com/easystats/effectsize/issues/628) )
- [`nnt()`](https://easystats.github.io/effectsize/reference/oddsratio.md)
  now properly accepts the `y` argument.

## effectsize 0.8.6

CRAN release: 2023-09-14

This is a minor update to bring `effectsize` in-line with the formula
methods in [`t.test()`](https://rdrr.io/r/stats/t.test.html) and
[`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) in
`R>=4.4.0`.

### Breaking Changes

- [`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md),
  [`hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.md),
  [`p_superiority()`](https://easystats.github.io/effectsize/reference/p_superiority.md),
  [`wmw_odds()`](https://easystats.github.io/effectsize/reference/p_superiority.md),
  [`means_ratio()`](https://easystats.github.io/effectsize/reference/means_ratio.md)
  and
  [`rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.md)
  no longer support setting `paired = TRUE` when using the formula
  method.

### Bug fixes

- `eta_squared(<gam>)` returns (approximate) effect sizes for smooths.

## effectsize 0.8.5

CRAN release: 2023-08-09

### New features

- [`interpret_cfi()`](https://easystats.github.io/effectsize/reference/interpret_gfi.md)
  gains a new rule option: `"hu&bentler1999"` (
  [\#538](https://github.com/easystats/effectsize/issues/538) ).
- [`cohens_f()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  added option to return unbiased estimators (based on Omega- or
  Epsilon-squared).
- [`tschuprows_t()`](https://easystats.github.io/effectsize/reference/phi.md)
  now returns an effect size corrected for small-sample bias. Set
  `adjust = FALSE` to preserve old behavior.
- [`w_to_v()`](https://easystats.github.io/effectsize/reference/w_to_fei.md)
  and others for converting between effect sizes of Chi-square tests.
- [`arr()`](https://easystats.github.io/effectsize/reference/oddsratio.md)
  and
  [`nnt()`](https://easystats.github.io/effectsize/reference/oddsratio.md)
  for Absolute Risk Reduction or Number Needed to Treat.
- [`oddsratio_to_arr()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md),
  [`riskratio_to_arr()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md),
  [`nnt_to_arr()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
  and their inverses.
- `logoddsratio_to_*()` and `*_to_logoddsratio()` have been added as
  convenient shortcuts for `oddsratio_to_*(log = TRUE)` and
  `*_to_oddsratio(log = TRUE)`.
- Added all missing functions to convert between (log) OR, RR, ARR, and
  NNT.

### Changes

- [`fei()`](https://easystats.github.io/effectsize/reference/phi.md)
  gives a more informative error method for invalid table inputs
  ([\#566](https://github.com/easystats/effectsize/issues/566)).
- `convert_*()` aliases are deprecated.

### Breaking Changes

- `*_to_riskratio()` and `riskratio_to_*()` argument `log` not longer
  converts RR to/from log(RR).
- [`interpret_gfi()`](https://easystats.github.io/effectsize/reference/interpret_gfi.md)
  and friends: some previously named `"default"` rules have been
  re-labelled as `"byrne1994"`.

### Bug fixes

- [`riskratio()`](https://easystats.github.io/effectsize/reference/oddsratio.md)
  returns correct CIs
  ([\#584](https://github.com/easystats/effectsize/issues/584))
- [`d_to_r()`](https://easystats.github.io/effectsize/reference/d_to_r.md)
  correctly treats specifying only `n1`/`n2` as equal group sizes
  ([\#571](https://github.com/easystats/effectsize/issues/571))

## effectsize 0.8.3

CRAN release: 2023-01-28

### Changes

- [`mahalanobis_d()`](https://easystats.github.io/effectsize/reference/mahalanobis_d.md)
  now defaults to one-sided CIs.

### New features

- [`means_ratio()`](https://easystats.github.io/effectsize/reference/means_ratio.md)
  for computing ratios of two means for ratio-scales outcomes (thanks to
  [@arcaldwell49](https://github.com/arcaldwell49)!)
- [`r_to_d()`](https://easystats.github.io/effectsize/reference/d_to_r.md)
  family of functions gain arguments for specifying group size (
  [\#534](https://github.com/easystats/effectsize/issues/534) )
- `r2_semipartial` for semi-partial squared correlations of model terms
  / parameters.

### Bug fixes

- ANOVA effect sizes for
  [`afex::mixed()`](https://rdrr.io/pkg/afex/man/mixed.html) now return
  effect sizes for the Intercept where applicable.
- Fixed error in
  [`cohens_w()`](https://easystats.github.io/effectsize/reference/phi.md)
  for 2-by-X tables.
- Solved integer overflow errors in
  [`rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.md)
  ( [\#476](https://github.com/easystats/effectsize/issues/476) )
- Fixed issue in
  [`effectsize()`](https://easystats.github.io/effectsize/reference/effectsize.md)
  for t-tests when input vectors has unequal amount of missing values.

## effectsize 0.8.2

CRAN release: 2022-10-31

### Breaking Changes

- [`omega_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  and
  [`epsilon_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  (and
  [`F_to_omega2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)
  and
  [`F_to_epsilon2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md))
  always return non-negative estimates (previously estimates were
  negative when the observed effect size is very small).
- [`rank_eta_squared()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.md)
  always returns a non-negative estimate (previously estimates were
  negative when the observed effect size is very small).

## effectsize 0.8.1

CRAN release: 2022-10-18

### Changes

- [`cohens_w()`](https://easystats.github.io/effectsize/reference/phi.md)
  has an exact upper bound when used as an effect size for
  goodness-of-fit.

### Bug fixes

- When using formula input to effect size function, `na.action`
  arguments are respected
  ([\#517](https://github.com/easystats/effectsize/issues/517))

## effectsize 0.8.0

CRAN release: 2022-10-09

### Breaking Changes

- [effectsize](https://easystats.github.io/effectsize/) now requires
  *`R >= 3.6`*
- [`fei()`](https://easystats.github.io/effectsize/reference/phi.md),
  [`cohens_w()`](https://easystats.github.io/effectsize/reference/phi.md)
  and
  [`pearsons_c()`](https://easystats.github.io/effectsize/reference/phi.md)
  always rescale the `p` input to sum-to-1.
- The order of some function arguments have been rearranged to be more
  consistent across functions:
  ([`phi()`](https://easystats.github.io/effectsize/reference/phi.md),
  [`cramers_v()`](https://easystats.github.io/effectsize/reference/phi.md),
  [`p_superiority()`](https://easystats.github.io/effectsize/reference/p_superiority.md),
  [`cohens_u3()`](https://easystats.github.io/effectsize/reference/p_superiority.md),
  [`p_overlap()`](https://easystats.github.io/effectsize/reference/p_superiority.md),
  [`rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.md),
  `cohens_f/_squared()`,
  [`chisq_to_phi()`](https://easystats.github.io/effectsize/reference/chisq_to_phi.md),
  [`chisq_to_cramers_v()`](https://easystats.github.io/effectsize/reference/chisq_to_phi.md),
  `F/t_to_f/2()`, `.es_aov_*()`).
- `normalized_chi()` has been renamed
  [`fei()`](https://easystats.github.io/effectsize/reference/phi.md).
- `cles`, `d_to_cles` and `rb_to_cles` are deprecated in favor of their
  respective effect size functions.

### Changes

- [`phi()`](https://easystats.github.io/effectsize/reference/phi.md) and
  [`cramers_v()`](https://easystats.github.io/effectsize/reference/phi.md)
  (and `chisq_to_phi/cramers_v()`) now apply the small-sample bias
  correction by default. To restore previous behavior, set
  `adjust = FALSE`.

### New features

- Set `options(es.use_symbols = TRUE)` to print proper symbols instead
  of transliterated effect size names. (On Windows, requires
  `R >= 4.2.0`)
- [`effectsize()`](https://easystats.github.io/effectsize/reference/effectsize.md)
  supports [`fisher.test()`](https://rdrr.io/r/stats/fisher.test.html).
- New datasets used in examples and vignettes - see
  `data(package = "effectsize")`.
- [`tschuprows_t()`](https://easystats.github.io/effectsize/reference/phi.md)
  and
  [`chisq_to_tschuprows_t()`](https://easystats.github.io/effectsize/reference/chisq_to_phi.md)
  for computing Tschuprow’s *T* - a relative of Cramer’s *V*.
- [`mahalanobis_d()`](https://easystats.github.io/effectsize/reference/mahalanobis_d.md)
  for multivariate standardized differences.
- Rank based effect sizes now accept ordered
  ([`ordered()`](https://rdrr.io/r/base/factor.html)) outcomes.
- [`rank_eta_squared()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.md)
  for one-way rank ANOVA.
- For Common Language Effect Sizes:
  - [`wmw_odds()`](https://easystats.github.io/effectsize/reference/p_superiority.md)
    and `rb_to_wmw_odds` for the Wilcoxon-Mann-Whitney odds (thanks
    [@arcaldwell49](https://github.com/arcaldwell49)!
    [\#479](https://github.com/easystats/effectsize/issues/479)).
  - [`p_superiority()`](https://easystats.github.io/effectsize/reference/p_superiority.md)
    now supports paired and one-sample cases.
  - [`vd_a()`](https://easystats.github.io/effectsize/reference/p_superiority.md)
    and
    [`rb_to_vda()`](https://easystats.github.io/effectsize/reference/diff_to_cles.md)
    for Vargha and Delaney’s *A* dominance effect size (aliases for
    `p_superiority(parametric = FALSE)` and
    [`rb_to_p_superiority()`](https://easystats.github.io/effectsize/reference/diff_to_cles.md)).
  - [`cohens_u1()`](https://easystats.github.io/effectsize/reference/p_superiority.md),
    [`cohens_u2()`](https://easystats.github.io/effectsize/reference/p_superiority.md),
    [`d_to_u1()`](https://easystats.github.io/effectsize/reference/diff_to_cles.md),
    and
    [`d_to_u2()`](https://easystats.github.io/effectsize/reference/diff_to_cles.md)
    added for Cohen’s U1 and U2.

### Bug fixes

- Common-language effect sizes now respects `mu` argument for all effect
  sizes.
- [`mad_pooled()`](https://easystats.github.io/effectsize/reference/sd_pooled.md)
  not returns correct value (previously was inflated by a factor of
  1.4826).
- [`pearsons_c()`](https://easystats.github.io/effectsize/reference/phi.md)
  and
  [`chisq_to_pearsons_c()`](https://easystats.github.io/effectsize/reference/chisq_to_phi.md)
  lose the `adjust` argument which applied an irrelevant adjustment to
  the effect size.
- Effect sizes for goodness-of-fit now work when passing a `p` that is a
  table.

## effectsize 0.7.0.5

CRAN release: 2022-08-10

### Breaking Changes

`effectsize` now requires minimal *`R`* version of `3.5`.

### Bug fixes

- [`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md)
  for paired / one sample now gives more accurate CIs (was off by a
  factor of `(N - 1) / N`;
  [\#457](https://github.com/easystats/effectsize/issues/457))
- [`kendalls_w()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.md)
  now deals correctly with singular ties
  ([\#448](https://github.com/easystats/effectsize/issues/448)).

## effectsize 0.7.0

CRAN release: 2022-05-26

### Breaking Changes

- **[`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html),
  [`standardize_posteriors()`](https://easystats.github.io/parameters/reference/standardize_parameters.html),
  &
  [`standardize_info()`](https://easystats.github.io/parameters/reference/standardize_info.html)
  have been moved to the `parameters` package.**
- **[`standardize()`](https://easystats.github.io/datawizard/reference/standardize.html)
  (for models) has been moved to the `datawizard` package.**
- [`phi()`](https://easystats.github.io/effectsize/reference/phi.md)
  only works for 2x2 tables.
- [`cramers_v()`](https://easystats.github.io/effectsize/reference/phi.md)
  only works for 2D tables.

### New features

- `normalized_chi()` gives an adjusted Cohen’s *w* for goodness of fit.
- [`cohens_w()`](https://easystats.github.io/effectsize/reference/phi.md)
  is now a fully-fledged function for x-tables and goodness-of-fit
  effect size (not just an alias for
  [`phi()`](https://easystats.github.io/effectsize/reference/phi.md)).
- Support for `insight`’s `display`, `print_md` and `print_html` for all
  [effectsize](https://easystats.github.io/effectsize/) outputs.

### Bug fixes

- [`kendalls_w()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.md)
  now deals with ties.
- [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  works with [`car::Manova()`](https://rdrr.io/pkg/car/man/Anova.html)
  that does not have an i-design.

## effectsize 0.6.0.1

CRAN release: 2022-01-26

*This is a patch release.*

### Bug fixes

- [`interpret.performance_lavaan()`](https://easystats.github.io/effectsize/reference/interpret_gfi.md)
  now works without attaching `effectsize` (
  [\#410](https://github.com/easystats/effectsize/issues/410) ).
- [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  now fully support multi-variate `car` ANOVAs (class `Anova.mlm`;
  [\#406](https://github.com/easystats/effectsize/issues/406) ).

## effectsize 0.6.0

CRAN release: 2022-01-14

### Breaking Changes

- [`pearsons_c()`](https://easystats.github.io/effectsize/reference/phi.md)
  effect size column name changed to `Pearsons_c` for consistency.

### New features

#### New API

See [*Support functions for model extensions*
vignette](https://easystats.github.io/effectsize/articles/effectsize_API.html).

#### Other features

- [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  family now supports
  [`afex::mixed()`](https://rdrr.io/pkg/afex/man/mixed.html) models.
- [`cles()`](https://easystats.github.io/effectsize/reference/p_superiority.md)
  for estimating common language effect sizes.
- [`rb_to_cles()`](https://easystats.github.io/effectsize/reference/diff_to_cles.md)
  for converting rank-biserial correlation to Probability of
  superiority.

### Changes

- [`effectsize()`](https://easystats.github.io/effectsize/reference/effectsize.md)
  for `BayesFactor` objects returns the same standardized output as for
  `htest`.

### Bug fixes

- [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  for MLM return effect sizes in the correct order of the responses.
- [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  family no longer fails when CIs fail due to non-finite *F*s / degrees
  of freedom.
- [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.html)
  for multivariate models standardizes the (multivariate) response.
- [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.html)
  for models with offsets standardizes offset variables according to
  `include_response` and `two_sd` (
  [\#396](https://github.com/easystats/effectsize/issues/396) ).
- [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md):
  fixed a bug that caused `afex_aov` models with more than 2
  within-subject factors to return incorrect effect sizes for the lower
  level factors (
  [\#389](https://github.com/easystats/effectsize/issues/389) ).

## effectsize 0.5.0

### Breaking Changes

- [`cramers_v()`](https://easystats.github.io/effectsize/reference/phi.md)
  correctly does not work with 1-dimensional tables (for goodness-of-fit
  tests).
- `interpret_d()`, `interpret_g()`, and `interpret_delta()` are now
  [`interpret_cohens_d()`](https://easystats.github.io/effectsize/reference/interpret_cohens_d.md),
  [`interpret_hedges_g()`](https://easystats.github.io/effectsize/reference/interpret_cohens_d.md),
  and
  [`interpret_glass_delta()`](https://easystats.github.io/effectsize/reference/interpret_cohens_d.md).
- `interpret_parameters()` was removed. Use
  [`interpret_r()`](https://easystats.github.io/effectsize/reference/interpret_r.md)
  instead (with caution!).
- Phi, Cohen’s *w*, Cramer’s *V*, ANOVA effect sizes, rank Epsilon
  squared, Kendall’s *W* - CIs default to 95% one-sided CIs
  (`alternative = "greater"`). (To restore previous behavior, set
  `ci = .9, alternative = "two.sided"`.)
- `adjust()`, `change_scale()`, `normalize()`, `ranktransform()`,
  [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.html)
  (data), and `unstandardize()` have moved to the new
  [`{datawizard}`](https://easystats.github.io/datawizard/) package!

### New features

- [`pearsons_c()`](https://easystats.github.io/effectsize/reference/phi.md)
  (and
  [`chisq_to_pearsons_c()`](https://easystats.github.io/effectsize/reference/chisq_to_phi.md))
  for estimating Pearson’s contingency coefficient.
- [`interpret_vif()`](https://easystats.github.io/effectsize/reference/interpret_vif.md)
  for interpretation of *variance inflation factors*.
- [`oddsratio_to_riskratio()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
  can now convert OR coefficients to RR coefficients from a logistic
  GLM(M).
- All effect-size functions gain an `alternative` argument which can be
  used to make one- or two-sided CIs.
- [`interpret()`](https://easystats.github.io/effectsize/reference/interpret.md)
  now accepts as input the results from
  [`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md),
  [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md),
  [`rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.md),
  etc.
- [`interpret_pd()`](https://easystats.github.io/effectsize/reference/interpret_pd.md)
  for the interpretation of the [*Probability of
  Direction*](https://easystats.github.io/bayestestR/reference/p_direction.html).

### Bug fixes

- [`kendalls_w()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.md)
  CIs now correctly bootstrap samples from the raw data (previously the
  rank-transformed data was sampled from).
- [`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md),
  [`sd_pooled()`](https://easystats.github.io/effectsize/reference/sd_pooled.md)
  and
  [`rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.md)
  now properly respect when `y` is a grouping character vector.
- [`effectsize()`](https://easystats.github.io/effectsize/reference/effectsize.md)
  for Chi-squared test of goodness-of-fit now correctly respects
  non-uniform expected probabilities (
  [\#352](https://github.com/easystats/effectsize/issues/352) ).

### Changes

- [`interpret_bf()`](https://easystats.github.io/effectsize/reference/interpret_bf.md)
  now accepts *`log(BF)`* as input.

## effectsize 0.4.5

CRAN release: 2021-05-25

### New features

- [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  family now indicate the type of sum-of-squares used.
- [`rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.md)
  estimates CIs using the normal approximation (previously used
  bootstrapping).
- [`hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.md)
  now used exact bias correction (thanks to
  [@mdelacre](https://github.com/mdelacre) for the suggestion!)
- [`glass_delta()`](https://easystats.github.io/effectsize/reference/cohens_d.md)
  now estimates CIs using the NCP method based on Algina et al (2006).

### Bug fixes

- [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  family returns correctly returns the type 2/3 effect sizes for mixed
  ANOVAs fit with `afex`.
- [`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md)
  family now correctly deals with missing factor levels (
  [\#318](https://github.com/easystats/effectsize/issues/318) )
- [`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md)
  /
  [`hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.md)
  minor fix for CI with unequal variances.

### Changes

- [`mad_pooled()`](https://easystats.github.io/effectsize/reference/sd_pooled.md)
  (the robust version of
  [`sd_pooled()`](https://easystats.github.io/effectsize/reference/sd_pooled.md))
  now correctly pools the the two samples.

## effectsize 0.4.4-1

CRAN release: 2021-04-05

### New features

- [`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html) +
  [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  support `tidymodels` (when that the underlying model is supported;
  [\#311](https://github.com/easystats/effectsize/issues/311) ).
- [`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md)
  family now supports `Pairs()` objects as input.
- [`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html)
  gains the `include_response` argument (default to `TRUE`) (
  [\#309](https://github.com/easystats/effectsize/issues/309) ).

### Bug fixes

- [`kendalls_w()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.md)
  now actually returns correct effect size. Previous estimates were
  incorrect, and based on transposing the groups and blocks.

## effectsize 0.4.4

CRAN release: 2021-03-14

`effectsize` now supports `R >= 3.4`.

### New features

- [`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html)
  now supports bootstrapped estimates (from
  [`parameters::bootstrap_model()`](https://easystats.github.io/parameters/reference/bootstrap_model.html)
  and
  [`parameters::bootstrap_parameters()`](https://easystats.github.io/parameters/reference/bootstrap_parameters.html)).
- `unstandardize()` which will reverse the effects of
  [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.html).
- [`interpret_kendalls_w()`](https://easystats.github.io/effectsize/reference/interpret_kendalls_w.md)
  to interpret Kendall’s coefficient of concordance.
- [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  family of functions can now also return effect sizes for the intercept
  by setting `include_intercept = TRUE` (
  [\#156](https://github.com/easystats/effectsize/issues/156) ).

### Bug fixes

- [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.html)
  can now deal with dates (
  [\#300](https://github.com/easystats/effectsize/issues/300) ).

## effectsize 0.4.3

CRAN release: 2021-01-18

### Breaking Changes

- [`oddsratio()`](https://easystats.github.io/effectsize/reference/oddsratio.md)
  and
  [`riskratio()`](https://easystats.github.io/effectsize/reference/oddsratio.md) -
  order of groups has been changed (the *first* groups is now the
  **treatment group**, and the *second* group is the **control group**),
  so that effect sizes are given as *treatment over control* (treatment
  / control) (previously was reversed). This is done to be consistent
  with other functions in R and in `effectsize`.

### New features

- [`cohens_h()`](https://easystats.github.io/effectsize/reference/oddsratio.md)
  effect size for comparing two independent proportions.

- [`rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.md),
  [`cliffs_delta()`](https://easystats.github.io/effectsize/reference/rank_biserial.md),
  [`rank_epsilon_squared()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.md)
  and
  [`kendalls_w()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.md)
  functions for effect sizes for rank-based tests.

- `adjust()` gains `keep_intercept` argument to keep the intercept.

- [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  family of functions supports `Anova.mlm` objects (from the `car`
  package).

- [`effectsize()`](https://easystats.github.io/effectsize/reference/effectsize.md):

  - supports Cohen’s *g* for McNemar’s test.

  - Extracts OR from Fisher’s Exact Test in the 2x2 case.

- [`eta2_to_f2()`](https://easystats.github.io/effectsize/reference/eta2_to_f2.md)
  /
  [`f2_to_eta2()`](https://easystats.github.io/effectsize/reference/eta2_to_f2.md)
  to convert between two types of effect sizes for ANOVA (
  [\#240](https://github.com/easystats/effectsize/issues/240) ).

- [`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md)
  family of functions gain `mu` argument.

### Bug fixes

- `adjust()` properly works when `multilevel = TRUE`.

- [`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md)
  family /
  [`sd_pooled()`](https://easystats.github.io/effectsize/reference/sd_pooled.md)
  now properly fails when given a missing column name.

### Changes

- [`effectsize()`](https://easystats.github.io/effectsize/reference/effectsize.md)
  for `htest` objects now tries first to extract the data used for
  testing, and computed the effect size directly on that data.

- [`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md)
  family /
  [`sd_pooled()`](https://easystats.github.io/effectsize/reference/sd_pooled.md)
  now respect any transformations (e.g. `I(log(x) - 3) ~ factor(y)`) in
  a passed formula.

- [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  family of functions gains a `verbose` argument.

- `verbose` argument more strictly respected.

- [`glass_delta()`](https://easystats.github.io/effectsize/reference/cohens_d.md)
  returns CIs based on the bootstrap.

## effectsize 0.4.1

CRAN release: 2020-12-07

### Breaking Changes

- [`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md)
  and
  [`glass_delta()`](https://easystats.github.io/effectsize/reference/cohens_d.md):
  The `correction` argument has been deprecated, in favor of it being
  correctly implemented in
  [`hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.md)
  ( [\#222](https://github.com/easystats/effectsize/issues/222) ).

- [`eta_squared_posterior()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  no longer uses
  [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html) by default.

### New features

- [`effectsize()`](https://easystats.github.io/effectsize/reference/effectsize.md)
  gains `type =` argument for specifying which effect size to return.

- [`eta_squared_posterior()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  can return a generalized Eta squared.

- [`oddsratio()`](https://easystats.github.io/effectsize/reference/oddsratio.md)
  and
  [`riskratio()`](https://easystats.github.io/effectsize/reference/oddsratio.md)
  functions for 2-by-2 contingency tables.

- [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.html)
  gains support for `mediation::mediate()` models.

- [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  family available for `manova` objects.

### Changes

- [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  family of functions returns non-partial effect size for one-way
  between subjects design
  ([\#180](https://github.com/easystats/effectsize/issues/180)).

### Bug fixes

- [`hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.md)
  correctly implements the available bias correction methods (
  [\#222](https://github.com/easystats/effectsize/issues/222) ).

- Fixed width of CI for Cohen’s *d* and Hedges’ *g* when using
  *non*-pooled SD.

## effectsize 0.4.0

CRAN release: 2020-10-25

### Breaking Changes

- [`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html)
  for multi-component models (such as zero-inflated) now returns the
  unstandardized parameters in some cases where standardization is not
  possible (previously returned `NA`s).

- Column name changes:

  - [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
    / `F_to_eta2` families of function now has the `Eta2` format, where
    previously was `Eta_Sq`.

  - `cramers_v` is now `Cramers_v`

### New features

- [`effectsize()`](https://easystats.github.io/effectsize/reference/effectsize.md)
  added support for `BayesFactor` objects (Cohen’s *d*, Cramer’s *v*,
  and *r*).

- [`cohens_g()`](https://easystats.github.io/effectsize/reference/cohens_g.md)
  effect size for paired contingency tables.

- Generalized Eta Squared now available via
  `eta_squared(generalized = ...)`.

- [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md),
  [`omega_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  and
  [`epsilon_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  fully support `aovlist`, `afex_aov` and `mlm` (or `maov`) objects.

- [`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html)
  can now return Odds ratios / IRRs (or any exponentiated parameter) by
  setting `exponentiate = TRUE`.

- Added
  [`cohens_f_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  and
  [`F_to_f2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)
  for Cohen’s *f*-squared.

- [`cohens_f()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  /
  [`cohens_f_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)can
  be used to estimate Cohen’s *f* for the R-squared change between two
  models.

- [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.html)
  and
  [`standardize_info()`](https://easystats.github.io/parameters/reference/standardize_info.html)
  work with weighted models / data (
  [\#82](https://github.com/easystats/effectsize/issues/82) ).

- Added `hardlyworking` (simulated) dataset, for use in examples.

- `interpret_*` (
  [\#131](https://github.com/easystats/effectsize/issues/131) ):

  - [`interpret_omega_squared()`](https://easystats.github.io/effectsize/reference/interpret_omega_squared.md)
    added `"cohen1992"` rule.

  - [`interpret_p()`](https://easystats.github.io/effectsize/reference/interpret_p.md)
    added *Redefine statistical significance* rules.

- [`oddsratio_to_riskratio()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
  for converting OR to RR.

### Changes

- CIs for Omega-/Epsilon-squared and Adjusted Phi/Cramer’s V return 0s
  instead of negative values.

- [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.html)
  for data frames gains the `remove_na` argument for dealing with `NA`s
  ( [\#147](https://github.com/easystats/effectsize/issues/147) ).

- [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.html)
  and
  [`standardize_info()`](https://easystats.github.io/parameters/reference/standardize_info.html)
  now (and by extension,
  [`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html))
  respect the weights in weighted models when standardizing (
  [\#82](https://github.com/easystats/effectsize/issues/82) ).

- Internal changes to
  [`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html)
  (reducing co-dependency with `parameters`) - argument `parameters` has
  been dropped.

### Bug fixes

- `ranktransform(sign = TURE)` correctly (doesn’t) deal with zeros.

- [`effectsize()`](https://easystats.github.io/effectsize/reference/effectsize.md)
  for `htest` works with Spearman and Kendall correlations (
  [\#165](https://github.com/easystats/effectsize/issues/165) ).

- [`cramers_v()`](https://easystats.github.io/effectsize/reference/phi.md)
  and [`phi()`](https://easystats.github.io/effectsize/reference/phi.md)
  now work with goodness-of-fit data (
  [\#158](https://github.com/easystats/effectsize/issues/158) )

- [`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html)
  for post-hoc correctly standardizes transformed outcome.

- Setting `two_sd = TRUE` in
  [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.html)
  and
  [`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html)
  (correctly) on uses 2-SDs of the predictors (and not the response).

- [`standardize_info()`](https://easystats.github.io/parameters/reference/standardize_info.html)
  / `standardize_parameters(method = "posthoc")` work for zero-inflated
  models ( [\#135](https://github.com/easystats/effectsize/issues/135) )

- `standardize_info(include_pseudo = TRUE)` /
  `standardize_parameters(method = "pseudo")` are less sensitive in
  detecting between-group variation of within-group variables.

- [`interpret_oddsratio()`](https://easystats.github.io/effectsize/reference/interpret_oddsratio.md)
  correctly treats extremely small odds the same as treats extremely
  large ones.

## effectsize 0.3.3

CRAN release: 2020-09-17

### New features

- `standardize_parameters(method = "pseudo")` returns
  pseudo-standardized coefficients for (G)LMM models.

- `d_to_common_language()` for common language measures of standardized
  differences (a-la Cohen’s d).

### Changes

- `r_to_odds()` family is now deprecated in favor of
  [`r_to_oddsratio()`](https://easystats.github.io/effectsize/reference/d_to_r.md).

- `interpret_odds()` is now deprecated in favor of
  [`interpret_oddsratio()`](https://easystats.github.io/effectsize/reference/interpret_oddsratio.md)

### Bug fixes

- [`phi()`](https://easystats.github.io/effectsize/reference/phi.md) and
  [`cramers_v()`](https://easystats.github.io/effectsize/reference/phi.md)
  did not respect the CI argument (
  [\#111](https://github.com/easystats/effectsize/issues/111) ).

- [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.html)
  /
  [`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html)
  properly deal with transformed data in the model formula (
  [\#113](https://github.com/easystats/effectsize/issues/113) ).

- [`odds_to_probs()`](https://easystats.github.io/effectsize/reference/odds_to_probs.md)
  was mis-treating impossible odds (NEVER TELL ME THE ODDS!
  [\#123](https://github.com/easystats/effectsize/issues/123) )

## effectsize 0.3.2

CRAN release: 2020-07-27

### New features

- [`eta_squared_posterior()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  for estimating Eta Squared for Bayesian models.

- [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md),
  [`omega_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  and
  [`epsilon_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  now works with

  - `ols` / `rms` models.

- [`effectsize()`](https://easystats.github.io/effectsize/reference/effectsize.md)
  for class `htest` supports `oneway.test(...)`.

### Bug fixes

- Fix minor miss-calculation of Chi-squared for 2\*2 table with small
  samples ( [\#102](https://github.com/easystats/effectsize/issues/102)
  ).

- Fixed miss-calculation of signed rank in `ranktransform()` (
  [\#87](https://github.com/easystats/effectsize/issues/87) ).

- Fixed bug in
  [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.html)
  for standard objects with non-standard class-attributes (like vectors
  of class `haven_labelled` or `vctrs_vctr`).

- Fix
  [`effectsize()`](https://easystats.github.io/effectsize/reference/effectsize.md)
  for one sample `t.test(...)` (
  [\#95](https://github.com/easystats/effectsize/issues/95) ; thanks to
  pull request by [@mutlusun](https://github.com/mutlusun) )

## effectsize 0.3.1

CRAN release: 2020-05-19

### New features

- [`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html)
  now returns CIs (
  [\#72](https://github.com/easystats/effectsize/issues/72) )

- [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md),
  [`omega_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  and
  [`epsilon_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  now works with

  - `gam` models.

  - `afex` models.

  - `lme` and `anova.lme` objects.

- New function
  [`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.html)
  for effect sizes.

- New plotting methods in the `see` package.

## effectsize 0.3.0

CRAN release: 2020-04-11

### New features

- New general purpose
  [`effectsize()`](https://easystats.github.io/effectsize/reference/effectsize.md)
  function.

- Effectsize for differences have CI methods, and return a data frame.

- Effectsize for ANOVA all have CI methods, and none are based on
  bootstrapping.

- New effect sizes for contingency tables
  ([`phi()`](https://easystats.github.io/effectsize/reference/phi.md)
  and
  [`cramers_v()`](https://easystats.github.io/effectsize/reference/phi.md)).

- [`chisq_to_phi()`](https://easystats.github.io/effectsize/reference/chisq_to_phi.md)
  /
  [`cramers_v()`](https://easystats.github.io/effectsize/reference/phi.md)
  functions now support CIs (via the ncp method), and return a data
  frame.

- [`F_to_eta2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)
  family of functions now support CIs (via the ncp method), and return a
  data frame.

- [`t_to_d()`](https://easystats.github.io/effectsize/reference/t_to_r.md)
  and
  [`t_to_r()`](https://easystats.github.io/effectsize/reference/t_to_r.md)
  now support CIs (via the ncp method), and return a data frame.

- [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.html)
  for model-objects has a default-method, which usually accepts all
  models. Exception for model-objects that do not work will be added if
  missing.

- `standardize.data.frame()` gets `append` and `suffix` arguments, to
  add (instead of replace) standardized variables to the returned data
  frame.

- [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md),
  [`omega_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  and
  [`epsilon_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
  now works

  - output from
    [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html).

  - `mlm` models.

### Bug fixes

- Fix
  [`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md)’s
  dealing with formula input
  ([\#44](https://github.com/easystats/effectsize/issues/44)).

- [`sd_pooled()`](https://easystats.github.io/effectsize/reference/sd_pooled.md)
  now returns the… pooled sd
  ([\#44](https://github.com/easystats/effectsize/issues/44)).

### Changes

- In
  [`t_to_d()`](https://easystats.github.io/effectsize/reference/t_to_r.md),
  argument `pooled` is now `paired`.

## effectsize 0.2.0

CRAN release: 2020-02-25

### Bug fixes

- `standardize.data.frame()` did not work when variables had missing
  values.

- Fixed wrong computation in
  [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.html)
  when `two_sd = TRUE`.

- Fixed bug with missing column names in
  [`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html)
  for models with different components (like count and zero-inflation).

## effectsize 0.1.1

CRAN release: 2020-01-27

### Changes

- News are hidden in an air of mystery…

## effectsize 0.1.0

### New features

- [`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html)
  and
  [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.html)
  now support models from packages *brglm*, *brglm2*, *mixor*, *fixest*,
  *cgam*, *cplm*, *cglm*, *glmmadmb* and *complmrob*.

### Bug fixes

- Fix CRAN check issues.
