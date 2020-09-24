# effectsize 0.3.x

## New features

- Added `hardlyworking` (simulated) dataset, for use in examples.
- `interpret_*` ( #131 ):
  - `interpret_omega_squared()` added `"cohen1992"` rule.
  - `interpret_p()` added *Redefine statistical significance* rules.
- `oddsratio_to_riskratio()` for converting OR to RR.

## Changes

- Internal changes to `standardize_parameters()` (reducing co-dependency with `parameters`) - argument `parameters` has been dropped.

## Bug fixes

- `standardize_info(include_pseudo = TRUE)` / `standardize_parameters(method = "pseudo")` are less sensitive in detecting between-group variation of within-group variables.
- `interpret_oddsratio()` correctly treats extremely small odds the same as treats extremely large ones.


# effectsize 0.3.3

## New features

- `standardize_parameters(method = "pseudo")` returns pseudo-standardized coefficients for (G)LMM models.
- `d_to_common_language()` for common language measures of standardized differences (a-la Cohen's d).

## Changes

- `r_to_odds()` family is now deprecated in favor of `r_to_oddsratio()`.
- `interpret_odds()` is now deprecated in favor of `interpret_oddsratio()`

## Bug fixes

- `phi()` and `cramers_v()` did not respect the CI argument ( #111 ).
- `standardize()` / `standardize_parameters()` properly deal with transformed data in the model formula ( #113 ).
- `odds_to_probs()` was mis-treating impossible odds (NEVER TELL ME THE ODDS! #123 )

# effectsize 0.3.2

## New features

- `eta_squared_posterior()` for estimating Eta Squared for Bayesian models.
- `eta_squared()`, `omega_squared()` and `epsilon_squared()` now works with
  - `ols` / `rms` models.
- `effectsize()` for class `htest` supports `oneway.test(...)`.


## Bug fixes

- Fix minor miss-calculation of Chi-squared for 2*2 table with small samples ( #102 ).
- Fixed miss-calculation of signed rank in `ranktransform()` ( #87 ).
- Fixed bug in `standardize()` for standard objects with non-standard class-attributes (like vectors of class `haven_labelled` or `vctrs_vctr`).  
- Fix `effectsize()` for one sample `t.test(...)` ( #95 ; thanks to pull request by @mutlusun )


# effectsize 0.3.1

## New features

- `standardize_parameters()` now returns CIs ( #72 )
- `eta_squared()`, `omega_squared()` and `epsilon_squared()` now works with
  - `gam` models.
  - `afex` models.
  - `lme` and `anova.lme` objects.
- New function `equivalence_test()` for effect sizes.
- New plotting methods in the `see` package.

# effectsize 0.3.0

## New features

- New general purpose `effectsize()` function.
- Effectsize for differences have CI methods, and return a data frame.
- Effectsize for ANOVA all have CI methods, and none are based on bootstrapping.
- New effect sizes for contingency tables (`phi()` and `cramers_v()`).
- `chisq_to_phi()` / `cramers_v()` functions now support CIs (via the ncp method), and return a data frame.
- `F_to_eta2()` family of functions now support CIs (via the ncp method), and return a data frame.
- `t_to_d()` and `t_to_r()` now support CIs (via the ncp method), and return a data frame.
- `standardize()` for model-objects has a default-method, which usually accepts all models. Exception for model-objects that do not work will be added if missing.
- `standardize.data.frame()` gets `append` and `suffix` arguments, to add (instead of replace) standardized variables to the returned data frame.
- `eta_squared()`, `omega_squared()` and `epsilon_squared()` now works
  - output from `parameters::model_parameters()`.
  - `mlm` models.

## Bug fixes

- Fix `cohens_d()`'s dealing with formula input (#44).
- `sd_pooled()` now returns the... pooled sd (#44).

## Changes

- In `t_to_d()`, argument `pooled` is now `paired`.

# effectsize 0.2.0

## Bug fixes

- `standardize.data.frame()` did not work when variables had missing values.
- Fixed wrong computation in `standardize()` when `two_sd = TRUE`.
- Fixed bug with missing column names in `standardize_parameters()` for models with different components (like count and zero-inflation).

# effectsize 0.1.1

## Changes

- News are hidden in an air of mystery...

# effectsize 0.1.0

## New features

- `standardize_parameters()` and `standardize()` now support models from packages *brglm*, *brglm2*, *mixor*, *fixest*, *cgam*, *cplm*, *cglm*, *glmmadmb* and *complmrob*.

## Bug fixes

- Fix CRAN check issues.
