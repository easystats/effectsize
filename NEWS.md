# effectsize 0.3.1

## New features

- `standardize_parameters()` can now return CIs ( #72 )
- `eta_squared()`, `omega_squared()` and `epsilon_squared()` now works with
  - `gam` models.
  - `afex` models.
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