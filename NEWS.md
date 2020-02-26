# effectsize 0.2.1

## Changes

- `standardize.data.frame()` gets `append` and `suffix` arguments, to add (instead of replace) standardized variables to the returned data frame.

## Bug fixes

- `standardize.data.frame()` did not work when variables had missing values.
- Fixed wrong computation in `standardize()` when `two_sd = TRUE`.

# effectsize 0.2.0

## Changes

- News are hidden in an air of mystery...

# effectsize 0.1.0

## New features

- `standardize_parameters()` and `standardize()` now support models from packages *brglm*, *brglm2*, *mixor*, *fixest*, *cgam*, *cplm*, *cglm*, *glmmadmb* and *complmrob*.

## Bug fixes

- Fix CRAN check issues.