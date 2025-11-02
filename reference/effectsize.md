# Effect Sizes

This function tries to return the best effect-size measure for the
provided input model. See details.

## Usage

``` r
# S3 method for class 'BFBayesFactor'
effectsize(model, type = NULL, ci = 0.95, test = NULL, verbose = TRUE, ...)

effectsize(model, ...)

# S3 method for class 'aov'
effectsize(model, type = NULL, ...)

# S3 method for class 'datawizard_crosstabs'
effectsize(model, type = NULL, drop = TRUE, verbose = TRUE, ...)

# S3 method for class 'datawizard_crosstab'
effectsize(model, type = NULL, drop = TRUE, verbose = TRUE, ...)

# S3 method for class 'datawizard_tables'
effectsize(model, type = NULL, drop = TRUE, verbose = TRUE, ...)

# S3 method for class 'datawizard_table'
effectsize(model, type = NULL, drop = TRUE, verbose = TRUE, ...)

# S3 method for class 'htest'
effectsize(model, type = NULL, verbose = TRUE, ...)
```

## Arguments

- model:

  An object of class `htest`, or a statistical model. See details.

- type:

  The effect size of interest. See details.

- ci:

  Value or vector of probability of the CI (between 0 and 1) to be
  estimated. Default to `0.95` (`95%`).

- test:

  The indices of effect existence to compute. Character (vector) or list
  with one or more of these options: `"p_direction"` (or `"pd"`),
  `"rope"`, `"p_map"`, `"p_significance"` (or `"ps"`), `"p_rope"`,
  `"equivalence_test"` (or `"equitest"`), `"bayesfactor"` (or `"bf"`) or
  `"all"` to compute all tests. For each "test", the corresponding
  **bayestestR** function is called (e.g.
  [`rope()`](https://easystats.github.io/bayestestR/reference/rope.html)
  or
  [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.html))
  and its results included in the summary output.

- verbose:

  Toggle off warnings.

- ...:

  Arguments passed to or from other methods. See details.

- drop:

  Should empty rows/cols be dropped?

## Value

A data frame with the effect size (depending on input) and and its CIs
(`CI_low` and `CI_high`).

## Details

- For an object of class `htest`, data is extracted via
  [`insight::get_data()`](https://easystats.github.io/insight/reference/get_data.html),
  and passed to the relevant function according to:

  - A **t-test** depending on `type`: `"cohens_d"` (default),
    `"hedges_g"`, or one of `"p_superiority"`, `"u1"`, `"u2"`, `"u3"`,
    `"overlap"`.

    - For a **Paired t-test**: depending on `type`: `"rm_rm"`,
      `"rm_av"`, `"rm_b"`, `"rm_d"`, `"rm_z"`.

  - A **Chi-squared tests of independence** or **Fisher's Exact Test**,
    depending on `type`: `"cramers_v"` (default), `"tschuprows_t"`,
    `"phi"`, `"cohens_w"`, `"pearsons_c"`, `"cohens_h"`, `"oddsratio"`,
    `"riskratio"`, `"arr"`, or `"nnt"`.

  - A **Chi-squared tests of goodness-of-fit**, depending on `type`:
    `"fei"` (default) `"cohens_w"`, `"pearsons_c"`

  - A **One-way ANOVA test**, depending on `type`: `"eta"` (default),
    `"omega"` or `"epsilon"` -squared, `"f"`, or `"f2"`.

  - A **McNemar test** returns *Cohen's g*.

  - A **Wilcoxon test** depending on `type`: returns "`rank_biserial`"
    correlation (default) or one of `"p_superiority"`, `"vda"`, `"u2"`,
    `"u3"`, `"overlap"`.

  - A **Kruskal-Wallis test** depending on `type`: `"epsilon"` (default)
    or `"eta"`.

  - A **Friedman test** returns *Kendall's W*. (Where applicable, `ci`
    and `alternative` are taken from the `htest` if not otherwise
    provided.)

- For an object of class `BFBayesFactor`, using
  [`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html),

  - A **t-test** depending on `type`: `"cohens_d"` (default) or one of
    `"p_superiority"`, `"u1"`, `"u2"`, `"u3"`, `"overlap"`.

  - A **correlation test** returns *r*.

  - A **contingency table test**, depending on `type`: `"cramers_v"`
    (default), `"phi"`, `"tschuprows_t"`, `"cohens_w"`, `"pearsons_c"`,
    `"cohens_h"`, `"oddsratio"`, or `"riskratio"`, `"arr"`, or `"nnt"`.

  - A **proportion test** returns *p*.

- Objects of class `anova`, `aov`, `aovlist` or `afex_aov`, depending on
  `type`: `"eta"` (default), `"omega"` or `"epsilon"` -squared, `"f"`,
  or `"f2"`.

- Objects of class `datawizard_crosstab(s)` / `datawizard_table(s)`
  built with
  [`datawizard::data_tabulate()`](https://easystats.github.io/datawizard/reference/data_tabulate.html) -
  same as Chi-squared tests of independence / goodness-of-fit,
  respectively.

- Other objects are passed to
  [`parameters::standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html).

**For statistical models it is recommended to directly use the listed
functions, for the full range of options they provide.**

## Plotting with `see`

The `see` package contains relevant plotting functions. See the
[plotting vignette in the `see`
package](https://easystats.github.io/see/articles/effectsize.html).

## See also

`vignette(package = "effectsize")`

## Examples

``` r

## Hypothesis Testing
## ------------------
data("Music_preferences")
Xsq <- chisq.test(Music_preferences)
effectsize(Xsq)
#> Cramer's V (adj.) |       95% CI
#> --------------------------------
#> 0.23              | [0.18, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].
effectsize(Xsq, type = "cohens_w")
#> Cohen's w |       95% CI
#> ------------------------
#> 0.34      | [0.27, 1.41]
#> 
#> - One-sided CIs: upper bound fixed at [1.41~].

# Or:
data("mtcars")
xtab <- datawizard::data_tabulate(mtcars, select = "cyl", by = "am")
effectsize(xtab)
#> Cramer's V (adj.) |       95% CI
#> --------------------------------
#> 0.46              | [0.00, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].

Tt <- t.test(1:10, y = c(7:20), alternative = "less")
effectsize(Tt)
#> Cohen's d |        95% CI
#> -------------------------
#> -2.19     | [-Inf, -1.32]
#> 
#> - Estimated using un-pooled SD.
#> - One-sided CIs: lower bound fixed at [-Inf].

Tt <- t.test(
  x = c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30),
  y = c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29),
  paired = TRUE
)
effectsize(Tt, type = "rm_b")
#> Becker's d |       95% CI
#> -------------------------
#> 0.49       | [0.09, 0.88]
#> 
#> - Adjusted for small sample bias.

Aov <- oneway.test(extra ~ group, data = sleep, var.equal = TRUE)
effectsize(Aov)
#> Eta2 |       95% CI
#> -------------------
#> 0.16 | [0.00, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].
effectsize(Aov, type = "omega")
#> Omega2 |       95% CI
#> ---------------------
#> 0.11   | [0.00, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].

Wt <- wilcox.test(1:10, 7:20, mu = -3, alternative = "less", exact = FALSE)
effectsize(Wt)
#> r (rank biserial) |         95% CI
#> ----------------------------------
#> -0.65             | [-1.00, -0.36]
#> 
#> - Deviation from a difference of -3.
#> - One-sided CIs: lower bound fixed at [-1.00].
effectsize(Wt, type = "u2")
#> Cohen's U2 |       95% CI
#> -------------------------
#> 0.73       | [0.00, 0.90]
#> 
#> - Non-parametric CLES

## Models and Anova Tables
## -----------------------
fit <- lm(mpg ~ factor(cyl) * wt + hp, data = mtcars)
effectsize(fit, method = "basic")
#> # Standardization method: basic
#> 
#> Parameter    | Std. Coef. |         95% CI
#> ------------------------------------------
#> (Intercept)  |       0.00 | [ 0.00,  0.00]
#> cyl [6]      |      -0.59 | [-1.89,  0.70]
#> cyl [8]      |      -1.06 | [-1.90, -0.23]
#> wt           |      -0.90 | [-1.33, -0.46]
#> hp           |      -0.25 | [-0.52,  0.01]
#> cyl [6] × wt |       0.50 | [-0.85,  1.84]
#> cyl [8] × wt |       1.15 | [ 0.06,  2.25]

anova_table <- anova(fit)
effectsize(anova_table)
#> # Effect Size for ANOVA (Type I)
#> 
#> Parameter      | Eta2 (partial) |       95% CI
#> ----------------------------------------------
#> factor(cyl)    |           0.86 | [0.76, 1.00]
#> wt             |           0.47 | [0.22, 1.00]
#> hp             |           0.14 | [0.00, 1.00]
#> factor(cyl):wt |           0.16 | [0.00, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].
effectsize(anova_table, type = "epsilon")
#> # Effect Size for ANOVA (Type I)
#> 
#> Parameter      | Epsilon2 (partial) |       95% CI
#> --------------------------------------------------
#> factor(cyl)    |               0.85 | [0.74, 1.00]
#> wt             |               0.44 | [0.20, 1.00]
#> hp             |               0.11 | [0.00, 1.00]
#> factor(cyl):wt |               0.09 | [0.00, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].

if (FALSE) { # requireNamespace("BayesFactor", quietly = TRUE) && interactive()
## Bayesian Hypothesis Testing
## ---------------------------
bf_prop <- BayesFactor::proportionBF(3, 7, p = 0.3)
effectsize(bf_prop)

bf_corr <- BayesFactor::correlationBF(attitude$rating, attitude$complaints)
effectsize(bf_corr)

data(RCT_table)
bf_xtab <- BayesFactor::contingencyTableBF(RCT_table, sampleType = "poisson", fixedMargin = "cols")
effectsize(bf_xtab)
effectsize(bf_xtab, type = "oddsratio")
effectsize(bf_xtab, type = "arr")

bf_ttest <- BayesFactor::ttestBF(sleep$extra[sleep$group == 1],
  sleep$extra[sleep$group == 2],
  paired = TRUE, mu = -1
)
effectsize(bf_ttest)
}
```
