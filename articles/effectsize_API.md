# Support Functions for Model Extensions

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`effectsize`](https://easystats.github.io/effectsize/)`)`

### Supporting ANOVA Effect Sizes

To add support for you model, create a new `.anova_es()` method
function. This functions should generally do 3 things:

1.  Build a data frame with all the required information.
2.  Pass the data frame to one of the 3 functions.
3.  Set some attributes to the output.

#### Simple ANOVA tables

The input data frame must have these columns: - `Parameter` (char) - The
name of the parameter or, more often, the term. - `Sum_Squares` (num) -
The sum of squares. - `df` (num) - The degrees of freedom associated
with the `Sum_Squares`. - `Mean_Square_residuals` (num; *optional*) - if
*not* present, is calculated as `Sum_Squares / df`. (Any other column is
ignored.)

And exactly *1* row Where `Parameter` is `Residual`.

Optionally, one of the rows can have a `(Intercept)` value for
`Parameter`.

An example of a minimally valid data frame:

\
`min_aov`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  Parameter ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"(Intercept)"``, ``"A"``, ``"B"``, ``"Residuals"``)``,`\
`  Sum_Squares ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``30``, ``40``, ``10``, ``100``)``,`\
`  df ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``1``, ``2``, ``50``)`\
`)`

Pass the data frame to
[`.es_aov_simple()`](https://easystats.github.io/effectsize/reference/effectsize_API.md):

\
[`.es_aov_simple`](https://easystats.github.io/effectsize/reference/effectsize_API.md)`(`\
`  ``min_aov``,`\
`  type ``=`` ``"eta"``, partial ``=`` ``TRUE``, generalized ``=`` ``FALSE``,`\
`  include_intercept ``=`` ``FALSE``,`\
`  ci ``=`` ``0.95``, alternative ``=`` ``"greater"``,`\
`  verbose ``=`` ``TRUE`\
`)`

    >   Parameter Eta2_partial   CI CI_low CI_high
    > 1         A        0.286 0.95   0.12       1
    > 2         B        0.091 0.95   0.00       1

The output is a data frame with the columns: `Parameter`, the effect
size, and (optionally) `CI` + `CI_low` + `CI_high`,

And with the following attributes: `generalized`, `ci`, `alternative`,
`anova_type` (`NA` or `NULL`), `approximate`.

You can then set the `anova_type` attribute to {1, 2, 3, or `NA`} and
return the output.

#### ANOVA Tables with Multiple Error Strata

(e.g., `aovlist` models.)

The input data frame must have these columns:

- `Group` (char) - The strata
- `Parameter` (char)
- `Sum_Squares` (num)
- `df` (num)
- `Mean_Square_residuals` (num; *optional*)

And exactly *1* row ***per `Group`*** Where `Parameter` is `Residual`.

Optionally, one of the rows can have a `(Intercept)` value for
`Parameter`.

An example of a minimally valid data frame:

\
`min_aovlist`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  Group ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"S"``, ``"S"``, ``"S:A"``, ``"S:A"``)``,`\
`  Parameter ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"(Intercept)"``, ``"Residuals"``, ``"A"``, ``"Residuals"``)``,`\
`  Sum_Squares ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``34``, ``21``, ``34``, ``400``)``,`\
`  df ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``12``, ``4``, ``30``)`\
`)`

Pass the data frame to
[`.es_aov_strata()`](https://easystats.github.io/effectsize/reference/effectsize_API.md),
along with a list of predictors (including the stratifying variables) to
the `DV_names` argument:

\
[`.es_aov_strata`](https://easystats.github.io/effectsize/reference/effectsize_API.md)`(`\
`  ``min_aovlist``,`\
`  DV_names ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"S"``, ``"A"``)``,`\
`  type ``=`` ``"omega"``, partial ``=`` ``TRUE``, generalized ``=`` ``FALSE``,`\
`  ci ``=`` ``0.95``, alternative ``=`` ``"greater"``,`\
`  verbose ``=`` ``TRUE``,`\
`  include_intercept ``=`` ``TRUE`\
`)`

    >   Group   Parameter Omega2_partial   CI CI_low CI_high
    > 1     S (Intercept)           0.57 0.95   0.21       1
    > 2   S:A           A           0.00 0.95   0.00       1

The output is a data frame with the columns: `Group`, `Parameter`, the
effect size, and (optionally) `CI` + `CI_low` + `CI_high`,

And with the following attributes: `generalized`, `ci`, `alternative`,
`approximate`.

You can then set the `anova_type` attribute to {1, 2, 3, or `NA`} and
return the output.

#### Approximate Effect sizes

When *sums of squares* cannot be extracted, we can still get
*approximate* effect sizes based on the
[`F_to_eta2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)
family of functions.

The input data frame must have these columns:

- `Parameter` (char)
- `F` (num) - The *F* test statistic.
- `df` (num) - effect degrees of freedom.
- (Can also have a `t` col instead, in which case `df` is set to 1, and
  `F` is `t^2`).
- `df_error` (num) - error degrees of freedom.

Optionally, one of the rows can have `(Intercept)` as the `Parameter`.

An example of a minimally valid data frame:

\
`min_anova`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  Parameter ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"(Intercept)"``, ``"A"``, ``"B"``)``,`\
`  F ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``4``, ``7``, ``0.7``)``,`\
`  df ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``1``, ``2``)``,`\
`  df_error ``=`` ``34`\
`)`

Pass the table to
[`.es_aov_table()`](https://easystats.github.io/effectsize/reference/effectsize_API.md):

\
[`.es_aov_table`](https://easystats.github.io/effectsize/reference/effectsize_API.md)`(`\
`  ``min_anova``,`\
`  type ``=`` ``"eta"``, partial ``=`` ``TRUE``, generalized ``=`` ``FALSE``,`\
`  include_intercept ``=`` ``FALSE``,`\
`  ci ``=`` ``0.95``, alternative ``=`` ``"greater"``,`\
`  verbose ``=`` ``TRUE`\
`)`

    >   Parameter Eta2_partial   CI CI_low CI_high
    > 1         A         0.17 0.95  0.023       1
    > 2         B         0.04 0.95  0.000       1

The output is a data frame with the columns: `Parameter`, the effect
size, and (optionally) `CI` + `CI_low` + `CI_high`,

And with the following attributes: `generalized`, `ci`, `alternative`,
`approximate`.

You can then set the `anova_type` attribute to {1, 2, 3, or `NA`} and
return the output, and optionally the `approximate` attribute, and
return the output.

#### *Example*

Let’s fit a simple linear model and change its class:

\
`mod`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``mpg`` ``~`` `[`factor`](https://rdrr.io/r/base/factor.html)`(``cyl``)`` ``+`` ``am``, ``mtcars``)`\
\
[`class`](https://rdrr.io/r/base/class.html)`(``mod``)`` ``<-`` ``"superMODEL"`

We now need a new `.anova_es.superMODEL` function:

\
`.anova_es.superMODEL`` ``<-`` ``function``(``model``, ``...``)`` ``{`\
`  ``# Get ANOVA table`\
`  ``anov`` ``<-`` `[`suppressWarnings`](https://rdrr.io/r/base/warning.html)`(``stats``:::`[`anova.lm`](https://rdrr.io/r/stats/anova.lm.html)`(``model``)``)`\
`  ``anov`` ``<-`` `[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html)`(``anov``)`\
\
`  ``# Clean up`\
`  ``anov``[[``"Parameter"``]``]`` ``<-`` `[`rownames`](https://rdrr.io/r/base/colnames.html)`(``anov``)`\
`  `[`colnames`](https://rdrr.io/r/base/colnames.html)`(``anov``)``[``2``:``1``]`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Sum_Squares"``, ``"df"``)`\
\
`  ``# Pass`\
`  ``out`` ``<-`` `[`.es_aov_simple`](https://easystats.github.io/effectsize/reference/effectsize_API.md)`(``anov``, ``...``)`\
\
`  ``# Set attribute`\
`  `[`attr`](https://rdrr.io/r/base/attr.html)`(``out``, ``"anova_type"``)`` ``<-`` ``1`\
\
`  ``out`\
`}`

And… that’s it! Our new `superMODEL` class of models is fully supported!

\
[`eta_squared`](https://easystats.github.io/effectsize/reference/eta_squared.md)`(``mod``)`

    > # Effect Size for ANOVA (Type I)
    > 
    > Parameter   | Eta2 (partial) |       95% CI
    > -------------------------------------------
    > factor(cyl) |           0.76 | [0.61, 1.00]
    > am          |           0.12 | [0.00, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

\
[`eta_squared`](https://easystats.github.io/effectsize/reference/eta_squared.md)`(``mod``, partial ``=`` ``FALSE``)`

    > # Effect Size for ANOVA (Type I)
    > 
    > Parameter   | Eta2 |       95% CI
    > ---------------------------------
    > factor(cyl) | 0.73 | [0.57, 1.00]
    > am          | 0.03 | [0.00, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

\
[`omega_squared`](https://easystats.github.io/effectsize/reference/eta_squared.md)`(``mod``)`

    > # Effect Size for ANOVA (Type I)
    > 
    > Parameter   | Omega2 (partial) |       95% CI
    > ---------------------------------------------
    > factor(cyl) |             0.73 | [0.56, 1.00]
    > am          |             0.08 | [0.00, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

\
`# Etc...`

## References
