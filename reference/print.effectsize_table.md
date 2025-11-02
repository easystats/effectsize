# Methods for `{effectsize}` Tables

Printing, formatting and plotting methods for `effectsize` tables.

## Usage

``` r
# S3 method for class 'effectsize_table'
plot(x, ...)

# S3 method for class 'effectsize_table'
print(x, digits = 2, use_symbols = getOption("es.use_symbols", FALSE), ...)

# S3 method for class 'effectsize_table'
print_md(x, digits = 2, use_symbols = getOption("es.use_symbols", FALSE), ...)

# S3 method for class 'effectsize_table'
print_html(
  x,
  digits = 2,
  use_symbols = getOption("es.use_symbols", FALSE),
  ...
)

# S3 method for class 'effectsize_table'
format(
  x,
  digits = 2,
  output = c("text", "markdown", "html"),
  use_symbols = getOption("es.use_symbols", FALSE),
  ...
)

# S3 method for class 'effectsize_difference'
print(x, digits = 2, append_CLES = NULL, ...)
```

## Arguments

- x:

  Object to print.

- ...:

  Arguments passed to or from other functions.

- digits:

  Number of digits for rounding or significant figures. May also be
  `"signif"` to return significant figures or `"scientific"` to return
  scientific notation. Control the number of digits by adding the value
  as suffix, e.g. `digits = "scientific4"` to have scientific notation
  with 4 decimal places, or `digits = "signif5"` for 5 significant
  figures (see also [`signif()`](https://rdrr.io/r/base/Round.html)).

- use_symbols:

  Should proper symbols be printed (`TRUE`) instead of transliterated
  effect size names (`FALSE`). See
  [effectsize_options](https://easystats.github.io/effectsize/reference/effectsize_options.md).

- output:

  Which output is the formatting intended for? Affects how title and
  footers are formatted.

- append_CLES:

  Which Common Language Effect Sizes should be printed as well? Only
  applicable to Cohen's *d*, Hedges' *g* for independent samples of
  equal variance (pooled sd) or for the rank-biserial correlation for
  independent samples (See
  [d_to_cles](https://easystats.github.io/effectsize/reference/diff_to_cles.md)).

## Plotting with `see`

The `see` package contains relevant plotting functions. See the
[plotting vignette in the `see`
package](https://easystats.github.io/see/articles/effectsize.html).

## See also

[`insight::display()`](https://easystats.github.io/insight/reference/display.html)
