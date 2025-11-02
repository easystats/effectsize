# Checks for a Valid Effect Size Name

For use by other functions and packages.

## Usage

``` r
is_effectsize_name(x, ignore_case = TRUE)

get_effectsize_name(x, ignore_case = TRUE)

get_effectsize_label(
  x,
  ignore_case = TRUE,
  use_symbols = getOption("es.use_symbols", FALSE)
)
```

## Arguments

- x:

  A character, or a vector.

- ignore_case:

  Should case of input be ignored?

- use_symbols:

  Should proper symbols be printed (`TRUE`) instead of transliterated
  effect size names (`FALSE`). See
  [effectsize_options](https://easystats.github.io/effectsize/reference/effectsize_options.md).
