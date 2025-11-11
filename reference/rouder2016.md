# Jeff Rouder's Example Dataset for Repeated Measures

A dataset "with 25 people each observing 50 trials in 2 conditions",
published as `effectSizePuzzler.txt` by Jeff Rouder on March 24, 2016
(*http://jeffrouder.blogspot.com/2016/03/the-effect-size-puzzler.html*).  
  
The data is used in examples and tests of
[`rm_d()`](https://easystats.github.io/effectsize/reference/repeated_measures_d.md).

## Format

A data frame with 2500 rows and 3 variables:

- id:

  participant: 1...25

- cond:

  condition: 1,2

- rt:

  response time in seconds

    data("rouder2016")
    head(rouder2016, n = 5)
    #>   id cond    rt
    #> 1  1    1 0.560
    #> 2  1    1 0.930
    #> 3  1    1 0.795
    #> 4  1    1 0.615
    #> 5  1    1 1.028

## See also

Other effect size datasets:
[`Music_preferences`](https://easystats.github.io/effectsize/reference/Music_preferences.md),
[`Music_preferences2`](https://easystats.github.io/effectsize/reference/Music_preferences2.md),
[`RCT_table`](https://easystats.github.io/effectsize/reference/RCT_table.md),
[`Smoking_FASD`](https://easystats.github.io/effectsize/reference/Smoking_FASD.md),
[`food_class`](https://easystats.github.io/effectsize/reference/food_class.md),
[`hardlyworking`](https://easystats.github.io/effectsize/reference/hardlyworking.md),
[`preferences2025`](https://easystats.github.io/effectsize/reference/preferences2025.md),
[`screening_test`](https://easystats.github.io/effectsize/reference/screening_test.md)
