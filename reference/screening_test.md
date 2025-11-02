# Results from 2 Screening Tests

A sample (simulated) dataset, used in tests and some examples.

## Format

A data frame with 1600 rows and 3 variables:

- Diagnosis:

  Ground truth

- Test1:

  Results given by the 1st test

- Test2:

  Results given by the 2nd test

    data("screening_test")
    head(screening_test, n = 5)
    #>   Diagnosis Test1 Test2
    #> 1       Neg "Neg" "Neg"
    #> 2       Neg "Neg" "Neg"
    #> 3       Neg "Neg" "Neg"
    #> 4       Neg "Neg" "Neg"
    #> 5       Neg "Neg" "Neg"

## See also

Other effect size datasets:
[`Music_preferences`](https://easystats.github.io/effectsize/reference/Music_preferences.md),
[`Music_preferences2`](https://easystats.github.io/effectsize/reference/Music_preferences2.md),
[`RCT_table`](https://easystats.github.io/effectsize/reference/RCT_table.md),
[`Smoking_FASD`](https://easystats.github.io/effectsize/reference/Smoking_FASD.md),
[`food_class`](https://easystats.github.io/effectsize/reference/food_class.md),
[`hardlyworking`](https://easystats.github.io/effectsize/reference/hardlyworking.md),
[`rouder2016`](https://easystats.github.io/effectsize/reference/rouder2016.md)
