# Workers' Salary and Other Information

A sample (simulated) dataset, used in tests and some examples.

## Format

A data frame with 500 rows and 5 variables:

- salary:

  Salary, in Shmekels

- xtra_hours:

  Number of overtime hours (on average, per week)

- n_comps:

  Number of compliments given to the boss (observed over the last week)

- age:

  Age in years

- seniority:

  How many years with the company

- is_senior:

  Has this person been working here for more than 4 years?

    data("hardlyworking")
    head(hardlyworking, n = 5)
    #>     salary xtra_hours n_comps age seniority is_senior
    #> 1 19744.65       4.16       1  32         3     FALSE
    #> 2 11301.95       1.62       0  34         3     FALSE
    #> 3 20635.62       1.19       3  33         5      TRUE
    #> 4 23047.16       7.19       1  35         3     FALSE
    #> 5 27342.15      11.26       0  33         4     FALSE

## See also

Other effect size datasets:
[`Music_preferences`](https://easystats.github.io/effectsize/reference/Music_preferences.md),
[`Music_preferences2`](https://easystats.github.io/effectsize/reference/Music_preferences2.md),
[`RCT_table`](https://easystats.github.io/effectsize/reference/RCT_table.md),
[`Smoking_FASD`](https://easystats.github.io/effectsize/reference/Smoking_FASD.md),
[`food_class`](https://easystats.github.io/effectsize/reference/food_class.md),
[`preferences2025`](https://easystats.github.io/effectsize/reference/preferences2025.md),
[`rouder2016`](https://easystats.github.io/effectsize/reference/rouder2016.md),
[`screening_test`](https://easystats.github.io/effectsize/reference/screening_test.md)
