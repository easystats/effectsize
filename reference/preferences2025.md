# Preferences of Poop vs Chocolate

A subset of Hussey and Cummins replication of *Balcetis & Dunning (2010)
Study 3b*. Each scale is average of 3 items both for "a toilet filled
with human poop" and for and for "a toilet filled with human poop" (how
positive or negative / pleasant or unpleasant / good or bad).

## Format

A data frame with 489 rows and 3 variables:

- participant_id:

  Unique identifier for each participant

- poop:

  Preference rating of poop (1 = low, 7 = high)

- chocolate:

  Preference rating of poop (1 = low, 7 = high)

    data("preferences2025")
    head(preferences2025, n = 5)
    #>   participant_id     poop chocolate
    #> 1          088cf 4.000000         6
    #> 2          bcae1 2.333333         7
    #> 3          d21b0 1.000000         7
    #> 4          2019a 1.000000         7
    #> 5          c1b73 3.000000         7

## References

Hussey, I., & Cummins, J. (2025). (Not so) simple preferences.
https://github.com/ianhussey/not-so-simple-preferences

## See also

[`repeated_measures_d()`](https://easystats.github.io/effectsize/reference/repeated_measures_d.md)

Other effect size datasets:
[`Music_preferences`](https://easystats.github.io/effectsize/reference/Music_preferences.md),
[`Music_preferences2`](https://easystats.github.io/effectsize/reference/Music_preferences2.md),
[`RCT_table`](https://easystats.github.io/effectsize/reference/RCT_table.md),
[`Smoking_FASD`](https://easystats.github.io/effectsize/reference/Smoking_FASD.md),
[`food_class`](https://easystats.github.io/effectsize/reference/food_class.md),
[`hardlyworking`](https://easystats.github.io/effectsize/reference/hardlyworking.md),
[`rouder2016`](https://easystats.github.io/effectsize/reference/rouder2016.md),
[`screening_test`](https://easystats.github.io/effectsize/reference/screening_test.md)
