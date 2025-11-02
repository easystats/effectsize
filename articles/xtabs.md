# Effect Sizes for Contingency Tables

This vignette provides a review of effect sizes for 1- and 2-D
contingency tables, which are typically analysed with
[`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) and
[`fisher.test()`](https://rdrr.io/r/stats/fisher.test.html).

``` r

library(effectsize)
options(es.use_symbols = TRUE) # get nice symbols when printing! (On Windows, requires R >= 4.2.0)
```

## Nominal Correlation

### 2-by-2 Tables

For 2-by-2 contingency tables, \phi (Phi) is homologous (though
directionless) to the biserial correlation between two dichotomous
variables, with 0 representing no association, and 1 representing a
perfect association.

``` r

(MPG_Gear <- table(mtcars$mpg < 20, mtcars$vs))
```

    >        
    >          0  1
    >   FALSE  3 11
    >   TRUE  15  3

``` r

phi(MPG_Gear, adjust = FALSE)
```

    > ϕ    |       95% CI
    > -------------------
    > 0.62 | [0.33, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

``` r

# Same as:
cor(mtcars$mpg < 20, mtcars$vs)
```

    > [1] -0.619

A “cousin” effect size is Pearson’s contingency coefficient, however it
is not a true measure of correlation, but rather a type of normalized
\chi^2 (see
[`chisq_to_pearsons_c()`](https://easystats.github.io/effectsize/reference/chisq_to_phi.md)):

``` r

pearsons_c(MPG_Gear)
```

    > Pearson's C |       95% CI
    > --------------------------
    > 0.53        | [0.31, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

### Larger Tables

For larger contingency tables, Cramér’s *V*, Tschuprow’s *T*, Cohen’s
*w*, and Pearson’s *C* can be used.

Both Cramér’s *V* and Tschuprow’s *T* are true measures of correlation:
they range from 0 to 1, with 0 indicating complete independence between
the nominal variables, and 1 indicating complete dependence. For square
tables, they are equal, however for non-square tables T \< V; While
Cramér’s *V* defines complete dependence as “it is possible to know
exactly the value of the columns from the rows *or* know exactly the
value of the rows from the columns”, Tschuprow’s *T* required both to be
true to achieve complete dependence - something that is not possible for
non-square tables. For example:

``` r

data("food_class")
food_class
```

    >           Soy Milk Meat
    > Vegan      47    0    0
    > Not-Vegan   0   12   21

In this case, if you know the food product, you know if it is vegan or
not, but knowing if the food is vegan or not will not always let you
know what food product it is.

``` r

cramers_v(food_class, adjust = FALSE)
```

    > Cramer's V |       95% CI
    > -------------------------
    > 1          | [0.81, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

``` r

tschuprows_t(food_class, adjust = FALSE)
```

    > Tschuprow's T |       95% CI
    > ----------------------------
    > 0.84          | [0.68, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

Cohen’s *w* and Pearson’s *C* are not true measures of correlation, but
are two types of normalized \chi^2 values. While Pearson’s *C* is capped
at 1, Cohen’s *w* can be larger than 1 (for both, 0 indicates no
association between the variables).

``` r

data("Music_preferences2")
Music_preferences2
```

    >       Pop Rock Jazz Classic
    > Psych 151  130   12       7
    > Econ   77    6  111       4
    > Law     0    4    2     165

``` r

chisq.test(Music_preferences2)
```

    > 
    >   Pearson's Chi-squared test
    > 
    > data:  Music_preferences2
    > X-squared = 854, df = 6, p-value <2e-16

``` r

cramers_v(Music_preferences2)
```

    > Cramer's V (adj.) |       95% CI
    > --------------------------------
    > 0.80              | [0.75, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

``` r

tschuprows_t(Music_preferences2)
```

    > Tschuprow's T (adj.) |       95% CI
    > -----------------------------------
    > 0.72                 | [0.68, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

``` r

pearsons_c(Music_preferences2)
```

    > Pearson's C |       95% CI
    > --------------------------
    > 0.75        | [0.73, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

``` r

cohens_w(Music_preferences2) # > 1
```

    > Cohen's w |       95% CI
    > ------------------------
    > 1.13      | [1.06, 1.41]
    > 
    > - One-sided CIs: upper bound fixed at [1.41~].

(Cramer’s *V*, Tschuprow’s *T*, and Cohen’s *w* can also be used for
2-by-2 tables, but there they are equivalent to \phi.)

### For a Bayesian \chi^2-test

A Bayesian estimate of these effect sizes can also be provided based on
`BayesFactor`’s version of a \chi^2-test via the
[`effectsize()`](https://easystats.github.io/effectsize/reference/effectsize.md)
function:

``` r

library(BayesFactor)
BFX <- contingencyTableBF(MPG_Gear, sampleType = "jointMulti")

effectsize(BFX, type = "phi") # for 2 * 2
```

    > ϕ (adj.) |       95% CI
    > -----------------------
    > 0.53     | [0.17, 0.76]

``` r

BFX <- contingencyTableBF(Music_preferences2, sampleType = "jointMulti")

effectsize(BFX, type = "cramers_v")
```

    > Cramer's V (adj.) |       95% CI
    > --------------------------------
    > 0.78              | [0.75, 0.81]

``` r

effectsize(BFX, type = "tschuprows_t")
```

    > Tschuprow's T (adj.) |       95% CI
    > -----------------------------------
    > 0.71                 | [0.68, 0.73]

``` r

effectsize(BFX, type = "cohens_w")
```

    > Cohen's w |       95% CI
    > ------------------------
    > 1.11      | [1.06, 1.15]

``` r

effectsize(BFX, type = "pearsons_c")
```

    > Pearson's C |       95% CI
    > --------------------------
    > 0.74        | [0.73, 0.75]

## Goodness of Fit

Cohen’s *w* and Pearson’s *C* are also applicable to tests of
goodness-of-fit, where they indicate the degree of deviation from the
hypothetical probabilities, with 0 reflecting no deviation.

``` r

O <- c(89, 37, 130, 28, 2) # observed group sizes
E <- c(.40, .20, .20, .15, .05) # expected group freq

chisq.test(O, p = E)
```

    > 
    >   Chi-squared test for given probabilities
    > 
    > data:  O
    > X-squared = 121, df = 4, p-value <2e-16

``` r

pearsons_c(O, p = E)
```

    > Pearson's C |       95% CI
    > --------------------------
    > 0.55        | [0.48, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

``` r

cohens_w(O, p = E)
```

    > Cohen's w |       95% CI
    > ------------------------
    > 0.65      | [0.54, 4.36]
    > 
    > - One-sided CIs: upper bound fixed at [4.36~].

However, these values are not (anti)correlations - they do not properly
adjust for the “shape” of the expected multinational distribution,
making them inflated (additionally, Cohen’s *w* can be larger than 1,
making it harder to interpret).

For these reasons, we recommend the פ (Fei) coefficient, which is a
measure of anti-correlation between the observed and the expected
distributions, ranging from 0 (observed distribution matches the
expected distribution perfectly) and 1 (the observed distribution is
maximally different than the expected one).

``` r

fei(O, p = E)
```

    > פ‎    |       95% CI
    > -------------------
    > 0.15 | [0.13, 1.00]
    > 
    > - Adjusted for uniform expected probabilities.
    > - One-sided CIs: upper bound fixed at [1.00].

``` r

# Observed perfectly matches Expected
(O1 <- E * 286)
```

    > [1] 114.4  57.2  57.2  42.9  14.3

``` r

fei(O1, p = E)
```

    > פ‎ |       95% CI
    > ----------------
    > 0 | [0.00, 1.00]
    > 
    > - Adjusted for uniform expected probabilities.
    > - One-sided CIs: upper bound fixed at [1.00].

``` r

# Observed deviates maximally from Expected:
# All observed values are in the least expected class!
(O2 <- c(rep(0, 4), 286))
```

    > [1]   0   0   0   0 286

``` r

fei(O2, p = E)
```

    > פ‎    |       95% CI
    > -------------------
    > 1.00 | [0.98, 1.00]
    > 
    > - Adjusted for uniform expected probabilities.
    > - One-sided CIs: upper bound fixed at [1.00].

## Differences in Proportions

For 2-by-2 tables, we can also compute the Odds-ratio (OR), where each
column represents a different *group*. Values larger than 1 indicate
that the odds are higher in the first group (and vice versa).

``` r

data("RCT_table")
RCT_table
```

    >            Group
    > Diagnosis   Treatment Control
    >   Sick             71      30
    >   Recovered        50     100

``` r

chisq.test(RCT_table) # or fisher.test(RCT_table)
```

    > 
    >   Pearson's Chi-squared test with Yates' continuity correction
    > 
    > data:  RCT_table
    > X-squared = 32, df = 1, p-value = 2e-08

``` r

oddsratio(RCT_table)
```

    > Odds ratio |       95% CI
    > -------------------------
    > 4.73       | [2.74, 8.17]

We can also compute the Risk-ratio (RR), which is the ratio between the
proportions of the two groups, and the Absolute Risk Reduction (ARR),
which is the *difference* between the proportions of the two groups -
both are measures which some claim to be more intuitive.

``` r

riskratio(RCT_table)
```

    > Risk ratio |       95% CI
    > -------------------------
    > 2.54       | [1.80, 3.60]

``` r

arr(RCT_table)
```

    > ARR  |       95% CI
    > -------------------
    > 0.36 | [0.24, 0.47]

Additionally, Cohen’s *h* can also be computed, which uses the *arcsin*
transformation. Negative values indicate smaller proportion in the first
group (and vice versa).

``` r

cohens_h(RCT_table)
```

    > Cohen's h |       95% CI
    > ------------------------
    > 0.74      | [0.50, 0.99]

### For a Bayesian \chi^2-test

A Bayesian estimate of these effect sizes can also be provided based on
`BayesFactor`’s version of a \chi^2-test via the
[`effectsize()`](https://easystats.github.io/effectsize/reference/effectsize.md)
function:

``` r

BFX <- contingencyTableBF(RCT_table, sampleType = "jointMulti")

effectsize(BFX, type = "or")
```

    > Odds ratio |       95% CI
    > -------------------------
    > 4.63       | [2.71, 8.08]

``` r

effectsize(BFX, type = "rr")
```

    > Risk ratio |       95% CI
    > -------------------------
    > 2.48       | [1.82, 3.59]

``` r

effectsize(BFX, type = "cohens_h")
```

    > Cohen's h |       95% CI
    > ------------------------
    > 0.74      | [0.49, 0.98]

## Paired Contingency Tables

For dependent (paired) contingency tables, Cohen’s *g* represents the
symmetry of the table, ranging between 0 (perfect symmetry) and 0.5
(perfect asymmetry).

For example, these two tests seem to be equally predictive of the
disease they are screening:

``` r

data("screening_test")

phi(screening_test$Diagnosis, screening_test$Test1)
```

    > ϕ (adj.) |       95% CI
    > -----------------------
    > 0.85     | [0.81, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

``` r

phi(screening_test$Diagnosis, screening_test$Test2)
```

    > ϕ (adj.) |       95% CI
    > -----------------------
    > 0.85     | [0.81, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

Does this mean they give the same number of positive/negative results?

``` r

tests <- table(Test1 = screening_test$Test1, Test2 = screening_test$Test2)
tests
```

    >        Test2
    > Test1   "Neg" "Pos"
    >   "Neg"   794    86
    >   "Pos"   150   570

``` r

mcnemar.test(tests)
```

    > 
    >   McNemar's Chi-squared test with continuity correction
    > 
    > data:  tests
    > McNemar's chi-squared = 17, df = 1, p-value = 4e-05

``` r

cohens_g(tests)
```

    > Cohen's g |       95% CI
    > ------------------------
    > 0.14      | [0.07, 0.19]

Test 1 gives more positive results than test 2!

## References
