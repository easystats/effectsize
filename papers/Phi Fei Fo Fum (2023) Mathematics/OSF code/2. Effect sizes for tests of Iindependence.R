# library(base)      # 4.2.2
library(effectsize) # 0.8.3.9
library(correlation) # 0.8.3
# library(tidyr)     # 1.3.0



# Table 1 ---------------------------

data("Titanic", package = "datasets")

# Collapse across Class and Age (make a 2-D table)
Titanic_xtab <- apply(Titanic, MARGIN = c(2, 4), FUN = sum)
Titanic_xtab
#>         Survived
#> Sex        No Yes
#>   Male   1364 367
#>   Female  126 344

chisq.test(Titanic_xtab, correct = FALSE)
#>
#> 	Pearson's Chi-squared test
#>
#> data:  Titanic_xtab
#> X-squared = 456.87, df = 1, p-value < 2.2e-16
#>


# Table 2 ---------------------------

phi(Titanic_xtab, adjust = FALSE)
#> Phi  |       95% CI
#> -------------------
#> 0.46 | [0.42, 1.00]
#>
#> - One-sided CIs: upper bound fixed at [1.00].

Titanic_data <- Titanic_xtab |>
  as.table() |>
  as.data.frame() |>
  tidyr::uncount(weights = Freq) |>
  transform(
    Survived = Survived == "Yes",
    Sex = Sex == "Male"
  )

correlation(Titanic_data, p_adjust = "none")
#> # Correlation Matrix (pearson-method)
#>
#> Parameter1 | Parameter2 |     r |         95% CI | t(2199) |         p
#> ----------------------------------------------------------------------
#> Sex        |   Survived | -0.46 | [-0.49, -0.42] |  -24.00 | < .001***
#>
#> p-value adjustment method: none
#> Observations: 2201


# Table 3 ---------------------------

# Collapse across Sex and Age (make a 2-D table)
Titanic_xtab2 <- apply(Titanic, MARGIN = c(1, 4), FUN = sum)
Titanic_xtab2
#>       Survived
#> Class   No Yes
#>   1st  122 203
#>   2nd  167 118
#>   3rd  528 178
#>   Crew 673 212

cramers_v(Titanic_xtab2, adjust = FALSE)
#> Cramer's V |       95% CI
#> -------------------------
#> 0.29       | [0.26, 1.00]
#>
#> - One-sided CIs: upper bound fixed at [1.00].


# Table 4 ---------------------------

data("food_class", package = "effectsize")
food_class
#>           Soy Milk Meat
#> Vegan      47    0    0
#> Not-Vegan   0   12   21

cramers_v(food_class, adjust = FALSE)
#> Cramer's V |       95% CI
#> -------------------------
#> 1.00       | [0.81, 1.00]
#>
#> - One-sided CIs: upper bound fixed at [1.00].

tschuprows_t(food_class, adjust = FALSE)
#> Tschuprow's T |       95% CI
#> ----------------------------
#> 0.84          | [0.68, 1.00]
#>
#> - One-sided CIs: upper bound fixed at [1.00].
