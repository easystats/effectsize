# library(base)     # 4.2.2
library(effectsize) # 0.8.3.9

# Table 5 -------------------------------------

O <- c(90, 10)

p_E <- c(0.5, 0.5)
cohens_w(O, p = p_E)
#> Cohen's w |       95% CI
#> ------------------------
#> 0.80      | [0.64, 1.00]
#>
#> - One-sided CIs: upper bound fixed at [1.00].

p_E <- c(0.35, 0.65)
cohens_w(O, p = p_E)
#> Cohen's w |       95% CI
#> ------------------------
#> 1.15      | [0.99, 1.36]
#>
#> - One-sided CIs: upper bound fixed at [1.36~].

O <- c(5, 10, 80, 5)
p_E <- c(0.25, 0.25, 0.25, 0.25)
cohens_w(O, p = p_E)
#> Cohen's w |       95% CI
#> ------------------------
#> 1.27      | [1.10, 1.73]
#>
#> - One-sided CIs: upper bound fixed at [1.73~].


# Table 6 -------------------------------------

O <- c(90, 10)
p_E <- c(0.5, 0.5)
fei(O, p = p_E)
#> Fei  |       95% CI
#> -------------------
#> 0.80 | [0.64, 1.00]
#>
#> - One-sided CIs: upper bound fixed at [1.00].

p_E <- c(0.35, 0.65)
fei(O, p = p_E)
#> Fei  |       95% CI
#> -------------------
#> 0.85 | [0.73, 1.00]
#>
#> - Adjusted for uniform expected probabilities.
#> - One-sided CIs: upper bound fixed at [1.00].

O <- c(5, 10, 80, 5)
p_E <- c(0.25, 0.25, 0.25, 0.25)
fei(O, p = p_E)
#> Fei  |       95% CI
#> -------------------
#> 0.73 | [0.64, 1.00]
#>
#> - Adjusted for non-uniform expected probabilities.
#> - One-sided CIs: upper bound fixed at [1.00].
