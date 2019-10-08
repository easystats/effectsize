Parameters Standardization
================

  - [Introduction](#introduction)
  - [How to interpret standardized
    coefficients?](#how-to-interpret-standardized-coefficients)
  - [Standardization methods](#standardization-methods)
      - [**“refit”**: Re-fitting the model with standardized
        data](#refit-re-fitting-the-model-with-standardized-data)
      - [**“2SD”**: Scaling by two 2 SDs](#sd-scaling-by-two-2-sds)
      - [**“smart”**: Scaling by the variances of the response and the
        predictor](#smart-scaling-by-the-variances-of-the-response-and-the-predictor)
      - [**“classic”**: Basic scaling of all
        parameters](#classic-basic-scaling-of-all-parameters)
      - [Methods Comparison](#methods-comparison)
          - [Models with only numeric
            predictors](#models-with-only-numeric-predictors)
          - [Models with factors](#models-with-factors)
          - [Models with interactions](#models-with-interactions)
      - [Conclusion](#conclusion)
  - [References](#references)

# Introduction

Standardising parameters (*i.e.*, coefficients) can allow for their
comparison within and between models, variables and studies. Moreover,
as it returns coefficients expressed in terms of **change of variance**
(for instance, coefficients expresed in terms of SD of the response
variable), it can allow for the usage of [effect size interpretation
guidelines](https://easystats.github.io/easystats/articles/interpret.html),
such as the famous Cohen’s (1988) rules of thumb.

However, standardizing the model’s parameters should *not* be
automatically and mindlessly done: for some research fields, particular
variables or types of studies (*e.g.*, replications), it sometimes makes
more sense to keep, use and interpret the original parameters,
especially if they are well known or easily understood.

Critically, **parameters standardization is not a trivial process**.
Different techniques exist, that can lead to drastically different
results. Thus, it is critical that the standardization method is
explicitly documented and detailed.

**`parameters` include different techniques of parameters
standardization**, described below (Bring 1994; Menard 2004, 2011;
Gelman 2008; Schielzeth 2010).

# How to interpret standardized coefficients?

Cohen’s d? Correlations r?

# Standardization methods

### **“refit”**: Re-fitting the model with standardized data

**This method is based on a complete model re-fit with a standardized
version of data**. Hence, this method is equal to standardizing the
variables before fitting the model. It is the “purest” and the most
accurate (Neter, Wasserman, and Kutner 1989), but it is also the most
computationally costly and long (especially for Bayesian models). This
method is particularly recommended for complex models that include
interactions or transformations (*e.g.*, polynomial or spline terms).

``` r
library(effectsize)

data <- iris
model <- lm(Sepal.Length ~ Petal.Width + Sepal.Width, data=data)

standardize_parameters(model, method="refit")
```

| Parameter   | Std\_Coefficient |
| :---------- | ---------------: |
| (Intercept) |             0.00 |
| Petal.Width |             0.89 |
| Sepal.Width |             0.21 |

The `robust` (default to `FALSE`) argument enables a **robust
standardization of data**, *i.e.*, based on the **median** and **MAD**
instead of the **mean** and **SD**.

``` r
standardize_parameters(model, method="refit", robust=TRUE)
```

| Parameter   | Std\_Coefficient |
| :---------- | ---------------: |
| (Intercept) |             0.11 |
| Petal.Width |             0.97 |
| Sepal.Width |             0.17 |

This method is very flexible as it can be applied to all types of models
(linear, logistic…).

``` r
data$binary <- ifelse(data$Sepal.Width > 3, 1, 0)
model <- glm(binary ~ Species + Sepal.Length, data = data, family="binomial")
standardize_parameters(model, method="refit")
```

| Parameter         | Std\_Coefficient |
| :---------------- | ---------------: |
| (Intercept)       |              3.3 |
| Speciesversicolor |            \-5.4 |
| Speciesvirginica  |            \-5.5 |
| Sepal.Length      |              1.5 |

### **“2SD”**: Scaling by two 2 SDs

Same as `method = "refit"`, however, standardization is done by dividing
by two times the `SD` or `MAD` (depending on `robust`). This method is
useful to obtain coefficients of continuous parameters comparable to
coefficients related to binary predictors (Gelman 2008).

### **“smart”**: Scaling by the variances of the response and the predictor

Post-hoc standardization of the model paramaters. The coefficients are
divided by the standard deviation (or MAD if `robust`) of the outcome
(which becomes their expression ‘unit’). Then, the coefficients related
to numeric variables are additionaly multiplied by the standard
deviation (or MAD if `robust`) of the related term, so that they
correspond to changes of 1 SD of the predictor (e.g., "A change in 1 SD
of `x` is related to a change of 0.24 of the SD of `y`). This does not
apply to binary variables or factors, so the coefficients are still
related to changes in levels.

``` r
model <- lm(Sepal.Length ~ Petal.Width + Sepal.Width, data=data)
standardize_parameters(model, method="smart")
```

| Parameter   | Std\_Coefficient |
| :---------- | ---------------: |
| (Intercept) |             0.00 |
| Petal.Width |             0.89 |
| Sepal.Width |             0.21 |

### **“classic”**: Basic scaling of all parameters

This method is similar to `method = "smart"`, but treats all variables
as continuous: it also scales the coefficient by the standard deviation
of factors (transformed to integers) or binary predictors. Altough being
inapropriate for these cases, this method is the one implemented by
default in other softwares, such as `lm.beta::lm.beta()`.

## Methods Comparison

We will use the “refit” method as the baseline. We will then compute the
differences between these standardized parameters and the ones provided
by the other functions. The **bigger the (absolute) number, the worse it
is**.

> **SPOILER ALERT: the standardization implemented in `parameters` is
> the most accurate and the most flexible.**

``` r
library(effectsize)
library(lm.beta)
library(MuMIn)

comparison <- function(model, robust=FALSE){
  out <- standardize_parameters(model, method="refit", robust=robust)[1:2]
  
  out$smart <- tryCatch({
    out[, 2] - standardize_parameters(model, method="smart", robust=robust)[, 2]
}, error = function(error_condition) {
    "Error"
})
  out$classic <- tryCatch({
    out[, 2] - standardize_parameters(model, method="classic", robust=robust)[, 2]
}, error = function(error_condition) {
    "Error"
})

  out$lm.beta <- tryCatch({
    out[, 2] - lm.beta::lm.beta(model)$standardized.coefficients
}, error = function(error_condition) {
    "Error"
}, warning = function(warning_condition) {
  "Error"
})
  
  out$MuMIn <- tryCatch({
    out[, 2] - MuMIn::std.coef(model, partial.sd=FALSE)[, 1]
}, error = function(error_condition) {
    "Error"
})

  out[, 2] <- NULL
  out
}
```

### Models with only numeric predictors

#### Linear Model

``` r
data <- iris
data$Group_Sepal.Width <- as.factor(ifelse(data$Sepal.Width > 3, "High", "Low"))
data$Binary_Sepal.Width <- as.factor(ifelse(data$Sepal.Width > 3, 1, 0))

model <- lm(Sepal.Length ~ Petal.Width + Sepal.Width, data=data) 
comparison(model)
```

| Parameter   | smart | classic | lm.beta | MuMIn |
| :---------- | ----: | ------: | ------: | ----: |
| (Intercept) |     0 |       0 |       0 |     0 |
| Petal.Width |     0 |       0 |       0 |     0 |
| Sepal.Width |     0 |       0 |       0 |     0 |

For this simple model, **all methods return results equal to the “refit”
method**.

#### Logistic Model

``` r
model <- glm(Binary_Sepal.Width ~ Petal.Width + Sepal.Length, data=data, family="binomial")
comparison(model)
```

| Parameter    |  smart | classic | lm.beta | MuMIn |
| :----------- | -----: | ------: | :------ | :---- |
| (Intercept)  | \-0.26 |  \-0.26 | Error   | Error |
| Petal.Width  |   0.00 |    0.00 | Error   | Error |
| Sepal.Length |   0.00 |    0.00 | Error   | Error |

#### Linear Mixed Model

``` r
library(lme4)

model <- lme4::lmer(Sepal.Length ~ Petal.Width + Sepal.Width + (1|Species), data=data)
comparison(model)
```

| Parameter   | smart | classic | lm.beta | MuMIn |
| :---------- | ----: | ------: | :------ | ----: |
| (Intercept) |     0 |       0 | Error   |     0 |
| Petal.Width |     0 |       0 | Error   |     0 |
| Sepal.Width |     0 |       0 | Error   |     0 |

For this simple mixed model, **all methods return results equal to the
“refit” method**.

When interactions are involved, post-hoc methods return different
results. However, methods implemented in other softwares perform
arguably worse.

#### Transformation

``` r
model <- lm(Sepal.Length ~ poly(Petal.Width, 2) + poly(Sepal.Width, 2), data=data)
comparison(model)
```

| Parameter             | smart | classic | lm.beta |  MuMIn |
| :-------------------- | ----: | ------: | ------: | -----: |
| (Intercept)           |     0 |    0.00 |    0.00 |   0.00 |
| poly(Petal.Width, 2)1 |     0 |   10.48 |   10.48 |  10.48 |
| poly(Petal.Width, 2)2 |     0 |  \-1.86 |  \-1.86 | \-1.86 |
| poly(Sepal.Width, 2)1 |     0 |    3.44 |    3.44 |   3.44 |
| poly(Sepal.Width, 2)2 |     0 |    0.28 |    0.28 |   0.28 |

For polynomial transformations, other software become very unreliable.

#### Bayesian Models

``` r
library(rstanarm)

model <- stan_glm(Sepal.Length ~ Petal.Width + Sepal.Width, data=data)
comparison(model)
```

| Parameter   | smart | classic | lm.beta | MuMIn |
| :---------- | ----: | ------: | ------: | ----: |
| (Intercept) |     0 |       0 |       0 |     0 |
| Petal.Width |     0 |       0 |       0 |     0 |
| Sepal.Width |     0 |       0 |       0 |     0 |

### Models with factors

#### Linear Model

``` r
model <- lm(Sepal.Length ~ Petal.Width + Group_Sepal.Width, data=data) 
comparison(model)
```

| Parameter             | smart | classic | lm.beta |  MuMIn |
| :-------------------- | ----: | ------: | ------: | -----: |
| (Intercept)           |  0.14 |    0.14 |    0.14 |   0.14 |
| Petal.Width           |  0.00 |    0.00 |    0.00 |   0.00 |
| Group\_Sepal.WidthLow |  0.00 |  \-0.12 |  \-0.12 | \-0.12 |

When factors are involved, methods that standardize the numeric
transformation of factors give different results.

#### Logistic Model

``` r
model <- glm(Binary_Sepal.Width ~ Petal.Width + Species, data=data, family="binomial")
comparison(model)
```

| Parameter         | smart | classic | lm.beta | MuMIn |
| :---------------- | ----: | ------: | :------ | :---- |
| (Intercept)       |     8 |     8.0 | Error   | Error |
| Petal.Width       |     0 |     0.0 | Error   | Error |
| Speciesversicolor |     0 |   \-5.8 | Error   | Error |
| Speciesvirginica  |     0 |   \-7.6 | Error   | Error |

#### Linear Mixed Model

``` r
library(lme4)

model <- lme4::lmer(Sepal.Length ~ Petal.Length + Group_Sepal.Width + (1|Species), data=data)
comparison(model)
```

| Parameter             | smart | classic | lm.beta |  MuMIn |
| :-------------------- | ----: | ------: | :------ | -----: |
| (Intercept)           |  0.15 |    0.15 | Error   |   0.15 |
| Petal.Length          |  0.00 |    0.00 | Error   |   0.00 |
| Group\_Sepal.WidthLow |  0.00 |  \-0.14 | Error   | \-0.14 |

#### Bayesian Models

``` r
library(rstanarm)

model <- stan_glm(Sepal.Length ~ Petal.Width + Group_Sepal.Width, data=data)
```

    > 
    > SAMPLING FOR MODEL 'continuous' NOW (CHAIN 1).
    > Chain 1: 
    > Chain 1: Gradient evaluation took 0.001 seconds
    > Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 10 seconds.
    > Chain 1: Adjust your expectations accordingly!
    > Chain 1: 
    > Chain 1: 
    > Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
    > Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
    > Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
    > Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
    > Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
    > Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    > Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    > Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    > Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    > Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    > Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    > Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
    > Chain 1: 
    > Chain 1:  Elapsed Time: 0.081 seconds (Warm-up)
    > Chain 1:                0.087 seconds (Sampling)
    > Chain 1:                0.168 seconds (Total)
    > Chain 1: 
    > 
    > SAMPLING FOR MODEL 'continuous' NOW (CHAIN 2).
    > Chain 2: 
    > Chain 2: Gradient evaluation took 0 seconds
    > Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    > Chain 2: Adjust your expectations accordingly!
    > Chain 2: 
    > Chain 2: 
    > Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
    > Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
    > Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
    > Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
    > Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
    > Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    > Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    > Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    > Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    > Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    > Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    > Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
    > Chain 2: 
    > Chain 2:  Elapsed Time: 0.08 seconds (Warm-up)
    > Chain 2:                0.08 seconds (Sampling)
    > Chain 2:                0.16 seconds (Total)
    > Chain 2: 
    > 
    > SAMPLING FOR MODEL 'continuous' NOW (CHAIN 3).
    > Chain 3: 
    > Chain 3: Gradient evaluation took 0 seconds
    > Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    > Chain 3: Adjust your expectations accordingly!
    > Chain 3: 
    > Chain 3: 
    > Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
    > Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
    > Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
    > Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
    > Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
    > Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    > Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    > Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    > Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    > Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    > Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    > Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
    > Chain 3: 
    > Chain 3:  Elapsed Time: 0.076 seconds (Warm-up)
    > Chain 3:                0.081 seconds (Sampling)
    > Chain 3:                0.157 seconds (Total)
    > Chain 3: 
    > 
    > SAMPLING FOR MODEL 'continuous' NOW (CHAIN 4).
    > Chain 4: 
    > Chain 4: Gradient evaluation took 0 seconds
    > Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    > Chain 4: Adjust your expectations accordingly!
    > Chain 4: 
    > Chain 4: 
    > Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
    > Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
    > Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
    > Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
    > Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
    > Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    > Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    > Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    > Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    > Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    > Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    > Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
    > Chain 4: 
    > Chain 4:  Elapsed Time: 0.084 seconds (Warm-up)
    > Chain 4:                0.08 seconds (Sampling)
    > Chain 4:                0.164 seconds (Total)
    > Chain 4:

``` r
comparison(model)
```

    > # Description of Posterior Distributions
    > 
    >             Parameter    smart classic lm.beta   MuMIn
    >           (Intercept)  0.13599  0.1360  0.1360  0.1360
    >           Petal.Width  0.00440  0.0044  0.0044  0.0044
    >  Group_Sepal.WidthLow -0.00195 -0.1255 -0.1255 -0.1255

| Parameter             | smart | classic | lm.beta |  MuMIn |
| :-------------------- | ----: | ------: | ------: | -----: |
| (Intercept)           |  0.14 |    0.14 |    0.14 |   0.14 |
| Petal.Width           |  0.00 |    0.00 |    0.00 |   0.00 |
| Group\_Sepal.WidthLow |  0.00 |  \-0.12 |  \-0.12 | \-0.12 |

``` r
library(rstanarm)

model <- stan_lmer(Sepal.Length ~ Petal.Width + Group_Sepal.Width + (1|Species), data=data)
```

    > 
    > SAMPLING FOR MODEL 'continuous' NOW (CHAIN 1).
    > Chain 1: 
    > Chain 1: Gradient evaluation took 0 seconds
    > Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    > Chain 1: Adjust your expectations accordingly!
    > Chain 1: 
    > Chain 1: 
    > Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
    > Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
    > Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
    > Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
    > Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
    > Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    > Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    > Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    > Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    > Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    > Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    > Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
    > Chain 1: 
    > Chain 1:  Elapsed Time: 0.803 seconds (Warm-up)
    > Chain 1:                0.574 seconds (Sampling)
    > Chain 1:                1.377 seconds (Total)
    > Chain 1: 
    > 
    > SAMPLING FOR MODEL 'continuous' NOW (CHAIN 2).
    > Chain 2: 
    > Chain 2: Gradient evaluation took 0 seconds
    > Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    > Chain 2: Adjust your expectations accordingly!
    > Chain 2: 
    > Chain 2: 
    > Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
    > Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
    > Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
    > Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
    > Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
    > Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    > Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    > Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    > Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    > Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    > Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    > Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
    > Chain 2: 
    > Chain 2:  Elapsed Time: 0.766 seconds (Warm-up)
    > Chain 2:                0.582 seconds (Sampling)
    > Chain 2:                1.348 seconds (Total)
    > Chain 2: 
    > 
    > SAMPLING FOR MODEL 'continuous' NOW (CHAIN 3).
    > Chain 3: 
    > Chain 3: Gradient evaluation took 0 seconds
    > Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    > Chain 3: Adjust your expectations accordingly!
    > Chain 3: 
    > Chain 3: 
    > Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
    > Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
    > Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
    > Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
    > Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
    > Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    > Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    > Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    > Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    > Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    > Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    > Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
    > Chain 3: 
    > Chain 3:  Elapsed Time: 0.683 seconds (Warm-up)
    > Chain 3:                0.409 seconds (Sampling)
    > Chain 3:                1.092 seconds (Total)
    > Chain 3: 
    > 
    > SAMPLING FOR MODEL 'continuous' NOW (CHAIN 4).
    > Chain 4: 
    > Chain 4: Gradient evaluation took 0.001 seconds
    > Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 10 seconds.
    > Chain 4: Adjust your expectations accordingly!
    > Chain 4: 
    > Chain 4: 
    > Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
    > Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
    > Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
    > Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
    > Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
    > Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    > Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    > Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    > Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    > Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    > Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    > Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
    > Chain 4: 
    > Chain 4:  Elapsed Time: 0.855 seconds (Warm-up)
    > Chain 4:                0.685 seconds (Sampling)
    > Chain 4:                1.54 seconds (Total)
    > Chain 4:

``` r
comparison(model)
```

    > # Description of Posterior Distributions
    > 
    >             Parameter   smart  classic lm.beta MuMIn
    >           (Intercept) 0.15478  0.15478   Error Error
    >           Petal.Width 0.00749  0.00749   Error Error
    >  Group_Sepal.WidthLow 0.00141 -0.13911   Error Error

| Parameter             |  smart | classic | lm.beta | MuMIn |
| :-------------------- | -----: | ------: | :------ | :---- |
| (Intercept)           |   0.15 |    0.15 | Error   | Error |
| Petal.Width           |   0.00 |    0.00 | Error   | Error |
| Group\_Sepal.WidthLow | \-0.01 |  \-0.15 | Error   | Error |

### Models with interactions

#### Between continuous

``` r
model <- lm(Sepal.Length ~ Petal.Width * Sepal.Width, data=data)
comparison(model)
```

| Parameter               |  smart | classic | lm.beta |  MuMIn |
| :---------------------- | -----: | ------: | ------: | -----: |
| (Intercept)             | \-0.01 |  \-0.01 |  \-0.01 | \-0.01 |
| Petal.Width             | \-0.28 |  \-0.28 |  \-0.28 | \-0.28 |
| Sepal.Width             | \-0.06 |  \-0.06 |  \-0.06 | \-0.06 |
| Petal.Width:Sepal.Width |   0.01 |    0.24 |    0.24 |   0.24 |

#### Between factors

``` r
model <- lm(Sepal.Length ~ Species * Group_Sepal.Width, data=data)
comparison(model)
```

| Parameter                               |  smart | classic | lm.beta |  MuMIn |
| :-------------------------------------- | -----: | ------: | ------: | -----: |
| (Intercept)                             | \-0.93 |  \-0.93 |  \-0.93 | \-0.93 |
| Speciesversicolor                       |   0.00 |    0.90 |    0.90 |   0.90 |
| Speciesvirginica                        |   0.00 |    1.10 |    1.10 |   1.10 |
| Group\_Sepal.WidthLow                   |   0.00 |  \-0.27 |  \-0.27 | \-0.27 |
| Speciesversicolor:Group\_Sepal.WidthLow |   0.00 |  \-0.14 |  \-0.14 | \-0.14 |
| Speciesvirginica:Group\_Sepal.WidthLow  |   0.00 |    0.08 |    0.08 |   0.08 |

#### Between factors and continuous

``` r
model <- lm(Sepal.Length ~ Petal.Width * Group_Sepal.Width, data=data)
comparison(model)
```

| Parameter                         |  smart | classic | lm.beta |  MuMIn |
| :-------------------------------- | -----: | ------: | ------: | -----: |
| (Intercept)                       |   0.12 |    0.12 |    0.12 |   0.12 |
| Petal.Width                       |   0.00 |    0.00 |    0.00 |   0.00 |
| Group\_Sepal.WidthLow             |   0.27 |    0.01 |    0.01 |   0.01 |
| Petal.Width:Group\_Sepal.WidthLow | \-0.05 |  \-0.01 |  \-0.01 | \-0.01 |

``` r
model <- lm(Sepal.Length ~ Group_Sepal.Width * Petal.Width, data=data)
comparison(model)
```

| Parameter                         | smart | classic | lm.beta |  MuMIn |
| :-------------------------------- | ----: | ------: | ------: | -----: |
| (Intercept)                       |  0.12 |    0.12 |    0.12 |   0.12 |
| Group\_Sepal.WidthLow             |  0.27 |    0.01 |    0.01 |   0.01 |
| Petal.Width                       |  0.00 |    0.00 |    0.00 |   0.00 |
| Group\_Sepal.WidthLow:Petal.Width |  0.00 |  \-0.01 |  \-0.01 | \-0.01 |

## Conclusion

Use `refit` if possible, otherwise `smart`.

# References

<div id="refs" class="references">

<div id="ref-bring1994standardize">

Bring, Johan. 1994. “How to Standardize Regression Coefficients.” *The
American Statistician* 48 (3): 209–13.

</div>

<div id="ref-gelman2008scaling">

Gelman, Andrew. 2008. “Scaling Regression Inputs by Dividing by Two
Standard Deviations.” *Statistics in Medicine* 27 (15): 2865–73.

</div>

<div id="ref-menard2004six">

Menard, Scott. 2004. “Six Approaches to Calculating Standardized
Logistic Regression Coefficients.” *The American Statistician* 58 (3):
218–23.

</div>

<div id="ref-menard2011standards">

———. 2011. “Standards for Standardized Logistic Regression
Coefficients.” *Social Forces* 89 (4): 1409–28.

</div>

<div id="ref-neter1989applied">

Neter, John, William Wasserman, and Michael H Kutner. 1989. “Applied
Linear Regression Models.”

</div>

<div id="ref-schielzeth2010simple">

Schielzeth, Holger. 2010. “Simple Means to Improve the Interpretability
of Regression Coefficients.” *Methods in Ecology and Evolution* 1 (2):
103–13.

</div>

</div>
