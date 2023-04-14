# library(base)         # 4.2.2
library(tidyverse)      # 2.0.0
library(ggdist)         # 3.2.1.9000
library(distributional) # 0.3.1
library(effectsize)     # 0.8.3.9
# library(see)          # 0.7.4
# library(pwr)          # 1.3.0


# Setup ---------------------------------------------------------------------

fei_to_chisq <- function(fei, N, p0) {
  fei^2 * (N * (1 / min(p0) - 1))
}

## Functions for simulation ------------------------

sim_data <- function(N, p, B = 1) {
  #' @param N sample size
  #' @param p true multinomial dist
  #' @param B number of samples of size N to return
  rmultinom(B, N, p)
}

sim_fei <- function(N, p, p0, B = 1) {
  #' @param N sample size
  #' @param p true multinomial dist
  #' @param p0 Null multinomial dist
  #' @param B number of samples of size N to return
  M <- sim_data(N, p, B)
  apply(M, 2, \(x) fei(x, p = p0, ci = NULL)[[1]])
}

# Test:
fei(c(0.5, 0.5), p = c(0.35, 0.65), ci = NULL)[[1]] # population truth
#> [1] 0.2307692

set.seed(777)
sim_fei(N = 50, p = c(0.5, 0.5), p0 = c(0.35, 0.65), B = 3)
#> [1] 0.2923077 0.2307692 0.2000000

## ggplot --------

theme_set(theme_bw())

lblr <- labeller(
  type = c(
    "binary_uniform" = "Expected:\n0.5 / 0.5",
    "binary" = "\n0.35 / 0.65",
    "quaternary" = "\n0.25 / 0.25 / 0.25 / 0.25"
  ),
  N = c("50" = "N = 50", "100" = "100", "350" = "350"),
  es = c("0.1" = "Effect Size: 0.1", "0.3" = 0.3, "0.5" = 0.5)
)


# Make simulation grid ----------------------------------------------------

# "type" refers to the null/designs in tables 5 and 6 - see "p0" variable below.
# (The true probabilities (p) were found with brute force to make the correct effect sizes)

grid <- expand.grid(
  type = c("binary_uniform", "binary", "quaternary"),
  es = c(0.1, 0.3, 0.5)
) |>
  as_tibble() |>
  arrange(type, es)

# init list column
grid$p <-
  grid$p0 <-
  rep(list(NA), nrow(grid))

# Type: 0.5
grid$p0[1:3] <- list(c(0.5, 0.5)) # null dist
grid$p[1:3] <- c(
  list(c(0.55, 0.45)),
  list(c(0.35, 0.65)),
  list(c(0.25, 0.75))
)

# Type: binary
grid$p0[4:6] <- list(c(0.35, 0.65)) # null dist
grid$p[4:6] <- c(
  list(c(0.415, 0.585)),
  list(c(0.545, 0.455)),
  list(c(0.675, 0.325))
)

# Type: quad
grid$p0[7:9] <- list(rep(0.25, 4))
grid$p[7:9] <- c(
  list(c(0.325, 0.225, 0.225, 0.225)),
  list(c(0.475, 0.175, 0.175, 0.175)),
  list(c(0.625, 0.125, 0.125, 0.125))
)

grid
#> # A tibble: 9 × 4
#>   type              es p0        p
#>   <fct>          <dbl> <list>    <list>
#> 1 binary_uniform   0.1 <dbl [2]> <dbl [2]>
#> 2 binary_uniform   0.3 <dbl [2]> <dbl [2]>
#> 3 binary_uniform   0.5 <dbl [2]> <dbl [2]>
#> 4 binary           0.1 <dbl [2]> <dbl [2]>
#> 5 binary           0.3 <dbl [2]> <dbl [2]>
#> 6 binary           0.5 <dbl [2]> <dbl [2]>
#> 7 quaternary       0.1 <dbl [4]> <dbl [4]>
#> 8 quaternary       0.3 <dbl [4]> <dbl [4]>
#> 9 quaternary       0.5 <dbl [4]> <dbl [4]>


## Validate effect sizes -----------

grid |>
  rowwise() |>
  mutate(
    es_hat = fei(p, p = p0, ci = NULL)[[1]],
    equal = all.equal(es, es_hat)
  ) |>
  select(-p0, -p)
#> # A tibble: 9 × 4
#> # Rowwise:
#>   type              es es_hat equal
#>   <fct>          <dbl>  <dbl> <lgl>
#> 1 binary_uniform   0.1    0.1 TRUE
#> 2 binary_uniform   0.3    0.3 TRUE
#> 3 binary_uniform   0.5    0.5 TRUE
#> 4 binary           0.1    0.1 TRUE
#> 5 binary           0.3    0.3 TRUE
#> 6 binary           0.5    0.5 TRUE
#> 7 quaternary       0.1    0.1 TRUE
#> 8 quaternary       0.3    0.3 TRUE
#> 9 quaternary       0.5    0.5 TRUE


# unnest the data
data_truth <- grid |>
  rowwise() |>
  mutate(
    data = list(data.frame(p, p0, Class = LETTERS[seq_along(p)]))
  ) |>
  select(-p0, -p) |>
  unnest(data) |>
  pivot_longer(c(p, p0), names_to = "Dist", values_to = "p") |>
  mutate(Dist = factor(Dist, levels = c("p0", "p"), labels = c("Null", "Truth")))


ggplot(data_truth, aes(Dist, p, fill = Class)) +
  facet_grid(
    rows = vars(es), cols = vars(type),
    scales = "free", labeller = lblr
  ) +
  geom_col(
    position = position_dodge(0.8), width = 0.8,
    color = "grey60"
  ) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  scale_fill_brewer(type = "qual") +
  labs(y = "Pr(Class)")


# Run simulation --------------------------------------------------------------

nsims <- 500

grid_with_N <- expand_grid(grid, N = c(50, 100, 350))

set.seed(777)
grid_pwr <- grid_with_N |>
  rowwise() |>
  reframe(
    across(c(N, es, type)),
    Fei = sim_fei(N, p, p0, B = nsims)
  )

## Expected distributions ---------------------------------

grid_dists <- grid_with_N |>
  rowwise() |>
  mutate(
    df = length(p0) - 1,
    lambda = fei_to_chisq(es, N, p0),
    ncp_chisq = dist_chisq(df, ncp = lambda), # non-central chisq distribution
    ncp_chi = sqrt(ncp_chisq), # non-central chi distribution
    scaled_ncp_chi = ncp_chi / sqrt((N * (1 / min(p0) - 1))) # re-scaled
  ) |>
  select(type, N, es, scaled_ncp_chi)


## Plot --------------------------------

ggplot(mapping = aes(fill = factor(es))) +
  facet_grid(rows = vars(N), cols = vars(type), labeller = lblr) +
  geom_histogram(aes(x = Fei),
    data = grid_pwr,
    binwidth = 0.025, position = position_identity(),
    alpha = 0.5, color = "grey85"
  ) +
  stat_slab(
    aes(
      xdist = scaled_ncp_chi, color = factor(es),
      # re-scale density to histogram
      thickness = after_stat(pdf * 13)
    ),
    data = grid_dists,
    normalize = "none",
    fill = NA
  ) +
  scale_x_continuous("פ", breaks = c(0.1, 0.3, 0.5)) +
  coord_cartesian(xlim = c(0, 0.7)) +
  see::scale_color_material(aesthetics = c("color", "fill")) +
  guides(color = "none") +
  labs(
    y = NULL,
    fill = "Effect\nSize"
  ) +
  theme_bw(base_size = 12)

ggsave("figure_1.tiff",
  dpi = 600, compression = "lzw",
  width = 13, height = 13, units = "cm",
  scale = 1.4
)

# Power -------------------------------------------------------------------

# Since common power tools support Cohen's w, and Fei is a scaled w,
# All power tools can be used to estimate power for Fei:

p0 <- c(0.35, 0.65)
Fei <- 0.3

pwr::pwr.chisq.test(
  w = fei_to_w(Fei, p = p0),
  df = length(p0) - 1,
  sig.level = 0.01,
  power = 0.85
)
#>
#>      Chi squared power calculation
#>
#>               w = 0.4088311
#>               N = 78.0676
#>              df = 1
#>       sig.level = 0.01
#>           power = 0.85
#>
#> NOTE: N is the number of observations
#>
