#' Effect size for non-parametric (rank sum) tests
#'
#' Compute the rank-biserial correlation (\eqn{r_{rb}}{r_rb}), Cliff's *delta*
#' (\eqn{\delta}), rank epsilon squared (\eqn{\varepsilon^2}{\epsilon^2}), and
#' Kendall's *W* effect sizes for non-parametric (rank sum) tests.
#'
#' @inheritParams cohens_d
#' @param x Can be one of:
#'   - A numeric vector, or a character name of one in `data`.
#'   - A formula in to form of `DV ~ groups` (for `rank_biserial()` and
#'   `rank_epsilon_squared()`) or `DV ~ groups | blocks` (for `kendalls_w()`;
#'   See details for the `blocks` and `groups` terminology used here).
#'   - A list of vectors (for `rank_epsilon_squared()`).
#'   - A matrix of `blocks x groups` (for `kendalls_w()`). See details for the
#'   `blocks` and `groups` terminology used here.
#' @param y An optional numeric vector of data values to compare to `x`, or a
#'   character name of one in `data`. Ignored if `x` is not a vector.
#' @param groups,blocks A factor vector giving the group / block for the
#'   corresponding elements of `x`, or a character name of one in `data`.
#'   Ignored if `x` is not a vector.
#' @param mu a number indicating the value around which (a-)symmetry (for
#'   one-sample or paired samples) or shift (for independent samples) is to be
#'   estimated. See [stats::wilcox.test].
#' @param iterations The number of bootstrap replicates for computing confidence
#'   intervals. Only applies when `ci` is not `NULL`. (Deprecated for
#'   `rank_biserial()`).
#'
#' @details
#' The rank-biserial correlation is appropriate for non-parametric tests of
#' differences - both for the one sample or paired samples case, that would
#' normally be tested with Wilcoxon's Signed Rank Test (giving the
#' **matched-pairs** rank-biserial correlation) and for two independent samples
#' case, that would normally be tested with Mann-Whitney's *U* Test (giving
#' **Glass'** rank-biserial correlation). See [stats::wilcox.test]. In both
#' cases, the correlation represents the difference between the proportion of
#' favorable and unfavorable pairs / signed ranks (Kerby, 2014). Values range
#' from `-1` indicating that all values of the second sample are smaller than
#' the first sample, to `+1` indicating that all values of the second sample are
#' larger than the first sample. (Cliff's *delta* is an alias to the
#' rank-biserial correlation in the two sample case.)
#' \cr\cr
#' The rank epsilon squared is appropriate for non-parametric tests of
#' differences between 2 or more samples (a rank based ANOVA). See
#' [stats::kruskal.test]. Values range from 0 to 1, with larger values
#' indicating larger differences between groups.
#' \cr\cr
#' Kendall's *W* is appropriate for non-parametric tests of differences between
#' 2 or more dependent samples (a rank based rmANOVA), where each `group` (e.g.,
#' experimental condition) was measured for each `block` (e.g., subject). This
#' measure is also common as a measure of reliability of the rankings of the
#' `groups` between raters (`blocks`). See [stats::friedman.test]. Values range
#' from 0 to 1, with larger values indicating larger differences between groups
#' / higher agreement between raters.
#'
#' ## Ties
#' When tied values occur, they are each given the average of the ranks that
#' would have been given had no ties occurred. No other corrections have been
#' implemented yet.
#'
#' # Confidence Intervals
#' Confidence intervals for the rank-biserial correlation (and Cliff's *delta*)
#' are estimated using the normal approximation (via Fisher's transformation).
#' Confidence intervals for rank Epsilon squared, and Kendall's *W* are
#' estimated using the bootstrap method (using the `{boot}` package).
#'
#' @return A data frame with the effect size (`r_rank_biserial`,
#'   `rank_epsilon_squared` or `Kendalls_W`) and its CI (`CI_low` and
#'   `CI_high`).
#'
#' @family effect size indices
#'
#' @examples
#' \donttest{
#' # two-sample tests -----------------------
#'
#' A <- c(48, 48, 77, 86, 85, 85)
#' B <- c(14, 34, 34, 77)
#' rank_biserial(A, B)
#'
#' x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
#' y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
#' rank_biserial(x, y, paired = TRUE)
#'
#' # one-sample tests -----------------------
#' x <- c(1.15, 0.88, 0.90, 0.74, 1.21)
#' rank_biserial(x, mu = 1)
#'
#' # anova tests ----------------------------
#'
#' x1 <- c(2.9, 3.0, 2.5, 2.6, 3.2) # control group
#' x2 <- c(3.8, 2.7, 4.0, 2.4) # obstructive airway disease group
#' x3 <- c(2.8, 3.4, 3.7, 2.2, 2.0) # asbestosis group
#' x <- c(x1, x2, x3)
#' g <- factor(rep(1:3, c(5, 4, 5)))
#' rank_epsilon_squared(x, g)
#'
#' wb <- aggregate(warpbreaks$breaks,
#'   by = list(
#'     w = warpbreaks$wool,
#'     t = warpbreaks$tension
#'   ),
#'   FUN = mean
#' )
#' kendalls_w(x ~ w | t, data = wb)
#' }
#'
#' @references
#' - Cureton, E. E. (1956). Rank-biserial correlation. Psychometrika, 21(3),
#' 287-290.
#'
#' - Glass, G. V. (1965). A ranking variable analogue of biserial correlation:
#' Implications for short-cut item analysis. Journal of Educational Measurement,
#' 2(1), 91-95.
#'
#' - Kendall, M.G. (1948) Rank correlation methods. London: Griffin.
#'
#' - Kerby, D. S. (2014). The simple difference formula: An approach to teaching
#' nonparametric correlation. Comprehensive Psychology, 3, 11-IT.
#'
#' - King, B. M., & Minium, E. W. (2008). Statistical reasoning in the
#' behavioral sciences. John Wiley & Sons Inc.
#'
#' - Cliff, N. (1993). Dominance statistics: Ordinal analyses to answer ordinal
#' questions. Psychological bulletin, 114(3), 494.
#'
#' - Tomczak, M., & Tomczak, E. (2014). The need to report effect size estimates
#' revisited. An overview of some recommended measures of effect size.
#'
#' @export
#' @importFrom stats na.omit complete.cases
rank_biserial <- function(x,
                          y = NULL,
                          data = NULL,
                          mu = 0,
                          ci = 0.95,
                          paired = FALSE,
                          verbose = TRUE,
                          ...,
                          iterations) {
  if (inherits(x, "htest")) {
    if (!grepl("Wilcoxon", x$method)) {
      stop("'x' is not a Wilcoxon-test!", call. = FALSE)
    }
    return(effectsize(x, ci = ci, verbose = verbose))
  }

  if (!missing(iterations) && verbose) {
    warning(
      "'iterations' argument is deprecated. CIs are estimated using a parametric normal approximation.",
      immediate. = TRUE
    )
  }

  ## Prep data
  out <- .deal_with_cohens_d_arguments(x, y, data)
  x <- out$x
  y <- out$y

  if (is.null(y)) {
    y <- rep(0, length.out = length(x))
    paired <- TRUE
  }

  if (paired) {
    oo <- stats::complete.cases(x, y)
    x <- x[oo]
    y <- y[oo]
  } else {
    x <- stats::na.omit(x)
    y <- stats::na.omit(y)
  }

  ## Compute
  r_rbs <- .r_rbs(x, y, mu = mu, paired = paired, verbose = verbose)
  out <- data.frame(r_rank_biserial = r_rbs)

  ## CI
  ci_method <- NULL
  if (is.numeric(ci)) {
    # if (requireNamespace("boot", quietly = TRUE)) {
    #   out <- cbind(out, .rbs_ci_boot(
    #     x,
    #     y,
    #     mu = mu,
    #     paired = paired,
    #     ci = ci,
    #     iterations = iterations
    #   ))
    #
    #   ci_method <- list(method = "bootstrap", iterations = iterations)
    # } else {
    #   ci <- NULL
    #   warning("For CIs, the 'boot' package must be installed.")
    # }

    # Parametric method
    stopifnot(length(ci) == 1, ci < 1, ci > 0)
    alpha <- 1 - ci
    out$CI <- ci
    rf <- atanh(r_rbs)
    if (paired) {
      nd <- sum((x - mu) != 0)
      maxw <- (nd^2 + nd) / 2

      # From: https://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test#Historical_T_statistic
      # wSE <- sqrt((n * (n + 1) * (2 * n + 1)) / 24)
      # Delta method for f(x) = w * 2 / (maxw) - 1
      # r_rbsSE <- wSE * sqrt(4 / (maxw)^2)
      # Delta method for z: z_rbsSE <- r_rbsSE / (1 - r_rbs^2)
      #   But simulations suggest that z_rbsSE is positively biased
      #   more than r_rbsSE is negatively biased, especially when r_rbs is large,
      #   so we use r_rbsSE instead
      rfSE <- sqrt((2 * nd^3 + 3 * nd^2 + nd) / 6) / maxw
    } else {
      n1 <- length(x)
      n2 <- length(y)

      # From: https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test#Normal_approximation_and_tie_correction
      # wSE <- sqrt((n1 * n2 * (n1 + n2 + 1)) / 12)
      # Delta method for f(x) = 1 - 2 * w / (n1 * n2) * sign(diff)
      # r_rbsSE <- wSE * sqrt(4 / (n1 * n2)^2)
      # Delta method for z: z_rbsSE <- r_rbsSE / (1 - r_rbs^2)
      #   But simulations suggest that z_rbsSE is positively biased
      #   more than r_rbsSE is negatively biased, especially when r_rbs is large,
      #   so we use r_rbsSE instead
      rfSE <- sqrt((n1 + n2 + 1) / (3 * n1 * n2))
    }

    confint <- tanh(rf + c(-1, 1) * qnorm(1 - alpha / 2) * rfSE)
    out$CI_low <- confint[1]
    out$CI_high <- confint[2]
    ci_method <- list(method = "normal")
  }

  class(out) <- c("effectsize_difference", "effectsize_table", "see_effectsize_table", class(out))
  attr(out, "paired") <- paired
  attr(out, "mu") <- mu
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "approximate") <- FALSE
  return(out)
}

#' @export
#' @rdname rank_biserial
cliffs_delta <- function(x,
                         y = NULL,
                         data = NULL,
                         mu = 0,
                         ci = 0.95,
                         iterations = 200,
                         verbose = TRUE,
                         ...) {
  rank_biserial(
    x, y,
    data = data,
    mu = mu,
    paired = FALSE,
    ci = ci,
    iterations = iterations,
    verbose = verbose
  )
}


#' @rdname rank_biserial
#' @export
#' @importFrom stats na.omit
#' @importFrom insight check_if_installed
rank_epsilon_squared <- function(x,
                                 groups,
                                 data = NULL,
                                 ci = 0.95,
                                 iterations = 200,
                                 ...) {
  if (inherits(x, "htest")) {
    if (!grepl("Kruskal-Wallis", x$method)) {
      stop("'x' is not a Kruskal-Wallis-test!", call. = FALSE)
    }
    return(effectsize(x, ci = ci, iterations = iterations))
  }

  ## pep data
  data <- .rank_anova_xg(x, groups, data)
  data <- stats::na.omit(data)
  x <- data$x
  groups <- data$groups

  ## compute
  E <- .repsilon(x, groups)
  out <- data.frame(rank_epsilon_squared = E)

  ## CI
  ci_method <- NULL
  if (is.numeric(ci)) {
    if (insight::check_if_installed("boot", "for estimating CIs", stop = FALSE)) {
      out <- cbind(out, .repsilon_ci(data, ci, iterations))
      ci_method <- list(method = "percentile bootstrap", iterations = iterations)
    } else {
      ci <- NULL
    }
  }

  class(out) <- c("effectsize_table", "see_effectsize_table", class(out))
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "approximate") <- FALSE
  return(out)
}

#' @rdname rank_biserial
#' @export
#' @importFrom stats na.omit
#' @importFrom insight check_if_installed
kendalls_w <- function(x,
                       groups,
                       blocks,
                       data = NULL,
                       ci = 0.95,
                       iterations = 200,
                       verbose = TRUE,
                       ...) {
  if (inherits(x, "htest")) {
    if (!grepl("Friedman", x$method)) {
      stop("'x' is not a Friedman-test!", call. = FALSE)
    }
    return(effectsize(x, ci = ci, iterations = iterations, verbose = verbose))
  }

  ## prep data
  data <- .kendalls_w_data(x, groups, blocks, data)
  data <- stats::na.omit(data)
  rankings <- apply(data, 1, ranktransform, verbose = verbose)
  rankings <- t(rankings) # keep dims

  ## compute
  W <- .kendalls_w(rankings)
  out <- data.frame(Kendalls_W = W)

  ## CI
  ci_method <- NULL
  if (is.numeric(ci)) {
    if (insight::check_if_installed("boot", "for estimating CIs", stop = FALSE)) {
      out <- cbind(out, .kendalls_w_ci(rankings, ci, iterations))
      ci_method <- list(method = "percentile bootstrap", iterations = iterations)
    } else {
      ci <- NULL
    }
  }

  class(out) <- c("effectsize_table", "see_effectsize_table", class(out))
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "approximate") <- FALSE
  return(out)
}

# rank_eta_squared <- function(x, g, data = NULL, ci = 0.95, iterations = 200) {
#
#   data <- .rank_anova_xg(x, g, data)
#   data <- stats::na.omit(data)
#   x <- data$x
#   g <- data$g
#
#   model <- stats::kruskal.test(x, g)
#
#   H <- unname(model$statistic)
#   k <- length(unique(g)) # model$parameter + 1
#   n <- length(g)
#
#   E <- (H - k + 1) / (n - k)
#
#   out <- data.frame(rank_eta_squared = E)
#
#   if (is.numeric(ci)) {
#     warning("Nope. Not yet.")
#     out$CI <- ci
#     out$CI_low <- 0
#     out$CI_high <- 1
#   }
#
#   class(out) <- c("effectsize_table", class(out))
#   return(out)
# }


# Utils -------------------------------------------------------------------

## Get ----

#' @keywords internal
#' @importFrom stats na.omit
.r_rbs <- function(x, y, mu, paired, verbose = FALSE) {
  if (paired) {
    Ry <- ranktransform((x - y) - mu, sign = TRUE, verbose = verbose)
    Ry <- stats::na.omit(Ry)

    n <- length(Ry)
    S <- (n * (n + 1) / 2)

    U1 <- sum(Ry[Ry > 0], na.rm = TRUE)
    U2 <- -sum(Ry[Ry < 0], na.rm = TRUE)
  } else {
    Ry <- ranktransform(c(x - mu, y), verbose = verbose)

    n1 <- length(x)
    n2 <- length(y)
    S <- (n1 * n2)

    U1 <- sum(Ry[seq_along(x)]) - n1 * (n1 + 1) / 2
    U2 <- sum(Ry[-seq_along(x)]) - n2 * (n2 + 1) / 2
  }

  u_ <- U1 / S
  f_ <- U2 / S
  return(u_ - f_)
}

#' @keywords internal
#' @importFrom stats kruskal.test
.repsilon <- function(x, groups) {
  model <- stats::kruskal.test(x, groups)

  H <- unname(model$statistic)
  n <- length(groups)

  E <- H / ((n^2 - 1) / (n + 1))
}


#' @keywords internal
.kendalls_w <- function(rankings) {
  # TODO add ties correction?
  n <- ncol(rankings) # items
  m <- nrow(rankings) # judges

  R <- colSums(rankings)
  S <- var(R) * (n - 1)
  W <- (12 * S) / (m^2 * (n^3 - n))
}

## CI ----

# #' @keywords internal
# #' @importFrom bayestestR ci
# .rbs_ci_boot <- function(x,
#                          y,
#                          mu = 0,
#                          paired = FALSE,
#                          ci = 0.95,
#                          iterations = 200) {
#   stopifnot(length(ci) == 1, ci < 1, ci > 0)
#
#   if (paired) {
#     data <- data.frame(x, y)
#     boot_rbs <- function(.data, .i) {
#       .data <- .data[.i, ]
#       .x <- .data$x
#       .y <- .data$y
#       .r_rbs(.x, .y, mu = mu, paired = TRUE, verbose = FALSE)
#     }
#   } else {
#     data <- data.frame(
#       i = seq_along(c(x, y))
#     )
#
#     boot_rbs <- function(.data, .i) {
#       .x <- sample(x, replace = TRUE)
#       .y <- sample(y, replace = TRUE)
#
#       .r_rbs(.x, .y, mu = mu, paired = FALSE, verbose = FALSE)
#     }
#   }
#
#   R <- boot::boot(
#     data = data,
#     statistic = boot_rbs,
#     R = iterations
#   )
#
#   out <- as.data.frame(
#     bayestestR::ci(na.omit(R$t), ci = ci, verbose = FALSE)
#   )
#   out$CI <- ci
#   out
# }

#' @keywords internal
.repsilon_ci <- function(data, ci, iterations) {
  stopifnot(length(ci) == 1, ci < 1, ci > 0)

  boot_r_epsilon <- function(.data, .i) {
    split(.data$x, .data$groups) <- lapply(split(.data$x, .data$groups),
      sample,
      replace = TRUE
    )
    .repsilon(.data$x, .data$groups)
  }

  R <- boot::boot(
    data = data,
    statistic = boot_r_epsilon,
    R = iterations
  )

  bCI <- boot::boot.ci(R, conf = ci, type = "perc")$percent
  bCI <- tail(as.vector(bCI), 2)

  data.frame(
    CI = ci,
    CI_low = bCI[1],
    CI_high = bCI[2]
  )
}

#' @keywords internal
.kendalls_w_ci <- function(data, ci, iterations) {
  stopifnot(length(ci) == 1, ci < 1, ci > 0)

  boot_w <- function(.data, .i) {
    .kendalls_w(.data[.i, ]) # sample rows
  }

  R <- boot::boot(
    data = data,
    statistic = boot_w,
    R = iterations
  )

  bCI <- boot::boot.ci(R, conf = ci, type = "perc")$percent
  bCI <- tail(as.vector(bCI), 2)

  data.frame(
    CI = ci,
    CI_low = bCI[1],
    CI_high = bCI[2]
  )
}

## data ----


#' @keywords internal
#' @importFrom stats model.frame
.rank_anova_xg <- function(x, groups, data) {
  if (inherits(frm <- x, "formula")) {
    mf <- stats::model.frame(formula = frm, data = data)

    if (length(frm) != 3 | ncol(mf) != 2) {
      stop("Formula must have the form of 'outcome ~ group'.", call. = FALSE)
    }

    x <- mf[[1]]
    groups <- factor(mf[[2]])
  } else if (inherits(x, "list")) {
    groups <- rep(seq_along(x), sapply(x, length))
    x <- unsplit(x, groups)
  } else if (is.character(x)) {
    x <- data[[x]]
    groups <- data[[groups]]
  } else if (length(x) != length(groups)) {
    stop("x and g must be of the same length.", call. = FALSE)
  }

  data.frame(x, groups)
}

#' @keywords internal
#' @importFrom stats model.frame reshape
.kendalls_w_data <- function(x, groups, blocks, data = NULL) {
  if (inherits(frm <- x, "formula")) {
    if ((length(frm) != 3L) ||
      (length(frm[[3L]]) != 3L) ||
      (frm[[3L]][[1L]] != as.name("|"))) {
      stop("Formula must have the 'x ~ groups | blocks'.", call. = FALSE)
    }

    frm[[3L]][[1L]] <- as.name("+")

    mf <- stats::model.frame(formula = frm, data = data)

    if (ncol(mf) != 3) {
      stop("Formula must have only two terms on the RHS.", call. = FALSE)
    }

    x <- mf[[1]]
    groups <- mf[[2]]
    blocks <- mf[[3]]
  } else if (inherits(x, c("table", "matrix", "array", "data.frame"))) {
    data <- data.frame(
      x = c(x),
      groups = rep(factor(seq_len(ncol(x))),
        each = nrow(x)
      ),
      blocks = rep(
        factor(seq_len(nrow(x))),
        ncol(x)
      )
    )

    x <- data[[1]]
    groups <- data[[2]]
    blocks <- data[[3]]
  } else if (is.character(x)) {
    x <- data[[x]]
    groups <- data[[groups]]
    blocks <- data[[blocks]]
  } else if (length(x) != length(groups) || length(x) != length(blocks)) {
    stop("x, groups and blocks must be of the same length.", call. = FALSE)
  }

  data <- data.frame(x, groups, blocks, stringsAsFactors = FALSE)

  data <- stats::reshape(
    data,
    direction = "wide",
    v.names = "x",
    timevar = "groups",
    idvar = "blocks"
  )

  as.matrix(data[, -1])
}
