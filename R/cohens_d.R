#' Effect size for differences
#'
#' Compute effect size indices for standardized differences: Cohen's *d*,
#' Hedges' *g* and Glass’s *delta*. (This function returns the **population**
#' estimate.)
#' \cr\cr
#' Both Cohen's *d* and Hedges' *g* are the estimated the standardized
#' difference between the means of two populations. Hedges' *g* provides a bias
#' correction to Cohen's *d* for small sample sizes. For sample sizes > 20, the
#' results for both statistics are roughly equivalent. Glass’s *delta* is
#' appropriate when the standard deviations are significantly different between
#' the populations, as it uses only the *second* group's standard deviation.
#'
#' @param x A formula, a numeric vector, or a character name of one in `data`.
#' @param y A numeric vector, a grouping (character / factor) vector, a or a
#'   character  name of one in `data`. Ignored if `x` is a formula.
#' @param data An optional data frame containing the variables.
#' @param correction Type of small sample bias correction to apply to produce
#'   Hedges' *g*. Can be `1` for Hedges and Olkin's original correction
#'   (default) or `2` for Hunter and Schmidt's correction (see McGrath & Meyer,
#'   2006).
#' @param pooled_sd If `TRUE` (default), a [sd_pooled()] is used (assuming equal
#'   variance). Else the mean SD from both groups is used instead.
#' @param paired If `TRUE`, the values of `x` and `y` are considered as paired.
#'   This produces an effect size that is equivalent to the one-sample effect
#'   size on `x - y`.
#' @inheritParams chisq_to_phi
#' @inheritParams eta_squared
#' @inheritParams stats::t.test
#'
#' @note The indices here give the population estimated standardized difference.
#'   Some statistical packages give the sample estimate instead (without
#'   applying Bessel's correction).
#'
#' @details
#'
#' ## Confidence Intervals for Glass' *delta*
#' Confidence Intervals for Glass' *delta* are estimated using the bootstrap
#' method.
#'
#' @inheritSection effectsize-CIs Confidence Intervals
#'
#' @return A data frame with the effect size ( `Cohens_d`, `Hedges_g`,
#'   `Glass_delta`) and their CIs (`CI_low` and `CI_high`).
#'
#' @seealso [d_to_common_language()] [sd_pooled()]
#' @family effect size indices
#'
#' @examples
#' cohens_d(sleep$extra, sleep$group)
#' hedges_g("extra", "group", data = sleep)
#'
#' cohens_d(sleep$extra[sleep$group == 1], sleep$extra[sleep$group == 2], paired = TRUE)
#'
#' cohens_d(mpg ~ am, data = mtcars)
#' cohens_d(mpg ~ am, data = mtcars, pooled_sd = FALSE)
#' cohens_d(mpg ~ am, data = mtcars, mu = -5)
#' hedges_g(mpg ~ am, data = mtcars)
#' if (require(boot)) glass_delta(mpg ~ am, data = mtcars)
#'
#' print(cohens_d(mpg ~ am, data = mtcars), append_CL = TRUE)
#' @references
#' - Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd Ed.). New York: Routledge.
#' - Hedges, L. V. & Olkin, I. (1985). Statistical methods for meta-analysis. Orlando, FL: Academic Press.
#' - Hunter, J. E., & Schmidt, F. L. (2004). Methods of meta-analysis: Correcting error and bias in research findings. Sage.
#' - McGrath, R. E., & Meyer, G. J. (2006). When effect sizes disagree: the case of r and d. Psychological methods, 11(4), 386.
#'
#' @importFrom stats var model.frame
#' @export
cohens_d <- function(x,
                     y = NULL,
                     data = NULL,
                     pooled_sd = TRUE,
                     mu = 0,
                     paired = FALSE,
                     ci = 0.95,
                     verbose = TRUE,
                     ...,
                     correction) {
  if (!missing(correction)) {
    warning("`correction` argument is deprecated. To apply bias correction, use `hedges_g()`.",
      call. = FALSE, immediate. = TRUE
    )
  }

  if (inherits(x, "htest")) {
    if (!grepl("t-test", x$method)) {
      stop("'x' is not a t-test!", call. = FALSE)
    }
    return(effectsize(x, type = "d", correction = correction, ci = ci, verbose = verbose))
  }


  .effect_size_difference(
    x,
    y = y,
    data = data,
    type = "d",
    pooled_sd = pooled_sd,
    mu = mu,
    paired = paired,
    ci = ci,
    verbose = verbose
  )
}

#' @rdname cohens_d
#' @param iterations The number of bootstrap replicates for computing confidence intervals. Only applies when \code{ci} is not \code{NULL}.
#' @export
hedges_g <- function(x,
                     y = NULL,
                     data = NULL,
                     correction = 1,
                     pooled_sd = TRUE,
                     mu = 0,
                     paired = FALSE,
                     ci = 0.95,
                     verbose = TRUE,
                     ...) {
  if (isTRUE(correction) || !correction %in% c(1, 2)) {
    warning("`correction` must be 1 or 2. See ?hedges_g. Setting to 1 for Hedges & Olkin's correction.",
      call. = FALSE, immediate. = TRUE
    )
    correction <- 1
  }

  if (inherits(x, "htest")) {
    if (!grepl("t-test", x$method)) {
      stop("'x' is not a t-test!", call. = FALSE)
    }
    return(effectsize(x, type = "g", correction = correction, ci = ci, verbose = verbose))
  }

  .effect_size_difference(
    x,
    y = y,
    data = data,
    type = "g",
    correction = correction,
    pooled_sd = pooled_sd,
    mu = mu,
    paired = paired,
    ci = ci,
    verbose = verbose
  )
}

#' @rdname cohens_d
#' @export
glass_delta <- function(x, y = NULL, data = NULL, mu = 0, ci = 0.95, iterations = 200, verbose = TRUE, ..., correction) {
  if (!missing(correction)) {
    warning("`correction` argument is deprecated. To apply bias correction, use `hedges_g()`.",
      call. = FALSE, immediate. = TRUE
    )
  }

  .effect_size_difference(
    x,
    y = y,
    data = data,
    mu = mu,
    type = "delta",
    ci = ci,
    verbose = verbose,
    iterations = iterations
  )
}



#' @importFrom stats sd na.omit complete.cases
#' @keywords internal
.effect_size_difference <- function(x,
                                    y = NULL,
                                    data = NULL,
                                    type = "d",
                                    mu = 0,
                                    correction = NULL,
                                    pooled_sd = TRUE,
                                    paired = FALSE,
                                    ci = 0.95,
                                    verbose = TRUE,
                                    iterations = NULL,
                                    ...) {
  out <- .deal_with_cohens_d_arguments(x, y, data, verbose)
  x <- out$x
  y <- out$y

  if (is.null(y)) {
    if (type == "delta") {
      stop("For Glass' Delta, please provide data from two samples.", call. = FALSE)
    }
    y <- rep(0, length.out = length(x))
    paired <- TRUE
  }

  # Compute index
  if (paired) {
    o <- stats::complete.cases(x, y)
    x <- x[o]
    y <- y[o]

    d <- mean(x - y)
    n <- length(x)
    s <- stats::sd(x - y)

    hn <- 1 / (n - 1)
    se <- s / sqrt(n)
    df <- n - 1

    pooled_sd <- NULL
  } else {
    x <- stats::na.omit(x)
    y <- stats::na.omit(y)

    d <- mean(x) - mean(y)
    n1 <- length(x)
    n2 <- length(y)
    n <- n1 + n2

    if (type %in% c("d", "g")) {
      hn <- (1 / n1 + 1 / n2)
      if (pooled_sd) {
        s <- suppressWarnings(sd_pooled(x, y))

        se <- s * sqrt(1 / n1 + 1 / n2)
        df <- n - 2
      } else {
        s1 <- stats::sd(x)
        s2 <- stats::sd(y)
        s <- sqrt((s1^2 + s2^2) / 2)

        se1 <- sqrt(s1^2 / n1)
        se2 <- sqrt(s2^2 / n2)
        se <- sqrt(se1^2 + se2^2)
        df <- se^4 / (se1^4 / (n1 - 1) + se2^4 / (n2 - 1))
      }
    } else if (type == "delta") {
      pooled_sd <- NULL
      s <- stats::sd(y)
    }
  }

  out <- data.frame(d = (d - mu) / s)
  types <- c("d" = "Cohens_d", "g" = "Hedges_g", "delta" = "Glass_delta")
  colnames(out) <- types[type]

  ci_method <- NULL
  if (is.numeric(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)

    # Add cis
    if (type %in% c("d", "g")) {
      out$CI <- ci

      t <- (d - mu) / se
      ts <- .get_ncp_t(t, df, ci)

      out$CI_low <- ts[1] * sqrt(hn)
      out$CI_high <- ts[2] * sqrt(hn)
      ci_method <- list(method = "ncp", distribution = "t")
    } else if (type == "delta") {
      if (requireNamespace("boot", quietly = TRUE)) {
        out <- cbind(out, .delta_ci(x, y, mu = mu, ci = ci, ...))
        ci_method <- list(method = "bootstrap", iterations = iterations)
      } else {
        ci <- NULL
        warning("'boot' package required for estimating CIs for Glass' delta. Please install the package and try again.", call. = FALSE)
      }
    }
  }


  if (type == "g") {
    if (correction == 1) {
      if (paired) {
        J <- 1 - 3 / (4 * (n - 1) - 1)
      } else {
        J <- 1 - 3 / (4 * n - 9)
      }
    } else if (correction == 2) {
      # McGrath & Meyer (2006)
      J <- ((n - 3) / (n - 2.25)) * sqrt((n - 2) / n)
    }

    out[, colnames(out) %in% c("Hedges_g", "CI_low", "CI_high")] <-
      out[, colnames(out) %in% c("Hedges_g", "CI_low", "CI_high")] * J
  }

  class(out) <- c("effectsize_difference", "effectsize_table", "see_effectsize_table", class(out))
  attr(out, "paired") <- paired
  attr(out, "correction") <- correction
  attr(out, "pooled_sd") <- pooled_sd
  attr(out, "mu") <- mu
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  return(out)
}






# Utils -------------------------------------------------------------------



#' @keywords internal
#' @importFrom stats terms
#' @importFrom stats delete.response
.deal_with_cohens_d_arguments <- function(x, y = NULL, data = NULL, verbose = TRUE) {

  # Sanity checks
  if (inherits(x, "formula") | is.character(x) | is.character(y)) {
    if (is.null(data)) { # && !is.data.frame(data <- y) ?
      stop("Please provide data argument.")
    }
  }


  ## Preprocess data

  # Formula
  if (inherits(x, "formula")) {
    if (length(x) != 3) {
      stop("Formula must have the 'outcome ~ group'.", call. = FALSE)
    }

    mf <- stats::model.frame(stats::lm(formula = x, data = data))

    x <- mf[[1]]
    if (ncol(mf) == 1) {
      y <- NULL
    } else if (ncol(mf) == 2) {
      y <- mf[[2]]
    } else {
      stop("Formula must have the 'outcome ~ group'.", call. = FALSE)
    }

    if (!is.null(y) && !is.factor(y)) y <- factor(y)
  }

  if (is.character(x)) {
    if (is.null(x <- data[[xn <- x]])) {
      stop("Column ", xn, " missing from data.", call. = FALSE)
    }
  }

  if (is.character(y)) {
    if (is.null(y <- data[[yn <- y]])) {
      stop("Column ", yn, " missing from data.", call. = FALSE)
    }
  }

  if (!is.numeric(x)) {
    stop("Cannot compute effect size for a non-numeric vector.", call. = FALSE)
  }

  # If y is a factor
  if (!is.null(y)) {
    if (!is.numeric(y)) {
      if (length(unique(y)) > 2) {
        stop("Cannot compute the difference as a factor with more than 2 levels has been provided.",
          call. = FALSE
        )
      }
      if (length(x) != length(y)) {
        stop("Grouping variable must be the same length.", call. = FALSE)
      }

      data <- split(x, y)
      x <- data[[1]]
      y <- data[[2]]
    } else if (verbose && length(unique(y)) == 2) {
      warning(
        "'y' is numeric but has only 2 unique values. If this is a grouping variable, convert it to a factor.",
        call. = FALSE
      )
    }
  }

  list(x = x, y = y)
}

.delta_ci <- function(x, y, mu = 0, ci = 0.95, iterations = 200) {
  boot_delta <- function(data, .i, mu = 0) {
    .x <- sample(x, replace = TRUE)
    .y <- sample(y, replace = TRUE)

    d <- mean(.x) - mean(.y)
    s <- stats::sd(.y)
    (d - mu) / s
  }

  # dud, not actually used
  data <- data.frame(
    i = seq_along(c(x, y))
  )

  R <- boot::boot(
    data = data,
    statistic = boot_delta,
    R = iterations,
    mu = mu
  )

  out <- as.data.frame(
    bayestestR::ci(na.omit(R$t), ci = ci, verbose = FALSE)
  )
  out$CI <- ci
  out
}
