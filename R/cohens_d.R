#' Effect size for differences
#'
#' Compute different indices of effect size. For very small sample sizes (n < 20)
#' Hedges' g is considered as less biased than Cohen's d.
#' For sample sizes > 20, the results for both statistics are roughly equivalent.
#' \cr\cr
#'  The Glass’s delta is appropriate if standard deviations are significantly different
#'  between groups, as it uses only the *second* group's standard deviation.
#'
#' @param x A formula, a numeric vector, or a character name of one in `data`.
#' @param y A numeric vector, a grouping (character / factor) vector, a or a character  name of one in `data`. Ignored if `x` is a formula.
#' @param data An optional data frame containing the variables.
#' @param correction If `TRUE`, applies a correction to make it less biased for small samples (McGrath & Meyer, 2006).
#' @param pooled_sd If `TRUE` (default), a \code{\link{sd_pooled}} is used (assuming equal variance). Else the mean SD from both groups is used instead.
#' @param paired If `TRUE`, the values of `x` and `y` are considered as paired.
#' @inheritParams chisq_to_phi
#'
#' @return A data frame with the effect size(s) and confidence interval(s).
#'
#' \subsection{Confidence Intervals}{
#' Confidence intervals are estimated using the Noncentrality parameter method;
#' These methods searches for a the best `ncp` (non-central parameters) for
#' of the noncentral t distribution for the desired tail-probabilities,
#' and then convert these `ncp`s to the corresponding effect sizes.
#' }
#'
#' @examples
#' cohens_d(iris$Sepal.Length, iris$Sepal.Width)
#' hedges_g("Sepal.Length", "Sepal.Width", data = iris)
#'
#' cohens_d(mpg ~ am, data = mtcars)
#' cohens_d(mpg ~ am, data = mtcars, pooled_sd = FALSE)
#' hedges_g(mpg ~ am, data = mtcars)
#' glass_delta(mpg ~ am, data = mtcars)
#'
#' @references \itemize{
#'  \item Cohen, J. (2013). Statistical power analysis for the behavioral sciences. Routledge.
#'  \item McGrath, R. E., & Meyer, G. J. (2006). When effect sizes disagree: the case of r and d. Psychological methods, 11(4), 386.
#'  \item Hedges, L. V. & Olkin, I. (1985). Statistical methods for meta-analysis. Orlando, FL: Academic Press.
#' }
#' @importFrom stats var model.frame
#' @export
cohens_d <- function(x,
                     y = NULL,
                     data = NULL,
                     correction = FALSE,
                     pooled_sd = TRUE,
                     paired = FALSE,
                     ci = 0.95) {
  .effect_size_difference(
    x,
    y = y,
    data = data,
    type = "d",
    correction = correction,
    pooled_sd = pooled_sd,
    paired = paired,
    ci = ci
  )
}

#' @rdname cohens_d
#' @export
hedges_g <- function(x,
                     y = NULL,
                     data = NULL,
                     correction = FALSE,
                     pooled_sd = TRUE,
                     paired = FALSE,
                     ci = 0.95) {
  .effect_size_difference(
    x,
    y = y,
    data = data,
    type = "g",
    correction = correction,
    pooled_sd = pooled_sd,
    paired = paired,
    ci = ci
  )
}

#' @rdname cohens_d
#' @export
glass_delta <- function(x, y = NULL, data = NULL, correction = FALSE, ci = 0.95) {
  .effect_size_difference(
    x,
    y = y,
    data = data,
    type = "delta",
    correction = correction,
    ci = ci
  )
}



#' @importFrom stats sd na.omit
#' @keywords internal
.effect_size_difference <- function(x,
                                    y = NULL,
                                    data = NULL,
                                    type = "d",
                                    correction = FALSE,
                                    pooled_sd = TRUE,
                                    paired = FALSE,
                                    ci = 0.95) {
  out <- .deal_with_cohens_d_arguments(x, y, data)
  x <- out$x
  y <- out$y

  if (is.null(y)) {
    if (type == "delta") {
      stop("For Glass' Delta, please provide data from two samples.", call. = FALSE)
    }
    y <- 0
    paired <- TRUE
  }

  # Compute index
  if (paired) {
    d <- mean(x - y, na.rm = TRUE)
    s <- stats::sd(x - y, na.rm = TRUE)
    n <- length(stats::na.omit(x - y))
    df <- n - 1
    hn <- 1 / df
    t <- d / (s / sqrt(n))
  } else {
    d <- mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)
    n1 <- length(stats::na.omit(x))
    n2 <- length(stats::na.omit(y))
    n <- n1 + n2
    df <- n - 2
    hn <- (1 / n1 + 1 / n2)
    if (type == "d" | type == "g") {
      if (pooled_sd) {
        s <- suppressWarnings(sd_pooled(x, y))
        t <- d / (s * sqrt(1 / n1 + 1 / n2))
      } else {
        s1 <- stats::sd(x, na.rm = TRUE)
        s2 <- stats::sd(y, na.rm = TRUE)
        s <- sqrt((s1 ^ 2 + s2 ^ 2) / 2)
        t <- d / sqrt(s1 ^ 2 / n1 + s2 ^ 2 / n2)
      }
    } else if (type == "delta") {
      s <- stats::sd(y, na.rm = TRUE)
      t <- d / (s * sqrt(1 / n1 + 1 / n2))
    }
  }

  out <- data.frame(d = d / s)
  types <- c("d" = "Cohens_d", "g" = "Hedges_g", "delta" = "Glass_delta")
  colnames(out) <- types[type]

  if (is.numeric(ci)) {
    # Add cis
    out$CI <- ci

    ts <- .get_ncp_t(t, df, ci)

    # paired <- 2 - paired
    #
    # out$CI_low <- paired * ts[1] / sqrt(hn)
    # out$CI_high <- paired * ts[2] / sqrt(hn)

    out$CI_low <- ts[1] * sqrt(hn)
    out$CI_high <- ts[2] * sqrt(hn)
  }


  if (type == "g") {
    if (paired) {
      J <- 1 - 3 / (4 * (n - 1) - 1)
    } else {
      J <- 1 - 3 / (4 * n - 9)
    }

    out[, colnames(out) %in% c("Hedges_g", "CI_low", "CI_high")] <-
      out[, colnames(out) %in% c("Hedges_g", "CI_low", "CI_high")] * J
  }

  # McGrath & Meyer (2006)
  if (correction) {
    correction <- ((n - 3) / (n - 2.25)) * sqrt((n - 2) / n)

    out[, colnames(out) %in% c(types, "CI_low", "CI_high")] <-
      out[, colnames(out) %in% c(types, "CI_low", "CI_high")] * correction
  }

  class(out) <- c("effectsize_table","see_effectsize_table", class(out))
  return(out)
}






# Utils -------------------------------------------------------------------



#' @keywords internal
#' @importFrom stats terms
#' @importFrom stats delete.response
.deal_with_cohens_d_arguments <- function(x, y = NULL, data = NULL) {

  # Sanity checks
  if (inherits(x, "formula") | is.character(x) | is.character(y)) {
    if (is.null(data)) {
      stop("Please provide data argument.")
    }
  }

  ## Preprocess data

  # Formula
  if (inherits(x, "formula")) {
    trms <- stats::terms(x)

    group <- all.vars(stats::delete.response(trms))
    outcome <- setdiff(all.vars(trms),group)

    if (!(length(outcome) == 1 & length(group) == 1)) {
      stop("Formula must have the 'outcome ~ group'.", call. = FALSE)
    }

    x <- data[[outcome]]
    y <- as.factor(data[[group]])
  }


  if (is.character(x)) {
    x <- data[[x]]
  }

  if (is.character(y)) {
    y <- data[[y]]
  }

  if (!is.numeric(x)) {
    stop("Cannot compute effect size for a non-numeric vector.", call. = FALSE)
  }

  # If y is a factor
  if (!is.null(y)) {
    if (!is.numeric(y)) {
      if (length(unique(y)) > 2) {
        stop("Cannot compute the difference as a factor with more than 2 levels has been provided.",
             call. = FALSE)
      }
      if (length(x) != length(y)) {
        stop("Grouping variable must be the same length.", call. = FALSE)
      }

      data <- split(x, y)
      x <- data[[1]]
      y <- data[[2]]
    } else if (length(unique(y)) == 2) {
      warning(
        "'y' is numeric but has only 2 unique values. If this is a grouping variable, convert it to a factor.",
        call. = FALSE
      )
    }
  }

  list(x = x, y = y)
}
