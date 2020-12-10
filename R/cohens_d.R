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
#'   (For `print()` the result of one of the standardized difference functions.)
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
#'
#' @note The indices here give the population estimated standardized difference.
#'   Some statistical packages give the sample estimate instead (without
#'   applying Bessel's correction).
#'
#' @details
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
#' cohens_d(sleep$extra[sleep$group==1], sleep$extra[sleep$group==2], paired = TRUE)
#'
#' cohens_d(mpg ~ am, data = mtcars)
#' cohens_d(mpg ~ am, data = mtcars, pooled_sd = FALSE)
#' hedges_g(mpg ~ am, data = mtcars)
#' glass_delta(mpg ~ am, data = mtcars)
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
                     paired = FALSE,
                     ci = 0.95,
                     correction) {
  if (!missing(correction)) {
    warning("`correction` argument is deprecated. To apply bias correction, use `hedges_g()`.",
      call. = FALSE, immediate. = TRUE
    )
  }

  .effect_size_difference(
    x,
    y = y,
    data = data,
    type = "d",
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
                     correction = 1,
                     pooled_sd = TRUE,
                     paired = FALSE,
                     ci = 0.95) {
  if (isTRUE(correction) || !correction %in% c(1, 2)) {
    warning("`correction` must be 1 or 2. See ?hedges_g. Setting to 1 for Hedges & Olkin's correction.",
      call. = FALSE, immediate. = TRUE
    )
    correction <- 1
  }

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
glass_delta <- function(x, y = NULL, data = NULL, ci = 0.95, correction) {
  if (!missing(correction)) {
    warning("`correction` argument is deprecated. To apply bias correction, use `hedges_g()`.",
      call. = FALSE, immediate. = TRUE
    )
  }

  .effect_size_difference(
    x,
    y = y,
    data = data,
    type = "delta",
    ci = ci
  )
}



#' @importFrom stats sd na.omit complete.cases
#' @keywords internal
.effect_size_difference <- function(x,
                                    y = NULL,
                                    data = NULL,
                                    type = "d",
                                    correction = NULL,
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
    y <- rep(0, length.out = length(x))
    paired <- TRUE
    pooled_sd <- NULL
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
  } else {
    x <- stats::na.omit(x)
    y <- stats::na.omit(y)

    d <- mean(x) - mean(y)
    n1 <- length(x)
    n2 <- length(y)
    n <- n1 + n2

    if (type == "d" | type == "g") {
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
      hn <- 1 / (n2 - 1)
      s <- stats::sd(y)

      se <- s / sqrt(n2)
      df <- n2 - 1
    }
  }

  out <- data.frame(d = d / s)
  types <- c("d" = "Cohens_d", "g" = "Hedges_g", "delta" = "Glass_delta")
  colnames(out) <- types[type]

  if (is.numeric(ci)) {
    # Add cis
    out$CI <- ci

    t <- d / se
    ts <- .get_ncp_t(t, df, ci)

    out$CI_low <- ts[1] * sqrt(hn)
    out$CI_high <- ts[2] * sqrt(hn)
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
    outcome <- setdiff(all.vars(trms), group)

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
          call. = FALSE
        )
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
