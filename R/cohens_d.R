#' Effect size for differences
#'
#' Compute different indices of effect size. For very small sample sizes (n < 20) Hedges' g is considered as less biased than Cohen's d. For sample sizes > 20, the results for both statistics are roughly equivalent. The Glass’s delta is appropriate if standard deviations are significantly different between groups, as it uses only the control group's (\code{x}) standard deviation.
#'
#' @param x A formula, a numeric vector, or a name of one in \code{data}.
#' @param y A numeric vector, a grouping (character / factor) vector, a or a name of one in \code{data}. Ignored if \code{x} is a formula.
#' @param data An optional data frame containing the variables.
#' @param correction If \code{TRUE}, applies a correction to make it less biased for small samples (McGrath & Meyer, 2006).
#' @param pooled_sd If \code{FALSE}, the regular SD from both combined groups is used instead of the \code{\link{sd_pooled}}.
#' @param paired If \code{TRUE}, the values of \code{x} and \code{y} are considered as paired.
#' @inheritParams chisq_to_phi
#'
#' @return A data frame with the effect size(s) and confidence interval(s).
#'
#' \subsection{Confidence Intervals}{
#' Confidence intervals are estimated using the Noncentrality parameter method;
#' These methods searches for a the best \code{ncp} (non-central parameters) for
#' of the noncentral F distribution for the desired tail-probabilities,
#' and then convert these \code{ncp}s to the corresponding effect sizes.
#' }
#'
#' @examples
#' cohens_d(iris$Sepal.Length, iris$Sepal.Width)
#' hedges_g("Sepal.Length", "Sepal.Width", data = iris)
#' glass_delta(Sepal.Length ~ Sepal.Width, data = iris)
#'
#' cohens_d(iris$Sepal.Length, iris$Sepal.Width, correct = TRUE, pooled_sd = FALSE)
#' cohens_d(Sepal.Length ~ Species, data = iris[iris$Species %in% c("versicolor", "setosa"), ])
#' @references \itemize{
#'  \item Cohen, J. (2013). Statistical power analysis for the behavioral sciences. Routledge.
#'  \item McGrath, R. E., & Meyer, G. J. (2006). When effect sizes disagree: the case of r and d. Psychological methods, 11(4), 386.
#'  \item Hedges, L. V. & Olkin, I. (1985). Statistical methods for meta-analysis. Orlando, FL: Academic Press.
#' }
#' @importFrom stats var model.frame
#' @export
cohens_d <- function(x, y = NULL, data = NULL, correction = FALSE, pooled_sd = TRUE, paired = FALSE, ci = 0.95) {
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
hedges_g <- function(x, y = NULL, data = NULL, correction = FALSE, pooled_sd = TRUE, paired = FALSE, ci = 0.95) {
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



#' @importFrom stats sd
#' @keywords internal
.effect_size_difference <- function(x, y = NULL, data = NULL, type = "d", correction = FALSE, pooled_sd = TRUE, paired = FALSE, ci = 0.95) {
  out <- .deal_with_cohens_d_arguments(x, y, data)
  x <- out$x
  y <- out$y

  if (paired) {
    n <- length(x)
    df <- n - 1
  } else {
    n <- length(c(x,y))
    df <- n - 2
  }

  # Compute index
  diff_of_means <- mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)

  if (type == "d" | type == "g") {
    if (paired) {
      denominator <- stats::sd(x - y, na.rm = TRUE)
    } else {
      if (pooled_sd) {
        denominator <- suppressWarnings(sd_pooled(x, y))
      } else {
        denominator <- stats::sd(c(x, y), na.rm = TRUE)
      }
    }
  } else if (type == "delta") {
    denominator <- stats::sd(x, na.rm = TRUE)
  }

  out <- data.frame(d = diff_of_means / denominator)
  types <- c("d" = "Cohens_d", "g" = "Hedges_g", "delta" = "Glass_delta")
  colnames(out) <- types[type]
  out <- cbind(
    out,
    .ci_from_t_test(x, y, type = type, paired = paired, ci = ci)
  )

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
  if (correction == TRUE) {
    out[, colnames(out) %in% c(types, "CI_low", "CI_high")] <-
      out[, colnames(out) %in% c(types, "CI_low", "CI_high")] *
      ((n - 3) / (n - 2.25)) * sqrt((n - 2) / n)
  }

  class(out) <- c("effectsize_table", class(out))
  return(out)
}






# Utils -------------------------------------------------------------------



#' @keywords internal
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
    trms <- terms(x)

    group <- all.vars(delete.response(trms))
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

#' @keywords internal
.ci_from_t_test <- function(x, y, type = "d", paired = FALSE, ci = 0.95){
  if (paired) {
    tobj <- stats::t.test(x - y)
    df <- length(x) - 1
  } else {
    if (type == "delta") {
      tobj <- stats::t.test(x, mu = mean(y))
      df <- length(x) - 1
      paired <- TRUE
    } else {
      tobj <- stats::t.test(x, y, var.equal = TRUE)
      df <- length(c(x,y)) - 2
    }
  }

  t_to_d(unname(tobj$statistic), df, ci = ci, paired = paired)[, -1 , drop = FALSE]
}