#' Effect Size for Paired Contingency Tables
#'
#' Cohen's *g* is an effect size of asymmetry (or marginal heterogeneity) for
#' dependent (paired) contingency tables ranging between 0 (perfect symmetry)
#' and 0.5 (perfect asymmetry) (see [stats::mcnemar.test()]). (Note this is not
#' *not* a measure of (dis)agreement between the pairs, but of (a)symmetry.)
#'
#' @inheritParams oddsratio_to_d
#' @inheritParams phi
#' @param alternative a character string specifying the alternative hypothesis;
#'   Controls the type of CI returned: `"two.sided"` (two-sided CI; default),
#'   `"greater"` (one-sided CI) or `"less"` (one-sided CI). Partial matching is
#'   allowed (e.g., `"g"`, `"l"`, `"two"`...). See *One-Sided CIs* in
#'   [effectsize_CIs].
#' @param ... Ignored
#'
#' @details
#'
#' # Confidence (Compatibility) Intervals (CIs)
#' Confidence intervals are based on the proportion (\eqn{P = g + 0.5})
#' confidence intervals returned by [stats::prop.test()] (minus 0.5), which give
#' a good close approximation.
#'
#' @inheritSection effectsize_CIs CIs and Significance Tests
#'
#' @return A data frame with the effect size (`Cohens_g`, `Risk_ratio`
#'   (possibly with the prefix `log_`), `Cohens_h`) and its CIs (`CI_low` and
#'   `CI_high`).
#'
#' @seealso [phi()] and friends for other effect sizes for contingency tables.
#' @family effect size indices
#'
#'
#' @references
#' - Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd Ed.). New York: Routledge.
#'
#' @examples
#' Performance <- matrix(c(
#'   794, 150,
#'   86, 570
#' ), nrow = 2)
#' dimnames(Performance) <- list(
#'   "1st Survey" = c("Approve", "Disapprove"),
#'   "2nd Survey" = c("Approve", "Disapprove")
#' )
#' Performance
#'
#' cohens_g(Performance)
#'
#' @export
#' @importFrom stats complete.cases prop.test
cohens_g <- function(x, y = NULL,
                     ci = 0.95, alternative = "two.sided",
                     ...) {
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))

  if (.is_htest_of_type(x, "McNemar")) {
    return(effectsize(x, ci = ci, alternative = alternative))
  }


  if (!is.matrix(x)) {
    if (is.null(y)) {
      stop("if 'x' is not a matrix, 'y' must be given", call. = FALSE)
    }
    if (length(x) != length(y)) {
      stop("'x' and 'y' must have the same length", call. = FALSE)
    }
    OK <- stats::complete.cases(x, y)
    x <- as.factor(x[OK])
    y <- as.factor(y[OK])
    if ((nlevels(x) < 2) || (nlevels(y) != nlevels(x))) {
      stop("'x' and 'y' must have the same number of levels (minimum 2)", call. = FALSE)
    }
    x <- table(x, y)
  } else {
    if ((nrow(x) < 2) || (ncol(x) != nrow(x))) {
      stop("'x' must be square with at least two rows and columns", call. = FALSE)
    }
  }


  b <- x[upper.tri(x)]
  c <- t(x)[upper.tri(x)]

  P <- sum(pmax(b, c)) / (sum(b) + sum(c))
  g <- P - 0.5

  out <- data.frame(Cohens_g = g)

  ci_method <- NULL
  if (is.numeric(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)
    out$CI <- ci

    n <- sum(b) + sum(c)
    k <- P * n

    res <- stats::prop.test(k, n,
      p = 0.5,
      alternative = alternative,
      conf.level = ci,
      correct = FALSE
    )

    out$CI <- ci
    out$CI_low <- res$conf.int[1] - 0.5
    out$CI_high <- res$conf.int[2] - 0.5

    ci_method <- list(method = "binomial")
  }

  class(out) <- c("effectsize_table", "see_effectsize_table", class(out))
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- alternative
  return(out)
}
