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
#' @family effect sizes for contingency table
#'
#'
#' @references
#' - Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd Ed.). New York: Routledge.
#'
#' @examples
#'
#' data("screening_test")
#'
#' phi(screening_test$Diagnosis, screening_test$Test1)
#'
#' phi(screening_test$Diagnosis, screening_test$Test2)
#'
#' # Both tests seem comparable - but are the tests actually different?
#'
#' (tests <- table(Test1 = screening_test$Test1, Test2 = screening_test$Test2))
#'
#' mcnemar.test(tests)
#'
#' cohens_g(tests)
#'
#' # Test 2 gives a negative result more than test 1!
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
      insight::format_error("if 'x' is not a matrix, 'y' must be given")
    }
    if (length(x) != length(y)) {
      insight::format_error("'x' and 'y' must have the same length")
    }
    OK <- stats::complete.cases(x, y)
    x <- as.factor(x[OK])
    y <- as.factor(y[OK])
    if ((nlevels(x) < 2) || (nlevels(y) != nlevels(x))) {
      insight::format_error("'x' and 'y' must have the same number of levels (minimum 2)")
    }
    x <- table(x, y)
  } else {
    if ((nrow(x) < 2) || (ncol(x) != nrow(x))) {
      insight::format_error("'x' must be square with at least two rows and columns")
    }
  }


  b <- x[upper.tri(x)]
  c <- t(x)[upper.tri(x)]

  P <- sum(pmax(b, c)) / (sum(b) + sum(c))
  g <- P - 0.5

  out <- data.frame(Cohens_g = g)

  ci_method <- NULL
  if (.test_ci(ci)) {
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
