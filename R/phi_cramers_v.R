#' Effect size for contingency tables
#'
#' Compute Cramer's *V*, phi (\eqn{\phi}), Cohen's *w* (an alias of phi) and
#' Cohen's *g* for contingency tables or goodness-of-fit. See details.
#'
#' @inheritParams stats::chisq.test
#' @param ci Confidence Interval (CI) level
#' @param adjust Should the effect size be bias-corrected? Defaults to `FALSE`.
#' @param CI Deprecated in favor of `ci`.
#' @param ... Arguments passed to [stats::chisq.test()], such as `p`. Ignored
#'   for `cohens_g()`.
#'
#' @details
#' Cramer's *V* and phi (\eqn{\phi}) are effect sizes for tests of independence
#' in 2D contingency tables, or for goodness-of-fit in 1D tables. For 2x2
#' tables, all 3 are identical, and are equal to the simple correlation between
#' two dichotomous variables, ranging between  0 (no dependence) and 1 (perfect
#' dependence). For larger tables, Cramer's *V* should be used, as it is bounded
#' between 0-1, whereas phi can be larger than 1.
#' \cr\cr
#' Cohen's *g* is an effect size for dependent (paired) contingency tables
#' ranging between 0 (perfect symmetry) and 0.5 (perfect asymmetry) (see
#' [stats::mcnemar.test()]).
#'
#' ## Confidence Intervals
#' For Cramer's *V* and phi, confidence intervals are estimated using the
#' Noncentrality parameter method; These methods searches for a the best `ncp`
#' (non-central parameters) for of the noncentral Chi-squared distribution for
#' the desired tail-probabilities, and then convert these `ncp`s to the
#' corresponding effect sizes.
#' \cr\cr
#' For Cohen's *g*, confidence intervals are based on the proportion (\eqn{P = g
#' + 0.5}) confidence intervals returned by [stats::prop.test()] (minus 0.5),
#' which give a good close approximation.
#'
#' @return A data frame with the effect size(s), and confidence interval(s).
#'
#' @seealso [chisq_to_phi()] for details regarding estimation and CIs.
#'
#' @examples
#' M <- rbind(c(150, 130, 35, 55),
#'            c(100, 50,  10, 40),
#'            c(165, 65,  2,  25))
#'
#' dimnames(M) <- list(Study = c("Psych", "Econ", "Law"),
#'                     Music = c("Pop", "Rock", "Jazz", "Classic"))
#' M
#'
#' phi(M)
#'
#' cramers_v(M)
#'
#'
#' Performance <-
#'   matrix(c(794, 86, 150, 570),
#'          nrow = 2,
#'          dimnames = list("1st Survey" = c("Approve", "Disapprove"),
#'                          "2nd Survey" = c("Approve", "Disapprove")))
#'
#' cohens_g(Performance)
#'
#' @references
#' - Cohen, J. (1988). Statistical power analysis for the behavioural sciences.
#'
#' @importFrom stats chisq.test
#' @export
phi <- function(x, y = NULL, ci = 0.95, adjust = FALSE, CI, ...){
  if (!missing(CI)) {
    ci <- CI
    warning("'CI' argument is deprecated. Use 'ci' instead.")
  }

  res <- suppressWarnings(stats::chisq.test(x, y, ...))
  Obs <- res$observed
  Exp <- res$expected

  if (inherits(Exp, "table")) {
    nr <- nrow(Obs)
    nc <- ncol(Obs)
  } else {
    nr <- length(Obs)
    nc <- 1
  }

  chisq_to_phi(chisq = .chisq(Obs, Exp),
               n = sum(Obs),
               nrow = nr,
               ncol = nc,
               ci = ci,
               adjust = adjust)
}

#' @rdname phi
#' @export
cohens_w <- phi

#' @rdname phi
#' @importFrom stats chisq.test
#' @export
cramers_v <- function(x, y = NULL, ci = 0.95, adjust = FALSE, CI,...){
  if (!missing(CI)) {
    ci <- CI
    warning("'CI' argument is deprecated. Use 'ci' instead.")
  }

  res <- suppressWarnings(stats::chisq.test(x, y, ...))
  Obs <- res$observed
  Exp <- res$expected

  if (inherits(Exp, "table")) {
    nr <- nrow(Obs)
    nc <- ncol(Obs)
  } else {
    nr <- length(Obs)
    nc <- 1
  }

  chisq_to_cramers_v(chisq = .chisq(Obs, Exp),
                     n = sum(Obs),
                     nrow = nr,
                     ncol = nc,
                     ci = ci,
                     adjust = adjust)
}

#' @rdname phi
#' @export
#' @importFrom stats complete.cases prop.test
cohens_g <- function(x, y = NULL, ci = 0.95, ...) {
  if (!is.matrix(x)) {
    if (is.null(y))
      stop("if 'x' is not a matrix, 'y' must be given")
    if (length(x) != length(y))
      stop("'x' and 'y' must have the same length")
    OK <- stats::complete.cases(x, y)
    x <- as.factor(x[OK])
    y <- as.factor(y[OK])
    if ((nlevels(x) < 2) || (nlevels(y) != nlevels(x)))
      stop("'x' and 'y' must have the same number of levels (minimum 2)")
    x <- table(x, y)
  } else {
    if ((nrow(x) < 2) || (ncol(x) != nrow(x)))
      stop("'x' must be square with at least two rows and columns")
  }


  b <- x[upper.tri(x)]
  c <- t(x)[upper.tri(x)]

  P <- sum(pmax(b, c)) / (sum(b) + sum(c))
  g <- P - 0.5

  out <- data.frame(cohens_g = g)

  if (is.numeric(ci)) {
    n <- sum(b) + sum(c)
    k <- P * n

    res <- stats::prop.test(k, n, p = 0.5,
                            conf.level = ci,
                            correct = FALSE)

    out$CI <- ci
    out$CI_low <- res$conf.int[1] - 0.5
    out$CI_high <- res$conf.int[2] - 0.5
  }

  class(out) <- c("effectsize_table", "see_effectsize_table", class(out))
  return(out)
}



#' @keywords internal
.chisq <- function(Obs, Exp) {
  sum(((Obs - Exp) ^ 2) / Exp)
}
