#' Convert Between *d*, *r*, and Odds Ratio
#'
#' Enables a conversion between different indices of effect size, such as
#' standardized difference (Cohen's d), (point-biserial) correlation r or (log) odds ratios.
#'
#' @param d,r,OR,logOR Standardized difference value (Cohen's d), correlation
#'   coefficient (r), Odds ratio, or logged Odds ratio.
#' @param n1,n2 Group sample sizes. If either is missing, groups are assumed to be of equal size.
#' @param p0 Baseline risk. If not specified, the _d_ to _OR_ conversion uses am approximation (see details).
#' @param log Take in or output the log of the ratio (such as in logistic models),
#'   e.g. when the desired input or output are log odds ratios instead odds ratios.
#' @param ... Arguments passed to or from other methods.
#'
#' @family convert between effect sizes
#' @seealso [cohens_d()]
#'
#' @examples
#' r_to_d(0.5)
#' d_to_oddsratio(1.154701)
#' oddsratio_to_r(8.120534)
#'
#' d_to_r(1)
#' r_to_oddsratio(0.4472136, log = TRUE)
#' oddsratio_to_d(1.813799, log = TRUE)
#'
#' @return Converted index.
#'
#' @details
#' Conversions between *d* and *OR* is done through these formulae:
#' - \eqn{d = \frac{\log(OR)\times\sqrt{3}}{\pi}}{d = log(OR) * sqrt(3) / pi}
#' - \eqn{log(OR) = d * \frac{\pi}{\sqrt(3)}}{log(OR) = d * pi / sqrt(3)}
#'
#' Converting between *d* and *r* is done through these formulae:
#' - \eqn{d = \frac{\sqrt{h} * r}{\sqrt{1 - r^2}}}{d = sqrt(h) * r / sqrt(1 - r^2)}
#' - \eqn{r = \frac{d}{\sqrt{d^2 + h}}}{r = d / sqrt(d^2 + h)}
#'
#' Where \eqn{h = \frac{n_1 + n_2 - 2}{n_1} + \frac{n_1 + n_2 - 2}{n_2}}{h = (n1 + n2 - 2) / n1 + (n1 + n2 - 2) / n2}.
#' When groups are of equal size, *h* reduces to approximately 4. The resulting
#' *r* is also called the binomial effect size display (BESD; Rosenthal et al.,
#' 1982).
#'
#' @references
#' - Borenstein, M., Hedges, L. V., Higgins, J. P. T., & Rothstein, H. R.
#' (2009). Converting among effect sizes. Introduction to meta-analysis, 45-49.
#'
#' - Jacobs, P., & Viechtbauer, W. (2017). Estimation of the biserial
#' correlation and its sampling variance for use in meta-analysis. Research
#' synthesis methods, 8(2), 161-180. \doi{10.1002/jrsm.1218}
#'
#' - Rosenthal, R., & Rubin, D. B. (1982). A simple, general purpose display of
#' magnitude of experimental effect. Journal of educational psychology, 74(2), 166.
#'
#' - Sánchez-Meca, J., Marín-Martínez, F., & Chacón-Moscoso, S. (2003).
#' Effect-size indices for dichotomized outcomes in meta-analysis. Psychological
#' methods, 8(4), 448.
#'
#' @export
d_to_r <- function(d, n1, n2, ...) {
  h <- .get_rd_h(n1, n2)
  d / (sqrt(d^2 + h))
}

#' @rdname d_to_r
#' @export
r_to_d <- function(r, n1, n2, ...) {
  h <- .get_rd_h(n1, n2)
  sqrt(h) * r / sqrt(1 - r^2)
}

# OR - d ----------------------------------------------------------------

#' @rdname d_to_r
#' @export
oddsratio_to_d <- function(OR, p0, log = FALSE, ...) {
  if (missing(p0) || !is.numeric(p0)) {
    # Use approximation
    if (log) {
      log_OR <- OR
    } else {
      log_OR <- log(OR)
    }

    return(log_OR * (sqrt(3) / pi))
  }


  if (log) {
    OR <- exp(OR)
  }

  odds1 <- OR * probs_to_odds(p0)
  p1 <- odds_to_probs(odds1)
  stats::qnorm(p1) - stats::qnorm(p0)
}

#' @rdname d_to_r
#' @export
logoddsratio_to_d <- function(logOR, p0, log = TRUE, ...) {
  oddsratio_to_d(logOR, p0, log = log, ...)
}

#' @rdname d_to_r
#' @export
d_to_oddsratio <- function(d, log = FALSE, ...) {
  log_OR <- d * pi / sqrt(3)

  if (log) {
    log_OR
  } else {
    exp(log_OR)
  }
}

#' @rdname d_to_r
#' @export
d_to_logoddsratio <- function(d, log = TRUE, ...) {
  d_to_oddsratio(d, log = log, ...)
}




# OR - r ----------------------------------------------------------------

#' @rdname d_to_r
#' @export
oddsratio_to_r <- function(OR, p0, n1, n2, log = FALSE, ...) {
  d_to_r(oddsratio_to_d(OR, p0, log = log), n1, n2)
}

#' @rdname d_to_r
#' @export
logoddsratio_to_r <- function(logOR, p0, n1, n2, log = TRUE, ...) {
  oddsratio_to_r(logOR, p0, n1, n2, log = log, ...)
}


#' @rdname d_to_r
#' @export
r_to_oddsratio <- function(r, n1, n2, log = FALSE, ...) {
  d_to_oddsratio(r_to_d(r), log = log, n1, n2)
}

#' @rdname d_to_r
#' @export
r_to_logoddsratio <- function(r, n1, n2, log = TRUE, ...) {
  r_to_oddsratio(r, n1, n2, log = log)
}


# Utils -------------------------------------------------------------------

#' @keywords internal
.get_rd_h <- function(n1, n2) {
  if (missing(n1) && missing(n2)) {
    return(4)
  }

  if (missing(n1)) n1 <- n2
  if (missing(n2)) n2 <- n1
  m <- n1 + n2 - 2
  m / n1 + m / n2
}
