#' Convert Between *d*, *r*, and Odds Ratio
#'
#' Enables a conversion between different indices of effect size, such as
#' standardized difference (Cohen's d), correlation r or (log) odds ratios.
#'
#' @param d Standardized difference value (Cohen's d).
#' @param r Correlation coefficient r.
#' @param OR *Odds ratio* values in vector or data frame.
#' @param log Take in or output the log of the ratio (such as in logistic models).
#' @param ... Arguments passed to or from other methods.
#'
#' @family convert between effect sizes
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
#' - \eqn{d = \frac{2 * r}{\sqrt{1 - r^2}}}{d = 2 * r / sqrt(1 - r^2)}
#' - \eqn{r = \frac{d}{\sqrt{d^2 + 4}}}{r = d / sqrt(d^2 + 4)}
#'
#' The conversion from *d* to *r* assumes equally sized groups. The resulting
#' *r* is also called the binomial effect size display (BESD; Rosenthal et al.,
#' 1982).
#'
#' @references
#' - Sánchez-Meca, J., Marín-Martínez, F., & Chacón-Moscoso, S. (2003).
#' Effect-size indices for dichotomized outcomes in meta-analysis. Psychological
#' methods, 8(4), 448.
#'
#' - Borenstein, M., Hedges, L. V., Higgins, J. P. T., & Rothstein, H. R.
#' (2009). Converting among effect sizes. Introduction to meta-analysis, 45-49.
#'
#' - Rosenthal, R., & Rubin, D. B. (1982). A simple, general purpose display of
#' magnitude of experimental effect. Journal of educational psychology, 74(2), 166.
#'
#' @export
#' @aliases convert_d_to_r
d_to_r <- function(d, ...) {
  d / (sqrt(d^2 + 4))
}

#' @export
convert_d_to_r <- d_to_r






#' @rdname d_to_r
#' @aliases convert_r_to_d
#' @export
r_to_d <- function(r, ...) {
  2 * r / sqrt(1 - r^2)
}

#' @export
convert_r_to_d <- r_to_d



# OR - d ----------------------------------------------------------------

#' @rdname d_to_r
#' @aliases convert_oddsratio_to_d
#' @export
oddsratio_to_d <- function(OR, log = FALSE, ...) {
  if (log) {
    log_OR <- OR
  } else {
    log_OR <- log(OR)
  }

  log_OR * (sqrt(3) / pi)
}

#' @export
convert_oddsratio_to_d <- oddsratio_to_d



#' @rdname d_to_r
#' @aliases convert_logoddsratio_to_d
#' @export
logoddsratio_to_d <- function(OR, log = TRUE, ...) {
  oddsratio_to_d(OR, log = log, ...)
}

#' @export
convert_logoddsratio_to_d <- logoddsratio_to_d



#' @rdname d_to_r
#' @aliases convert_d_to_oddsratio
#' @export
d_to_oddsratio <- function(d, log = FALSE, ...) {
  log_OR <- d * pi / sqrt(3)

  if (log) {
    log_OR
  } else {
    exp(log_OR)
  }
}

#' @export
convert_d_to_oddsratio <- d_to_oddsratio




# OR - r ----------------------------------------------------------------

#' @rdname d_to_r
#' @aliases convert_oddsratio_to_r
#' @export
oddsratio_to_r <- function(OR, log = FALSE, ...) {
  d_to_r(oddsratio_to_d(OR, log = log))
}

#' @export
convert_oddsratio_to_r <- oddsratio_to_r

#' @rdname d_to_r
#' @aliases convert_logoddsratio_to_r
#' @export
logoddsratio_to_r <- function(OR, log = TRUE, ...) {
  oddsratio_to_r(OR, log = log, ...)
}

#' @export
convert_logoddsratio_to_r <- logoddsratio_to_r



#' @rdname d_to_r
#' @aliases convert_r_to_oddsratio
#' @export
r_to_oddsratio <- function(r, log = FALSE, ...) {
  d_to_oddsratio(r_to_d(r), log = log)
}

#' @export
convert_r_to_oddsratio <- r_to_oddsratio
