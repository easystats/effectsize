#' Convert between *d*, *r* and *Odds ratio*
#'
#' Enables a conversion between different indices of effect size, such as
#' standardized difference (Cohen's d), correlation r or (log) odds ratios.
#'
#' @param d Standardized difference value (Cohen's d).
#' @param r Correlation coefficient r.
#' @param odds *Odds ratio* values in vector or data frame.
#' @param log Take in or output log odds ratio (such as in logistic models).
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' r_to_d(0.5)
#' d_to_odds(d = 1.154701)
#' odds_to_r(odds = 8.120534)
#'
#' d_to_r(d = 1)
#' r_to_odds(0.4472136, log = TRUE)
#' odds_to_d(1.813799, log = TRUE)
#' @return Converted index.
#'
#' @details
#' - *d to r*: \eqn{d = \frac{2 * r}{\sqrt{1 - r^2}}}
#' - *r to d*: \eqn{r = \frac{d}{\sqrt{d^2 + 4}}}
#' - *OR to d*: \eqn{d = \frac{\log(OR)\times\sqrt{3}}{\pi}}
#' - *d to OR*: \eqn{log(OR) = d * \frac{\pi}{\sqrt(3)}}
#'
#' Conversions between *OR* and *r* is done through these formulae.
#' \cr\cr
#' When converting *d* to *r*, the resulting *r* is also called the binomial
#' effect size display (BESD; Rosenthal et al., 1982).
#'
#' @references
#' - Sánchez-Meca, J., Marín-Martínez, F., & Chacón-Moscoso, S. (2003). Effect-size indices for dichotomized outcomes in meta-analysis. Psychological methods, 8(4), 448.
#' - Borenstein, M., Hedges, L. V., Higgins, J. P. T., & Rothstein, H. R. (2009). Converting among effect sizes. Introduction to meta-analysis, 45-49.
#' - Rosenthal, R., & Rubin, D. B. (1982). A simple, general purpose display of magnitude of experimental effect. Journal of educational psychology, 74(2), 166.
#'
#' @export
d_to_r <- function(d, ...) {
  d / (sqrt(d^2 + 4))
}


#' @rdname d_to_r
#' @export
r_to_d <- function(r, ...) {
  2 * r / sqrt(1 - r^2)
}



#' @rdname d_to_r
#' @export
convert_d_to_r <- d_to_r

#' @rdname d_to_r
#' @export
convert_r_to_d <- r_to_d



# Odds - d ----------------------------------------------------------------



#' @rdname d_to_r
#' @export
odds_to_d <- function(odds, log = FALSE, ...) {
  if (log == FALSE) {
    log_odds <- log(odds)
  } else {
    log_odds <- odds
  }

  log_odds * (sqrt(3) / pi)
}

#' @rdname d_to_r
#' @export
convert_odds_to_d <- odds_to_d

#' @rdname d_to_r
#' @export
logodds_to_d <- function(odds, log = TRUE, ...) {
  odds_to_d(odds, log = log, ...)
}



#' @rdname d_to_r
#' @export
d_to_odds <- function(d, log = FALSE, ...) {
  if (log == TRUE) {
    d * pi / sqrt(3)
  } else {
    exp(d * pi / sqrt(3))
  }
}

#' @rdname d_to_r
#' @export
convert_d_to_odds <- d_to_odds




# Odds - r ----------------------------------------------------------------

#' @rdname d_to_r
#' @export
odds_to_r <- function(odds, log = FALSE, ...) {
  d_to_r(odds_to_d(odds, log = log))
}

#' @rdname d_to_r
#' @export
convert_odds_to_r <- odds_to_r

#' @rdname d_to_r
#' @export
logodds_to_r <- function(odds, log = TRUE, ...) {
  odds_to_r(odds, log = log, ...)
}



#' @rdname d_to_r
#' @export
r_to_odds <- function(r, log = FALSE, ...) {
  d_to_odds(r_to_d(r), log = log)
}

#' @rdname d_to_r
#' @export
convert_r_to_odds <- r_to_odds




