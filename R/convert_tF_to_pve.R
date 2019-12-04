#' Convert test statistics (F, t) to indices of \strong{PARTIAL} variance explained (\strong{partial} Eta / Omega / Epsilon squared)
#'
#' These functions are convenience functions to convert F and t test statistics to \strong{partial} Eta squared, (\eqn{\eta{_p}^2}), Omega squared (\eqn{\omega{_p}^2}) and Epsilon squared (\eqn{\epsilon{_p}^2}; an alias for the adjusted Eta squared). These are useful in cases where the various Sum of Squares and Mean Squares are not easily available or their computation is not straightforward (e.g., in liner mixed models, contrasts, etc.). For test statistics derived from \code{lm} and \code{aov} models, these functions give exact results. For all other cases, they return close approximations.
#'
#' @param t,f The t or the F statistics.
#' @param df,df_error Degrees of freedom of numerator or of the error estimate (i.e., the residuals).
#' @param ... Arguments passed to or from other methods.
#'
#' @return A numeric value between 0-1 (Note that for \eqn{\omega_p^2} and \eqn{\epsilon_p^2}
#' it is possible to compute a negative number; even though this doesn't make any practical sense,
#' it is recommended to report the negative number and not a 0).
#'
#' @details These functions use the following formulae:
#' \cr\cr
#' \deqn{\eta_p^2 = \frac{F \times df_{num}}{F \times df_{num} + df_{den}}}
#' \cr\cr
#' \deqn{\epsilon_p^2 = \frac{(F - 1) \times df_{num}}{F \times df_{num} + df_{den}}}
#' \cr\cr
#' \deqn{\omega_p^2 = \frac{(F - 1) \times df_{num}}{F \times df_{num} + df_{den} + 1}}
#' \cr\cr\cr
#' For \eqn{t}, the conversion is based on the equality of \eqn{t^2 = F} when \eqn{df_{num}=1}.
#'
#' @note \eqn{Adj. \eta_p^2} is an alias for \eqn{\epsilon_p^2}.
#'
#' @examples
#' \donttest{
#' library(afex)
#' data(md_12.1)
#' aov_ez("id", "rt", md_12.1,
#'   within = c("angle", "noise"),
#'   anova_table = list(correction = "none", es = "pes")
#' )
#' # compare to:
#' F_to_eta2(40.72, 2, 18)
#' F_to_eta2(33.77, 1, 9)
#' F_to_eta2(45.31, 2, 18)
#'
#'
#' library(lmerTest) # for the df_error
#' fit <- lmer(extra ~ group + (1 | ID), sleep)
#' anova(fit)
#' # Type III Analysis of Variance Table with Satterthwaite's method
#' #       Sum Sq Mean Sq NumDF DenDF F value   Pr(>F)
#' # group 12.482  12.482     1     9  16.501 0.002833 **
#' # ---
#' # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#' F_to_eta2(16.501, 1, 9)
#' F_to_omega2(16.501, 1, 9)
#' F_to_epsilon2(16.501, 1, 9)
#' }
#'
#' @references
#' \itemize{
#'   \item Friedman, H. (1982). Simplified determinations of statistical power, magnitude of effect and research sample sizes. Educational and Psychological Measurement, 42(2), 521-526. \doi{10.1177/001316448204200214}
#'   \item Mordkoff, J. T. (2019). A Simple Method for Removing Bias From a Popular Measure of Standardized Effect Size: Adjusted Partial Eta Squared. Advances in Methods and Practices in Psychological Science, 2(3), 228-232. \doi{10.1177/2515245919855053}
#'   \item Albers, C., & Lakens, D. (2018). When power analyses based on pilot data are biased: Inaccurate effect size estimators and follow-up bias. Journal of experimental social psychology, 74, 187-195. \doi{10.31234/osf.io/b7z4q}
#' }
#'
#' @export
F_to_eta2 <- function(f, df, df_error, ...) {
  (f * df) / (f * df + df_error)
}



#' @rdname F_to_eta2
#' @export
t_to_eta2 <- function(t, df_error, ...) {
  F_to_eta2(t^2, 1, df_error)
}

#' @rdname F_to_eta2
#' @export
F_to_epsilon2 <- function(f, df, df_error, ...) {
  ((f - 1) * df) / (f * df + df_error)
}

#' @rdname F_to_eta2
#' @export
t_to_epsilon2 <- function(t, df_error, ...) {
  F_to_epsilon2(t^2, 1, df_error)
}

#' @rdname F_to_eta2
#' @export
F_to_eta2_adj <- F_to_epsilon2

#' @rdname F_to_eta2
#' @export
t_to_eta2_adj <- t_to_epsilon2

#' @rdname F_to_eta2
#' @export
F_to_omega2 <- function(f, df, df_error, ...) {
  ((f - 1) * df) / (f * df + df_error + 1)
}

#' @rdname F_to_eta2
#' @export
t_to_omega2 <- function(t, df_error, ...) {
  F_to_omega2(t^2, 1, df_error)
}
