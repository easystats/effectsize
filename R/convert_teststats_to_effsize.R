#' Compute Partial Variance Explained Effect Sizes From Test Statistics
#'
#' These functions are convenience functions to convert \eqn{F} and \eqn{t}
#' test statistics to \eqn{\eta_p^2}, \eqn{\omega_p^2}, \eqn{\epsilon_p^2}, or
#' \eqn{Adj. \eta_p^2}. These are useful in cases where the various \eqn{SS}s and
#' \eqn{MS}s are not easily available or their computation is not straightforward
#' (e.g., in liner mixed models, contrasts, etc.).
#' \cr\cr
#' For test statistics derived from \code{lm} and \code{aov} models, these functions
#' give exact results. For all other cases, these give practically exact results.
#'
#' @param F_stat The \eqn{F} statistic.
#' @param t_stat The \eqn{t} statistic.
#' @param df Numerator degrees of freedom for the \eqn{F} statistic.
#' @param df_residual Denominator degrees of freedom for the \eqn{F} and \eqn{t} statistics.
#'
#' @return A numeric integer between 0-1 (Note that for \eqn{\omega_p^2} and \eqn{\epsilon_p^2}
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
#' \dontrun{
#' library(afex)
#' data(md_12.1)
#' aov_ez("id", "rt", md_12.1,
#'   within = c("angle", "noise"),
#'   anova_table = list(correction = "none", es = "pes")
#' )
#' # compare to:
#' F_to_partial_eta_squared(40.72, 2, 18)
#' F_to_partial_eta_squared(33.77, 1, 9)
#' F_to_partial_eta_squared(45.31, 2, 18)
#'
#'
#' library(lmerTest) # for the df_residual
#' fit <- lmer(extra ~ group + (1 | ID), sleep)
#' anova(fit)
#' # Type III Analysis of Variance Table with Satterthwaite's method
#' #       Sum Sq Mean Sq NumDF DenDF F value   Pr(>F)
#' # group 12.482  12.482     1     9  16.501 0.002833 **
#' # ---
#' # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#' F_to_partial_eta_squared(16.501, 1, 9)
#' F_to_partial_omega_squared(16.501, 1, 9)
#' F_to_partial_epsilon_squared(16.501, 1, 9)
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
F_to_partial_eta_squared <- function(F_stat, df, df_residual) {
  (F_stat * df) / (F_stat * df + df_residual)
}



#' @rdname F_to_partial_eta_squared
#' @export
t_to_partial_eta_squared <- function(t_stat, df_residual) {
  F_to_partial_eta_squared(t_stat^2, 1, df_residual)
}

#' @rdname F_to_partial_eta_squared
#' @export
F_to_partial_epsilon_squared <- function(F_stat, df, df_residual) {
  ((F_stat - 1) * df) /
    (F_stat * df + df_residual)
}

#' @rdname F_to_partial_eta_squared
#' @export
t_to_partial_epsilon_squared <- function(t_stat, df_residual) {
  F_to_partial_epsilon_squared(t_stat^2, 1, df_residual)
}

#' @rdname F_to_partial_eta_squared
#' @export
F_to_adj_partial_eta_squared <- F_to_partial_epsilon_squared

#' @rdname F_to_partial_eta_squared
#' @export
t_to_adj_partial_eta_squared <- t_to_partial_epsilon_squared

#' @rdname F_to_partial_eta_squared
#' @export
F_to_partial_omega_squared <- function(F_stat, df, df_residual) {
  ((F_stat - 1) * df) / (F_stat * df + df_residual + 1)
}

#' @rdname F_to_partial_eta_squared
#' @export
t_to_partial_omega_squared <- function(t_stat, df_residual) {
  F_to_partial_omega_squared(t_stat^2, 1, df_residual)
}


#' Compute Effect sizes for Contingency Tables (\eqn{chi^2})
#'
#' These convert \eqn{chi^2} test statistics to \eqn{\phi} and \eqn{\Cramer's V}.
#'
#' @param chisq The \eqn{chi^2} statistic.
#' @param N The sample size
#' @param a The number of rows in the contingency table.
#' @param b The number of columns in the contingency tables.
#'
#' @return A numeric integer between 0-1.
#'
#' @examples
#' \dontrun{
#' ctab <- as.table(rbind(c(762, 327, 468), c(484, 239, 477), c(484, 239, 477)))
#' N <- sum(ctab)
#' a <- nrow(ctab)
#' b <- ncol(ctab)
#'
#' (Xsq <- chisq.test(ctab))
#' #
#' #         Pearson's Chi-squared test
#' #
#' # data:  ctab
#' # X-squared = 41.234, df = 4, p-value = 2.405e-08
#'
#' chisq_to_phi(30.07, N)
#' chisq_to_cramers_V(30.07, N, a, b)
#' }
#'
#' @export
chisq_to_phi <- function(chisq, N) {
  sqrt(chisq / N)
}

#' @rdname chisq_to_phi
#' @export
chisq_to_cramers_V <- function(chisq, N, a, b) {
  chisq_to_phi(chisq, N) /
    sqrt((min(a, b) - 1))
}
