#' Convert test statistics (F, t) to indices of \strong{partial} variance explained (\strong{partial} Eta / Omega / Epsilon squared and Cohen's f)
#'
#' These functions are convenience functions to convert F and t test statistics to
#' \strong{partial} Eta squared, (\eqn{\eta{_p}^2}), Omega squared (\eqn{\omega{_p}^2}),
#' Epsilon squared (\eqn{\epsilon{_p}^2} (an alias for the adjusted Eta squared) and Cohen's f.
#' These are useful in cases where the various Sum of Squares and Mean Squares are not
#' easily available or their computation is not straightforward (e.g., in liner mixed models,
#' contrasts, etc.). For test statistics derived from \code{lm} and \code{aov} models, these
#' functions give exact results. For all other cases, they return close approximations.
#' \cr
#' See \href{https://easystats.github.io/effectsize/articles/from_test_statistics.html}{Effect Size from Test Statistics vignette.}
#'
#' @param t,f The t or the F statistics.
#' @param df,df_error Degrees of freedom of numerator or of the error estimate (i.e., the residuals).
#' @inheritParams chisq_to_phi
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data frame with the effect size(s) between 0-1, and confidence interval(s) (Note that for \eqn{\omega_p^2} and \eqn{\epsilon_p^2}
#' it is possible to compute a negative number; even though this doesn't make any practical sense,
#' it is recommended to report the negative number and not a 0).
#'
#' @details These functions use the following formulae:
#' \cr
#' \deqn{\eta_p^2 = \frac{F \times df_{num}}{F \times df_{num} + df_{den}}}
#' \cr
#' \deqn{\epsilon_p^2 = \frac{(F - 1) \times df_{num}}{F \times df_{num} + df_{den}}}
#' \cr
#' \deqn{\omega_p^2 = \frac{(F - 1) \times df_{num}}{F \times df_{num} + df_{den} + 1}}
#' \cr
#' \deqn{f_p = \sqrt{\frac{\eta_p^2}{1-\eta_p^2}}}
#' \cr\cr\cr
#' For \eqn{t}, the conversion is based on the equality of \eqn{t^2 = F} when \eqn{df_{num}=1}.
#' \subsection{Confidence Intervals}{
#' Confidence intervals are estimated using the Noncentrality parameter method;
#' These methods searches for a the best \code{ncp} (non-central parameters) for
#' of the noncentral F distribution for the desired tail-probabilities,
#' and then convert these \code{ncp}s to the corresponding effect sizes.
#' }
#'
#' @note \eqn{Adj. \eta_p^2} is an alias for \eqn{\epsilon_p^2}.
#'
#' @examples
#' \donttest{
#' if (require("afex")) {
#'   data(md_12.1)
#'   aov_ez("id", "rt", md_12.1,
#'     within = c("angle", "noise"),
#'     anova_table = list(correction = "none", es = "pes")
#'   )
#' }
#' # compare to:
#' F_to_eta2(
#'   f = c(40.72, 33.77, 45.31),
#'   df = c(2, 1, 2),
#'   df_error = c(18, 9, 18)
#' )
#'
#'
#' if (require("lmerTest")) { # for the df_error
#'   fit <- lmer(extra ~ group + (1 | ID), sleep)
#'   anova(fit)
#'   # Type III Analysis of Variance Table with Satterthwaite's method
#'   #       Sum Sq Mean Sq NumDF DenDF F value   Pr(>F)
#'   # group 12.482  12.482     1     9  16.501 0.002833 **
#'   # ---
#'   # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#'   F_to_eta2(16.501, 1, 9)
#'   F_to_omega2(16.501, 1, 9)
#'   F_to_epsilon2(16.501, 1, 9)
#'   F_to_f(16.501, 1, 9)
#' }
#'
#' ## Use with emmeans based contrasts
#' if (require(emmeans) & require(dplyr)) {
#'   warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
#'
#'   joint_tests(warp.lm, by = "wool") %>%
#'     bind_cols(F_to_eta2(.$F.ratio, .$df1, .$df2))
#' }
#' }
#' @references
#' \itemize{
#'   \item Friedman, H. (1982). Simplified determinations of statistical power, magnitude of effect and research sample sizes. Educational and Psychological Measurement, 42(2), 521-526. \doi{10.1177/001316448204200214}
#'   \item Mordkoff, J. T. (2019). A Simple Method for Removing Bias From a Popular Measure of Standardized Effect Size: Adjusted Partial Eta Squared. Advances in Methods and Practices in Psychological Science, 2(3), 228-232. \doi{10.1177/2515245919855053}
#'   \item Albers, C., & Lakens, D. (2018). When power analyses based on pilot data are biased: Inaccurate effect size estimators and follow-up bias. Journal of experimental social psychology, 74, 187-195. \doi{10.31234/osf.io/b7z4q}
#'   \item Steiger, J. H. (2004). Beyond the F test: Effect size confidence intervals and tests of close fit in the analysis of variance and contrast analysis. Psychological Methods, 9, 164-182.
#'   \item Cumming, G., & Finch, S. (2001). A primer on the understanding, use, and calculation of confidence intervals that are based on central and noncentral distributions. Educational and Psychological Measurement, 61(4), 532-574.
#' }
#'
#' @export
F_to_eta2 <- function(f, df, df_error, ci = 0.9, ...) {
  .F_to_pve(f, df, df_error, ci = ci, es = "eta2")
}

#' @rdname F_to_eta2
#' @export
t_to_eta2 <- function(t, df_error, ci = 0.9, ...) {
  F_to_eta2(t^2, 1, df_error, ci = ci)
}

#' @rdname F_to_eta2
#' @export
F_to_epsilon2 <- function(f, df, df_error, ci = 0.9, ...) {
  .F_to_pve(f, df, df_error, ci = ci, es = "epsilon2")
}

#' @rdname F_to_eta2
#' @export
t_to_epsilon2 <- function(t, df_error, ci = 0.9, ...) {
  F_to_epsilon2(t^2, 1, df_error, ci = ci)
}

#' @rdname F_to_eta2
#' @export
F_to_eta2_adj <- F_to_epsilon2

#' @rdname F_to_eta2
#' @export
t_to_eta2_adj <- t_to_epsilon2

#' @rdname F_to_eta2
#' @export
F_to_omega2 <- function(f, df, df_error, ci = 0.9, ...) {
  .F_to_pve(f, df, df_error, ci = ci, es = "omega2")
}

#' @rdname F_to_eta2
#' @export
t_to_omega2 <- function(t, df_error, ci = 0.9, ...) {
  F_to_omega2(t^2, 1, df_error, ci = ci)
}


#' @rdname F_to_eta2
#' @export
F_to_f <- function(f, df, df_error, ci = 0.9, ...){
  res_eta <- F_to_eta2(f, df, df_error, ci = ci)

  res <- data.frame(Cohens_f_partial = sqrt(res_eta$Eta_Sq_partial /
                                              (1 - res_eta$Eta_Sq_partial)))

  if (is.numeric(ci)) {
    res$CI <- res_eta$CI
    res$CI_low <- sqrt(res_eta$CI_low /
                         (1 - res_eta$CI_low))
    res$CI_high <- sqrt(res_eta$CI_high /
                          (1 - res_eta$CI_high))
  }

  class(res) <- c("effectsize_table",class(res))
  return(res)
}


#' @rdname F_to_eta2
#' @export
t_to_f <- function(t, df_error, ci = 0.9, ...){
  F_to_f(t^2, 1, df_error, ci = ci)
}


#' @keywords internal
.F_to_pve <- function(f, df, df_error, ci, es){
  switch(es,
          eta2 = {
            es_f <- function(.f, df, df_error) {
              (.f * df) / (.f * df + df_error)
            }
            es_name <- "Eta_Sq_partial"
          },
          epsilon2 = {
            es_f <- function(.f, df, df_error) {
              ((.f - 1) * df) / (.f * df + df_error)
            }
            es_name <- "Epsilon_Sq_partial"
          },
          omega2 = {
            es_f <- function(.f, df, df_error) {
              ((.f - 1) * df) / (.f * df + df_error + 1)
            }
            es_name <- "Omega_Sq_partial"
          }
  )

  res <- data.frame(ES = es_f(f, df, df_error))
  colnames(res) <- es_name

  if (is.numeric(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)
    res$CI <- ci
    fs <- t(mapply(.get_ncp_F, f, df, df_error, ci))

    res$CI_low <- es_f(fs[, 1], df, df_error)
    res$CI_high <- es_f(fs[, 2], df, df_error)
  }

  class(res) <- c("effectsize_table", class(res))
  return(res)
}