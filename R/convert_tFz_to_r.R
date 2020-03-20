# t -----------------------------------------------------------------------

#' Convert test statistics (t, z, F) to effect sizes of differences (Cohen's d) or association (\strong{partial} r)
#'
#' These functions are convenience functions to convert t, z and F test statistics to Cohen's d and
#' \strong{partial} r. These are useful in cases where the data required to compute these are not easily
#' available or their computation is not straightforward (e.g., in liner mixed models, contrasts, etc.).
#'
#' @param r The correlation coefficient r.
#' @param t,f,z The t, the F or the z statistics.
#' @param df,df_error Degrees of freedom of numerator or of the error estimate (i.e., the residuals).
#' @param n The number of observations (the sample size).
#' @param pooled Should the estimate accout for the t-value being based on a repeated-measures design, or not (default).
#' @param CI Confidence Interval (CI) level
#' @param ... Arguments passed to or from other methods.
#'
#' #' @return A data frame with the effect size(s) between 0-1, and confidence interval(s)
#'
#' @details These functions use the following formulae:
#' \cr\cr
#' \deqn{r_{partial} = t / \sqrt{t^2 + df_{error}}}
#' \cr\cr
#' \deqn{r_{partial} = z / \sqrt{z^2 + N}}
#' \cr\cr
#' \deqn{Cohen's d = 2 * t / \sqrt{df_{error}}}
#' \cr\cr
#' \deqn{Cohen's d_z = t / \sqrt{df_{error}}}
#' \cr\cr
#' \deqn{Cohen's d = 2 * z / \sqrt{N}}
#'
#' \subsection{Confidence Intervals}{
#' Confidence intervals are estimated using the Noncentrality parameter method;
#' These methods searches for a the best \code{ncp} (non-central parameters) for
#' of the noncentral F distribution for the desired tail-probabilities,
#' and then convert these \code{ncp}s to the corresponding effect sizes.
#' }
#'
#' @examples
#' ## t Tests
#' res <- t.test(1:10, y = c(7:20), var.equal = TRUE)
#' t_to_d(t = res$statistic, res$parameter)
#' t_to_r(t = res$statistic, res$parameter)
#'
#' res <- with(sleep, t.test(extra[group == 1], extra[group == 2], paired = TRUE))
#' t_to_d(t = res$statistic, res$parameter, pooled = TRUE)
#' t_to_r(t = res$statistic, res$parameter)
#'
#' res <- cor.test(iris$Sepal.Width, iris$Petal.Width)
#' t_to_r(t = res$statistic, n = 150)
#'
#' \donttest{
#' ## Linear Regression
#' model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
#' library(parameters)
#' (param_tab <- parameters(model))
#' # > Parameter    | Coefficient |   SE |       95% CI |     t |  df |      p
#' # > -----------------------------------------------------------------------
#' # > (Intercept)  |        2.25 | 0.25 | [1.76, 2.74] |  9.07 | 147 | < .001
#' # > Sepal.Width  |        0.60 | 0.07 | [0.46, 0.73] |  8.59 | 147 | < .001
#' # > Petal.Length |        0.47 | 0.02 | [0.44, 0.51] | 27.57 | 147 | < .001
#'
#' t_to_r(param_tab$t[2:3], param_tab$df_error[2:3])
#' # > [1] 0.5781005 0.9153894
#'
#' ## Use with emmeans based contrasts (see also t_to_eta2)
#' if (require(emmeans) & require(dplyr)) {
#'   warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
#'
#'   emmeans(warp.lm,  ~ tension | wool) %>%
#'     contrast("pairwise") %>%
#'     summary() %>%
#'     bind_cols(t_to_d(.$t.ratio, .$df))
#' }
#'
#' # How does this compare to actual partial correlations?
#' if (require("ppcor")) {
#'   pcor(iris[1:3])$estimate[1, -1]
#' }
#' }
#' @references
#' \itemize{
#'   \item Friedman, H. (1982). Simplified determinations of statistical power, magnitude of effect and research sample sizes. Educational and Psychological Measurement, 42(2), 521-526. \doi{10.1177/001316448204200214}
#'   \item Wolf, F. M. (1986). Meta-analysis: Quantitative methods for research synthesis (Vol. 59). Sage.
#'   \item Rosenthal, R. (1991). Meta-analytic procedures for social research. Newbury Park, CA: SAGE Publications, Incorporated.
#'   \item Steiger, J. H. (2004). Beyond the F test: Effect size confidence intervals and tests of close fit in the analysis of variance and contrast analysis. Psychological Methods, 9, 164-182.
#'   \item Cumming, G., & Finch, S. (2001). A primer on the understanding, use, and calculation of confidence intervals that are based on central and noncentral distributions. Educational and Psychological Measurement, 61(4), 532-574.
#' }
#'
#' @export
t_to_r <- function(t, df_error, CI = 0.95, ...) {

  res <- data.frame(r = t / sqrt(t^2 + df_error))

  if (is.numeric(CI)) {
    stopifnot(length(CI) == 1, CI < 1, CI > 0)
    res$CI <- CI

    ts <- t(mapply(.get_ncp_t,
                   t, df_error, CI))

    res$CI_low <- ts[,1] / sqrt(ts[,1]^2 + df_error)
    res$CI_high <- ts[,2] / sqrt(ts[,2]^2 + df_error)
  }

  return(res)
}

#' @rdname t_to_r
#' @export
convert_t_to_r <- t_to_r

# z -----------------------------------------------------------------------



#' @rdname t_to_r
#' @export
z_to_r <- function(z, n, CI = 0.95, ...) {

  res <- data.frame(r = z / sqrt(z^2 + n))

  if (is.numeric(CI)) {
    stopifnot(length(CI) == 1, CI < 1, CI > 0)
    res$CI <- CI

    alpha <- 1 - CI
    probs <- c(alpha / 2, 1 - alpha / 2)

    qs <- qnorm(probs)
    zs <- cbind(qs[1] + z, qs[2] + z)

    res$CI_low <- zs[,1] / sqrt(zs[,1]^2 + n)
    res$CI_high <- zs[,2] / sqrt(zs[,2]^2 + n)
  }

  return(res)
}

#' @rdname t_to_r
#' @export
convert_z_to_r <- z_to_r

# F -----------------------------------------------------------------------

#' @rdname t_to_r
#' @export
F_to_r <- function(f, df, df_error = NULL, n = NULL, CI = 0.95, ...) {
  if (df > 1) {
    stop("Cannot convert F with more than 1 df to r.")
  }
  t_to_r(sqrt(f), n = n, df_error = df_error, CI = CI)
}

#' @rdname t_to_r
#' @export
convert_F_to_r <- F_to_r



