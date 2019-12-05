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
#' @param ... Arguments passed to or from other methods.
#'
#' @return A numeric value of the requested effect size.
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
#' }
#'
#' # How does this compare to actual partial correlations?
#' library(ppcor)
#' pcor(iris[1:3])$estimate[1, -1]
#'
#' @references
#' \itemize{
#'   \item Friedman, H. (1982). Simplified determinations of statistical power, magnitude of effect and research sample sizes. Educational and Psychological Measurement, 42(2), 521-526. \doi{10.1177/001316448204200214}
#'   \item Wolf, F. M. (1986). Meta-analysis: Quantitative methods for research synthesis (Vol. 59). Sage.
#'   \item Rosenthal, R. (1991). Meta-analytic procedures for social research. Newbury Park, CA: SAGE Publications, Incorporated.
#' }
#'
#' @export
t_to_r <- function(t, n = NULL, df_error = NULL, ...) {
  if(is.null(df_error) & !is.null(n)){
    df_error <- n - 2
  }
  t / sqrt(t^2 + df_error)
}


#' @rdname t_to_r
#' @export
r_to_t <- function(r, n = NULL, df_error = NULL, ...){
  if(is.null(df_error) & !is.null(n)){
    df_error <- n - 2
  }
  sign(r) * sqrt(-(r^2 * df_error) / (r^2 - 1))
}


# z -----------------------------------------------------------------------



#' @rdname t_to_r
#' @export
z_to_r <- function(z, n = NULL, df_error = NULL, ...) {
  if(is.null(n) & !is.null(df_error)){
    n <- df_error + 2
  }
  z / sqrt(z^2 + n)
}



#' @rdname t_to_r
#' @export
r_to_z <- function(r, n = NULL, df_error = NULL, ...) {
  if(is.null(n) & !is.null(df_error)){
    n <- df_error + 2
  }
  sign(r) * sqrt(-(r^2 * n) / (r^2 - 1))
}

# F -----------------------------------------------------------------------



#' @rdname t_to_r
#' @export
F_to_r <- function(f, df, df_error = NULL, n = NULL, ...) {
  if (df > 1) {
    stop("Cannot convert F with more than 1 df to r.")
  }
  t_to_r(sqrt(f), n = n, df_error = df_error)
}



# Aliases -----------------------------------------------------------------

#' @rdname t_to_r
#' @export
convert_t_to_r <- t_to_r

#' @rdname t_to_r
#' @export
convert_r_to_t <- r_to_t



#' @rdname t_to_r
#' @export
convert_z_to_r <- z_to_r

#' @rdname t_to_r
#' @export
convert_r_to_z <- r_to_z




#' @rdname t_to_r
#' @export
convert_F_to_r <- F_to_r
