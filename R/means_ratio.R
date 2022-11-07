#' Ratio of Means
#'
#' @description Computes ratio of two means, which is also known as the "response ratio" (RR)
#' and referred to as the Means_Ratio in the output of this function.
#' Pair with any reported [`stats::t.test()`].
#'
#' @param paired If `TRUE`, the values of `x` and `y` are considered as paired.
#' The correlation between these variables will affect the CIs.
#' @param adjust Should the effect size be bias-corrected? Defaults to `TRUE`;
#'   Advisable for small samples.
#' @inheritParams chisq_to_phi
#' @inheritParams cohens_d
#' @note The bias corrected response ratio reported from this function is derived from Lajeunesse (2015).
#'
#' @details
#'  Unlike other effect sizes, there is no difference between groups
#' (equality of means) when the response ratio is equal to 1.
#' The log is taken of the ratio of means,
#' which makes this outcome measure symmetric around 0 and
#' yields a corresponding sampling distribution that is closer to normality.
#' Hence, this measure cannot be computed when the means have opposite signs.
#' The response ratio is only meant to be used for ratio scale measurements,
#' where both means are positive.
#'
#' Unlike other functions in `effectsize`, the response ratio confidence intervals
#' a calculated using the methods described by Lajeunesse (2011 & 2015) using the
#' log-ratio standard error assuming a normal distribution.
#'
#' @inheritSection effectsize_CIs CIs and Significance Tests
#'
#' @return A data frame with the effect size ( `Means_Ratio`) and their CIs (`CI_low` and `CI_high`).
#'
#'
#' @examples
#' \donttest{
#' data(mtcars)
#' }
#'
#' @references
#' Lajeunesse, M. J. (2011). On the meta‐analysis of response ratios for studies with correlated and multi‐group designs. Ecology, 92(11), 2049-2055 .https://doi.org/10.1890/11-0423.1
#'
#' Lajeunesse, M. J. (2015). Bias and correction for the log response ratio in ecological meta‐analysis. Ecology, 96(8), 2056-2063. https://doi.org/10.1890/14-2402.1
#'
#' Hedges, L. V., Gurevitch, J., & Curtis, P. S. (1999). The meta-analysis of response ratios in experimental ecology. Ecology, 80(4), 1150–1156. https://doi.org/10.1890/0012-9658(1999)080[1150:TMAORR]2.0.CO;2
#' @importFrom stats var model.frame
#' @export

means_ratio <- function(x,
                y = NULL,
                data = NULL,
                ci = 0.95,
                alternative = c("two.sided", "less", "greater"),
                paired = FALSE,
                verbose = TRUE,
                adjust = TRUE,
                ...) {
  alternative <- match.arg(alternative)

  if (!is.numeric(ci)) {
    ci <- NULL
  }
  if(is.numeric(ci)){
    if(!((length(ci) == 1L)
         && is.finite(ci)
         && (ci > 0)
         && (ci < 1)))
      stop("'ci' must be a single number between 0 and 1")
    ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1
    alpha <- 1 - ci.level
  }

  if(is.null(data) & is.null(y)){
    stop("Only one sample provided. y or data must be provided.")
  } else if(!is.null(y)) {
    DNAME <- paste(deparse(substitute(x)), "and",
                   deparse(substitute(y)))
  } else {

    DNAME <- gsub("~", "by", Reduce(paste, deparse(x)))
  }
  ## Prep data
  out <- .get_data_2_samples(x=x, y=y, data=data,
                             verbose = TRUE,
                             paired = FALSE,
                             allow_ordered = FALSE,
                             ...)
  x <- out$x
  y <- out$y

  if(is.null(y)){
    stop("Missing y. Only one sample provided.")
  }

  if (!is.numeric(x)) {
    stop("'x' must be numeric")
  }

  if (!is.numeric(y)){
    stop("'y' must be numeric")
  }

  if(any(x <=0) || any(y <= 0)){
    stop("Data cannot have values less than or equal to 1.")
  }
  if(length(x) < 1L)
    stop("not enough (finite) 'x' observations")

  if(paired) { ##------------------ paired case -------------------
    ## Get summary stats ----
    m1 = mean(x)
    sd1 = sd(x)
    n1 = length(x)
    m2 = mean(y)
    sd2 = sd(y)
    n2 = length(y)
    r = cor(x,y)

    if(n1 != n2){
      warning("Paired samples of varying lengths. Results likely bogus.")
    }

    ## Degrees of freedom ------
    df1 = min(c(n1,n2))

    ## Calc log RR ------
    log_val = .logrom_calc(
      paired = TRUE,
      m1 = m1,
      sd1 = sd1,
      n1 = n1,
      m2 = m2,
      sd2 = sd2,
      n2 = n2,
      r = r,
      adjust = adjust
    )

  } else { ##------------------------ 2-sample case -------------------------
    if(length(y) < 1L)
      stop("not enough 'y' observations")
    ## summary statistics ----
    m1 = mean(x)
    sd1 = sd(x)
    n1 = length(x)
    m2 = mean(y)
    sd2 = sd(y)
    n2 = length(y)
    # ri = cor(x,y)
    ## degrees of freedom ----
    df1 = n1+n2-2

    ## Calc log RR -----
    log_val = .logrom_calc(
      paired = FALSE,
      m1 = m1,
      sd1 = sd1,
      n1 = n1,
      m2 = m2,
      sd2 = sd2,
      n2 = n2,
      r = NULL,
      adjust = adjust
    )


  }

  SE <- sqrt(log_val$var_rom)
  rom = exp(log_val$log_rom)

  out = data.frame(Means_Ratio = rom)

  if(is.numeric(ci)){
    # Normal approx ------
    interval <- exp(log_val$log_rom + c(-1, 1) * qnorm(1 - alpha / 2) * SE)

    # Central t method ------
    #interval <- exp(log_val$log_rom + c(-1, 1) * qt(1 - alpha / 2,df1) * SE)

    interval = switch(alternative,
                      "two.sided" = interval,
                      "less" = c(-Inf,interval[2]),
                      "greater" = c(interval[1],+Inf))
    out$ci = ci
    out$CI_low <- interval[1]
    out$CI_high <- interval[2]
    ci_method <- list(method = "normal", distribution = "log-normal")
  }

  class(out) <- c("effectsize_difference",
                  "effectsize_table",
                  "see_effectsize_table", class(out))
  .someattributes(out) <- .nlist(
    paired, ci, ci_method, alternative,
    mu = 0,
    approximate = TRUE
  )
  return(out)

}


#' @importFrom stats sd
#' @keywords internal

.logrom_calc = function(paired = FALSE,
                        m1,
                        sd1,
                        n1,
                        m2,
                        sd2,
                        n2,
                        r = NULL,
                        adjust = TRUE) {
  if (!paired) {
    yi <- log(m1 / m2)
    ### large sample approximation to the sampling variance (does not assume homoscedasticity)
    vi <- sd1^2 / (n1 * m1^2) + sd2^2 / (n2 * m2 ^2)
  }

  if (paired) {
    yi <- log(m1 / m2)
    vi <- sd1^2 / (n1 * m1^2) + sd2^2 / (n1 * m2^2) - 2 * r * sd1 *
      sd2 / (m1 * m2 * n1)

  }

  if(adjust == TRUE){
    J = 0.5 * (sd1 ^ 2 / (n1 * m1 ^ 2) - sd2 ^ 2 / (n2 * m2 ^ 2))
    yi = yi + J

    Jvar = 0.5 * (sd1^4 / (n1^2 * m1^4) - sd2^4 / (n2^2 * m2^4))
    vi = vi + Jvar
  }


  rval = list(
    log_rom = yi,
    var_rom = vi
  )
  return(rval)
}
