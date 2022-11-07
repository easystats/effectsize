#' Ratio of Means
#'
#' Computes the ratio of two means (also known as the "response ratio"; RR) of
#' **variables on a ratio scale** (with an absolute 0). Pair with any reported
#' [`stats::t.test()`].
#'
#' @param paired If `TRUE`, the values of `x` and `y` are considered as paired.
#'   The correlation between these variables will affect the CIs.
#' @param adjust Should the effect size be bias-corrected? Defaults to `TRUE`;
#'   Advisable for small samples.
#' @param log Should the log-ratio be returned? Defaults to `FALSE`.
#'  Normally distributed and useful for meta-analysis.
#' @inheritParams chisq_to_phi
#' @inheritParams cohens_d
#'
#' @details
#' The Means Ratio ranges from 0 to \eqn{\infty}, with values smaller than 1
#' indicating that the second mean is larger than the first, values larger than
#' 1 indicating that the second mean is smaller than the first, and values of 1
#' indicating that the means are equal.
#'
#' # Confidence (Compatibility) Intervals (CIs)
#' Confidence intervals are estimated as described by Lajeunesse (2011 & 2015)
#' using the log-ratio standard error assuming a normal distribution. By this
#' method, the log is taken of the ratio of means, which makes this outcome
#' measure symmetric around 0 and yields a corresponding sampling distribution
#' that is closer to normality.
#'
#' @inheritSection effectsize_CIs CIs and Significance Tests
#'
#' @return A data frame with the effect size (`Means_ratio` or
#'   `Means_ratio_adjusted`) and their CIs (`CI_low` and `CI_high`).
#'
#' @family standardized differences
#'
#' @note The bias corrected response ratio reported from this function is
#'   derived from Lajeunesse (2015).
#'
#' @examples
#' x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
#' y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
#' means_ratio(x, y)
#'
#' # The ratio is scale invariant, making it a standardized effect size
#' means_ratio(3 * x, 3 * y)
#'
#' @references
#' Lajeunesse, M. J. (2011). On the meta‐analysis of response ratios for studies
#' with correlated and multi‐group designs. Ecology, 92(11), 2049-2055.
#' \doi{10.1890/11-0423.1}
#'
#' Lajeunesse, M. J. (2015). Bias and correction for the log response ratio in
#' ecological meta‐analysis. Ecology, 96(8), 2056-2063. \doi{10.1890/14-2402.1}
#'
#' Hedges, L. V., Gurevitch, J., & Curtis, P. S. (1999). The meta-analysis of
#' response ratios in experimental ecology. Ecology, 80(4), 1150–1156.
#' \doi{10.1890/0012-9658(1999)080[1150:TMAORR]2.0.CO;2}
#'
#' @export
means_ratio <- function(x, y = NULL, data = NULL,
                        paired = FALSE, adjust = TRUE,
                        log = FALSE,
                        ci = 0.95, alternative = "two.sided",
                        verbose = TRUE, ...) {
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))

  ## Prep data
  out <- .get_data_2_samples(
    x = x, y = y, data = data,
    verbose = verbose,
    paired = paired,
    ...
  )
  x <- out$x
  y <- out$y

  if (is.null(y)) {
    stop("Only one sample provided. y or data must be provided.")
  }

  if (any(x < 0) || any(y < 0)) {
    stop("x,y must be non-negative (on a ratio scale).")
  }


  if (paired) {
    ## ------------------ paired case -------------------
    # Get summary stats
    m1 <- mean(x)
    sd1 <- stats::sd(x)
    m2 <- mean(y)
    sd2 <- stats::sd(y)

    if(m1 <= 0 || m2 <=0){
      stop("Means less than or equal zero. Unable to calculate means ratio.")
    }

    df1 <- n <- length(x)
    r <- stats::cor(x, y)

    # Calc log RR
    log_val <- .logrom_calc(
      paired = TRUE,
      m1 = m1,
      sd1 = sd1,
      m2 = m2,
      sd2 = sd2,
      n1 = n,
      r = r,
      adjust = adjust
    )
  } else {
    ## ------------------------ 2-sample case -------------------------
    # summary statistics
    m1 <- mean(x)
    sd1 <- stats::sd(x)
    n1 <- length(x)
    m2 <- mean(y)
    sd2 <- stats::sd(y)
    n2 <- length(y)

    if(m1 <= 0 || m2 <=0){
      stop("Means less than or equal zero. Unable to calculate means ratio.")
    }

    df1 <- n1 + n2 - 2

    # Calc log RR
    log_val <- .logrom_calc(
      paired = FALSE,
      m1 = m1,
      sd1 = sd1,
      n1 = n1,
      m2 = m2,
      sd2 = sd2,
      n2 = n2,
      adjust = adjust
    )
  }

  if(log == FALSE){
  if (adjust) {
    out <- data.frame(Means_ratio_adjusted = exp(log_val$log_rom))
  } else {
    out <- data.frame(Means_ratio = exp(log_val$log_rom))
  }
  }
  if(log == TRUE){
    if (adjust) {
      out <- data.frame(log_Means_ratio_adjusted = (log_val$log_rom))
    } else {
      out <- data.frame(log_Means_ratio = (log_val$log_rom))
    }
  }

  if (is.numeric(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)

    # Add cis
    out$CI <- ci
    ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1
    alpha <- 1 - ci.level

    SE <- sqrt(log_val$var_rom)

    # Normal approx
    if(log == FALSE){
      interval <- exp(log_val$log_rom + c(-1, 1) * stats::qnorm(alpha / 2, lower.tail = FALSE) * SE)
    } else {
      interval <- (log_val$log_rom + c(-1, 1) * stats::qnorm(alpha / 2, lower.tail = FALSE) * SE)
    }

    ci_method <- list(method = "normal")

    # Central t method
    # interval <- exp(log_val$log_rom + c(-1, 1) * stats::qt(alpha / 2, df1, lower.tail = FALSE) * SE)

    out$CI_low <- interval[1]
    out$CI_high <- interval[2]

    if (alternative == "less") {
      out$CI_low <- 0
    } else if (alternative == "greater") {
      out$CI_high <- Inf
    }
  } else {
    ci_method <- alternative <- NULL
  }

  class(out) <- c("effectsize_difference", "effectsize_table", "see_effectsize_table", class(out))
  .someattributes(out) <- .nlist(
    paired, ci, ci_method, alternative,
    mu = 0,
    approximate = TRUE
  )
  return(out)
}


#' @importFrom stats sd
#' @keywords internal

.logrom_calc <- function(paired = FALSE,
                         m1,
                         sd1,
                         n1,
                         m2,
                         sd2,
                         n2 = n1,
                         r = NULL,
                         adjust = TRUE) {
  if (isTRUE(paired)) {
    yi <- log(m1 / m2)
    vi <-
      sd1^2 / (n1 * m1^2) +
      sd2^2 / (n1 * m2^2) -
      2 * r * sd1 * sd2 / (m1 * m2 * n1)
  } else {
    yi <- log(m1 / m2)
    ### large sample approximation to the sampling variance (does not assume homoscedasticity)
    vi <- sd1^2 / (n1 * m1^2) + sd2^2 / (n2 * m2^2)
  }


  if (isTRUE(adjust)) {
    J <- 0.5 * (sd1^2 / (n1 * m1^2) - sd2^2 / (n2 * m2^2))
    yi <- yi + J

    Jvar <- 0.5 * (sd1^4 / (n1^2 * m1^4) - sd2^4 / (n2^2 * m2^4))
    vi <- vi + Jvar
  }


  list(
    log_rom = yi,
    var_rom = vi
  )
}
