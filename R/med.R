#' Most Extreme Differences
#'
#' Compute the Most Extreme Differences effect sizes between two ECDFs. Pair
#' with any reported Kolmogorov-Smirnov test ([`stats::ks.test()`]).
#'
#' @inheritParams cohens_d
#' @param pdf a character string naming a cumulative distribution function or an
#'   actual cumulative distribution function such as `pnorm`. Only continuous
#'   CDFs are valid
#' @param alternative a character string specifying the alternative hypothesis.
#'   This _does not_ control the type of CI returned, but the effect calculated;
#'   see [`stats::ks.test()`] for more details.
#' @param ... for the default method, parameters of the distribution specified
#'   (as a character string) by `pdf`.
#'
#' @details Something...
#'
#' @inheritSection effectsize_CIs CIs and Significance Tests
#' @inheritSection print.effectsize_table Plotting with `see`
#'
#' @return A data frame with the effect size `D` and its CI (`CI_low` and
#'   `CI_high`).
#'
#' @family standardized differences
#'
#' @examples
#' x <- rnorm(50)
#' y <- runif(30)
#'
#' # Do x and y come from the same distribution?
#' ks.test(x, y)
#' med(x, y)
#'
#' # Is the CDS of x larger than that of y?
#' ks.test(x, y, alternative = "l")
#' med(x, y, alternative = "l")
#'
#' plot(ecdf(x), xlim = range(c(x, y)), col = "blue")
#' lines(ecdf(y), col = "red")
#'
#' # Does x come from a shifted gamma distribution with shape 3 and rate 2?
#' ks.test(x+2, "pgamma", 3, 2)
#' med(x+2, pdf = "pgamma", shape = 3, rate = 2)
#'
#' plot(ecdf(x + 2), xlim = range(x + 2), col = "blue")
#' curve(pgamma(x, 3, 2), col = "red", add = TRUE)
#'
#'
#' @export
med <- function(
    x,
    y = NULL,
    data = NULL,
    pdf,
    mu = 0,
    reference = NULL,
    ci = 0.95,
    alternative = "two.sided",
    verbose = TRUE,
    ...
) {
  # if (.is_htest_of_type(x, "t-test")) {
  #   return(effectsize(x, type = type, verbose = verbose, data = data, ...))
  # } else if (
  #   .is_BF_of_type(x, c("BFoneSample", "BFindepSample"), "t-squared")
  # ) {
  #   return(effectsize(x, ci = ci, verbose = verbose, ...))
  # }

  alternative <- .match.alt(alternative)
  out <- .get_data_2_samples(
    x,
    y,
    data,
    reference = reference,
    verbose = verbose,
    ...
  )
  x <- out[["x"]]
  y <- out[["y"]]

  test <- stats::ks.test(x - mu, y %||% pdf, alternative = alternative, ...)

  out <- data.frame(D = unname(test$statistic))

  if (.test_ci(ci)) {
    # Add cis
    out$CI <- ci
    ci_level <- .adjust_ci(ci, alternative)
    alpha <- 1 - ci_level

    if (is.null(y)) {
      n <- length(x)
      s <- sqrt(n)
    } else {
      n1 <- length(x)
      n2 <- length(y)
      s <- sqrt(n1 * n2 / (n1 + n2))
    }

    conf_int <- out[["D"]] + c(-1, 1) * stats::qnorm(1 - alpha / 2) / s
    out$CI_low <- pmax(conf_int[1], 0)
    out$CI_high <- pmin(conf_int[2], 1)
    ci_method <- list(method = "normal")
  } else {
    ci_method <- alternative <- NULL
  }

  class(out) <- c(
    "effectsize_difference",
    "effectsize_table",
    "see_effectsize_table",
    class(out)
  )
  .someattributes(out) <- .nlist(
    ci,
    mu,
    ci_method,
    alternative,
    approximate = FALSE
  )
  out
}

