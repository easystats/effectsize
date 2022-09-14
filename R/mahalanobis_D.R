#' Compute Mahalanobis' D (multivariate Cohen's d)
#'
#' Compute effect size indices for standardized difference between two normal
#' multivariate distributions or between one multivariate distribution and a
#' defined point. This is the standardized effect size for Hotelling's \eqn{T^2}
#' test. *D* is computed as:
#' \cr\cr
#' \deqn{D = \sqrt{(\bar{X}_1-\bar{X}_2-\mu)^T \Sigma_p^{-1} (\bar{X}_1-\bar{X}_2-\mu)}}
#' \cr\cr
#' Where \eqn{\bar{X}_i} are the column means, \eqn{\Sigma_p} is the *pooled*
#' covariance matrix, and \eqn{\mu} is a vector of the null differences for each
#' variable. When there is only one variate, this formula reduces to Cohen's
#' *d*.
#'
#' @inheritParams cohens_d
#' @param x,y A data frame or matrix. Any incomplete observations (with `NA`
#'   values) are dropped. `x` can also be a formula (see details) in which case
#'   `y` is ignored.
#' @param pooled_cov Should equal covariance be assumed? Currently only
#'   `pooled_cov = TRUE` is supported.
#' @param mu A named list/vector of the true difference in means for each
#'   variable. Can also be a vector of length 1, which will be recycled.
#' @param ... Not used.
#'
#' @inheritSection effectsize_CIs Confidence (Compatibility) Intervals (CIs)
#' @inheritSection effectsize_CIs CIs and Significance Tests
#'
#' @details
#' To specify a `x` as a formula:
#' - Two sample case: `DV1 + DV2 ~ group` or `cbind(DV1, DV2) ~ group`
#' - One sample case: `DV1 + DV2 ~ 1` or `cbind(DV1, DV2) ~ 1`
#'
#' @return A data frame with the `Mahalanobis_D` and potentially its CI
#'   (`CI_low` and `CI_high`).
#'
#' @seealso [cohens_d()], [cov_pooled()], [stats::mahalanobis()]
#' @family effect size indices
#'
#' @references
#' - Del Giudice, M. (2017). Heterogeneity coefficients for Mahalanobis' D as a multivariate effect size. Multivariate Behavioral Research, 52(2), 216-221.
#' - Mahalanobis, P. C. (1936). On the generalized distance in statistics. National Institute of Science of India.
#' - Reiser, B. (2001). Confidence intervals for the Mahalanobis distance. Communications in Statistics-Simulation and Computation, 30(1), 37-45.
#'
#' @examples
#' ## Two samples --------------
#' mtcars_am0 <- subset(mtcars, am == 0,
#'                      select = c(mpg, hp, cyl))
#' mtcars_am1 <- subset(mtcars, am == 1,
#'                      select = c(mpg, hp, cyl))
#'
#' mahalanobis_d(mtcars_am0, mtcars_am1)
#'
#' # Or
#' mahalanobis_d(mpg + hp + cyl ~ am, data = mtcars)
#'
#' mahalanobis_d(mpg + hp + cyl ~ am, data = mtcars, alternative = "greater")
#'
#' # Different mu:
#' mahalanobis_d(mpg + hp + cyl ~ am, data = mtcars,
#'               mu = c(mpg = -4, hp = 15, cyl = 0))
#'
#'
#' # D is a multivariate d, so when only 1 variate is provided:
#' mahalanobis_d(hp ~ am, data = mtcars)
#'
#' cohens_d(hp ~ am, data = mtcars)
#'
#'
#' # One sample ---------------------------
#' mahalanobis_d(mtcars[,c("mpg", "hp", "cyl")])
#'
#' # Or
#' mahalanobis_d(mpg + hp + cyl ~ 1, data = mtcars,
#'               mu = c(mpg = 15, hp = 5, cyl = 3))
#'
#' @export
mahalanobis_d <- function(x, y = NULL, data = NULL,
                          pooled_cov = TRUE, mu = 0,
                          ci = 0.95,
                          alternative = "two.sided",
                          verbose = TRUE,
                          ...) {
  # TODO add one sample case DV1 + DV2 ~ 1
  # TODO add paired samples case DV1 + DV2 ~ 1 | ID
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))
  data <- .get_data_multivariate(x, y, data, verbose = verbose)
  x <- data[["x"]]
  y <- data[["y"]]
  if (verbose && (anyNA(x) || anyNA(y))) {
    warning("Missing values detected. NAs dropped.", call. = FALSE)
  }
  x <- na.omit(x)
  y <- na.omit(y)


  # deal with mu
  if (is.vector(mu)) {
    if (length(mu) == 1L) {
      mu <- rep(mu, length.out = ncol(x))
      names(mu) <- colnames(x)
    } else if (length(mu) != ncol(x) || is.null(names(mu))) {
      stop("mu must be of length 1 or a named vector/list of length ncol(x).", call. = FALSE)
    }

    mu <- as.list(mu)
  }

  if (!is.list(mu)) {
    stop("mu must be of length 1 or a named vector/list of length ncol(x).", call. = FALSE)
  } else if (!all(names(mu) == colnames(x))) {
    stop("x,y must have the same variables (in the same order)", call. = FALSE)
  } else if (!all(lengths(mu) == 1L) || !all(sapply(mu, is.numeric))) {
    stop("Each element of mu must be a numeric vector of length 1.", call. = FALSE)
  }
  mu <- unlist(mu)

  if (is.null(y)) {
    d <- colMeans(x) - mu

    COV <- cov(x)

    n <- nrow(x)
    p <- ncol(x)

    hn <- n
    df <- n - p
    ff <- hn * (df / (p * (n - 1)))
  } else {
    d <- colMeans(x) - colMeans(y) - mu

    if (!pooled_cov) {
      warning("Non-pooled cov not supported.", call. = FALSE)
    }
    COV <- cov_pooled(x, y, verbose = verbose)

    n1 <- nrow(x)
    n2 <- nrow(y)
    p <- ncol(x)

    hn <- (n1 * n2) / (n1 + n2)
    df <- (n1 + n2 - p - 1)
    ff <- hn * (df / (p * (n1 + n2 - 2)))
  }

  out <- data.frame(Mahalanobis_D = sqrt(t(d) %*% solve(COV) %*% d))

  ci_method <- NULL
  if (is.numeric(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)

    # Add cis
    out$CI <- ci
    ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1

    f <- ff * out[[1]]^2

    fs <- .get_ncp_F(f, p, df, ci.level) * p

    out$CI_low <- sqrt(fs[1] / hn)
    out$CI_high <- sqrt(fs[2] / hn)
    ci_method <- list(method = "ncp", distribution = "F")
    if (alternative == "less") {
      out$CI_low <- 0
    } else if (alternative == "greater") {
      out$CI_high <- Inf
    }
  } else {
    alternative <- NULL
  }

  class(out) <- c("effectsize_difference", "effectsize_table", "see_effectsize_table", class(out))
  .someattributes(out) <- .nlist(
    pooled_cov, mu = sqrt(sum(mu^2)),
    ci, ci_method, alternative,
    approximate = FALSE
  )
  return(out)
}

