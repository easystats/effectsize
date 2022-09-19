#' Effect Size for Rank Based ANOVA
#'
#' Compute rank epsilon squared (\eqn{\varepsilon^2}{\epsilon^2}) (to accompany
#' [stats::kruskal.test()]), and Kendall's *W* (to accompany
#' [stats::friedman.test()]) effect sizes for non-parametric (rank sum) one-way
#' ANOVAs.
#'
#' @inheritParams phi
#' @inheritParams rank_biserial
#' @param x Can be one of:
#'   - A numeric vector, or a character name of one in `data`.
#'   - A list of vectors (for `rank_epsilon_squared()`).
#'   - A matrix of `blocks x groups` (for `kendalls_w()`) (or `groups x blocks`
#'   if `blocks_on_rows = FALSE`). See details for the `blocks` and `groups`
#'   terminology used here.
#'   - A formula in the form of:
#'       - `DV ~ groups` for `rank_epsilon_squared()`.
#'       - `DV ~ groups | blocks` for `kendalls_w()` (See details for the
#'       `blocks` and `groups` terminology used here).
#' @param groups,blocks A factor vector giving the group / block for the
#'   corresponding elements of `x`, or a character name of one in `data`.
#'   Ignored if `x` is not a vector.
#' @param blocks_on_rows Are blocks on rows (`TRUE`) or columns (`FALSE`).
#' @param iterations The number of bootstrap replicates for computing confidence
#'   intervals. Only applies when `ci` is not `NULL`.
#'
#'
#' @details
#' The rank epsilon squared is appropriate for non-parametric tests of
#' differences between 2 or more samples (a rank based ANOVA). See
#' [stats::kruskal.test]. Values range from 0 to 1, with larger values
#' indicating larger differences between groups.
#' \cr\cr
#' Kendall's *W* is appropriate for non-parametric tests of differences between
#' 2 or more dependent samples (a rank based rmANOVA), where each `group` (e.g.,
#' experimental condition) was measured for each `block` (e.g., subject). This
#' measure is also common as a measure of reliability of the rankings of the
#' `groups` between raters (`blocks`). See [stats::friedman.test]. Values range
#' from 0 to 1, with larger values indicating larger differences between groups
#' / higher agreement between raters.
#'
#' # Confidence (Compatibility) Intervals (CIs)
#' Confidence intervals for rank Epsilon squared, and Kendall's *W* are
#' estimated using the bootstrap method (using the `{boot}` package).
#'
#' @inheritSection effectsize_CIs CIs and Significance Tests
#' @inheritSection effectsize_CIs Bootstrapped CIs
#' @inheritSection rank_biserial Ties
#'
#'
#' @return A data frame with the effect size `(`rank_epsilon_squared` or
#'   `Kendalls_W`) and its CI (`CI_low` and `CI_high`).
#'
#' @family effect size indices
#' @seealso [rank_biserial()] for more rank based effect sizes
#'
#' @examples
#' \donttest{
#' # Rank Epsilon Squared
#' # ====================
#'
#' rank_epsilon_squared(mpg ~ cyl, data = mtcars)
#'
#'
#'
#' # Kendall's W
#' # ===========
#' dat <- data.frame(
#'   cond = c("A", "B", "A", "B", "A", "B"),
#'   ID = c("L", "L", "M", "M", "H", "H"),
#'   y = c(44.56, 28.22, 24, 28.78, 24.56, 18.78)
#' )
#' (W <- kendalls_w(y ~ cond | ID, data = dat, verbose = FALSE))
#'
#' interpret_kendalls_w(0.11)
#' interpret(W, rules = "landis1977")
#' }
#'
#' @references
#' - Kendall, M.G. (1948) Rank correlation methods. London: Griffin.
#'
#' @export
#' @importFrom stats na.omit
#' @importFrom insight check_if_installed
rank_epsilon_squared <- function(x,
                                 groups,
                                 data = NULL,
                                 ci = 0.95,
                                 alternative = "greater",
                                 iterations = 200,
                                 ...) {
  alternative <- match.arg(alternative, c("greater", "two.sided", "less"))

  if (inherits(x, "htest")) {
    if (!grepl("Kruskal-Wallis", x$method)) {
      stop("'x' is not a Kruskal-Wallis-test!", call. = FALSE)
    }
    return(effectsize(x, ci = ci, iterations = iterations, alternative = alternative))
  }

  ## pep data
  data <- .get_data_multi_group(x, groups, data, ...)
  data <- stats::na.omit(data)

  ## compute
  out <- data.frame(rank_epsilon_squared = .repsilon(data))

  ## CI
  ci_method <- NULL
  if (is.numeric(ci)) {
    if (insight::check_if_installed("boot", "for estimating CIs", stop = FALSE)) {
      out <- cbind(out, .repsilon_ci(data, ci, alternative, iterations))
      ci_method <- list(method = "percentile bootstrap", iterations = iterations)
    } else {
      ci <- NULL
    }
  }
  if (is.null(ci)) alternative <- NULL

  class(out) <- c("effectsize_table", "see_effectsize_table", class(out))
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- alternative
  return(out)
}

#' @rdname rank_epsilon_squared
#' @export
#' @importFrom stats na.omit
#' @importFrom insight check_if_installed
kendalls_w <- function(x,
                       groups,
                       blocks,
                       data = NULL,
                       blocks_on_rows = TRUE,
                       ci = 0.95,
                       alternative = "greater",
                       iterations = 200,
                       verbose = TRUE,
                       ...) {
  alternative <- match.arg(alternative, c("greater", "two.sided", "less"))

  if (inherits(x, "htest")) {
    if (!grepl("Friedman", x$method)) {
      stop("'x' is not a Friedman-test!", call. = FALSE)
    }
    return(effectsize(x, ci = ci, iterations = iterations, verbose = verbose, alternative = alternative))
  }

  ## prep data
  if (is.matrix(x) && !blocks_on_rows) x <- t(x)
  data <- .get_data_nested_groups(x, groups, blocks, data, ...)
  data <- stats::na.omit(data)

  ## compute
  W <- .kendalls_w(data, verbose = verbose)
  out <- data.frame(Kendalls_W = W)

  ## CI
  ci_method <- NULL
  if (is.numeric(ci)) {
    if (insight::check_if_installed("boot", "for estimating CIs", stop = FALSE)) {
      out <- cbind(out, .kendalls_w_ci(data, ci, alternative, iterations))
      ci_method <- list(method = "percentile bootstrap", iterations = iterations)
    } else {
      ci <- NULL
    }
  }
  if (is.null(ci)) alternative <- NULL

  class(out) <- c("effectsize_table", "see_effectsize_table", class(out))
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- alternative
  return(out)
}

# rank_eta_squared <- function(x, g, data = NULL, ci = 0.95, iterations = 200) {
#
#   data <- .get_data_multi_group(x, g, data)
#   data <- stats::na.omit(data)
#   x <- data$x
#   g <- data$g
#
#   model <- stats::kruskal.test(x, g)
#
#   H <- unname(model$statistic)
#   k <- length(unique(g)) # model$parameter + 1
#   n <- length(g)
#
#   E <- (H - k + 1) / (n - k)
#
#   out <- data.frame(rank_eta_squared = E)
#
#   if (is.numeric(ci)) {
#     warning("Nope. Not yet.", call. = FALSE)
#     out$CI <- ci
#     out$CI_low <- 0
#     out$CI_high <- 1
#   }
#
#   class(out) <- c("effectsize_table", class(out))
#   return(out)
# }

# Utils -------------------------------------------------------------------

## Get ----

#' @keywords internal
#' @importFrom stats kruskal.test
.repsilon <- function(data) {
  model <- stats::kruskal.test(data$x, data$groups)

  H <- unname(model$statistic)
  n <- nrow(data)

  E <- H / ((n^2 - 1) / (n + 1))
}


#' @keywords internal
.kendalls_w <- function(data, verbose) {
  rankings <- apply(data, 1, .safe_ranktransform, verbose = verbose)
  rankings <- t(rankings) # keep dims

  n <- ncol(rankings) # items
  m <- nrow(rankings) # judges
  R <- colSums(rankings)

  no_ties <- apply(rankings, 1, function(x) length(x) == insight::n_unique(x))
  if (!all(no_ties)) {
    if (verbose) {
      warning(
        sprintf(
          "%d block(s) contain ties%s.",
          sum(!no_ties),
          ifelse(any(apply(as.data.frame(rankings)[!no_ties, ], 1, insight::n_unique) == 1),
                 ", some containing only 1 unique ranking", ""
          )
        ),
        call. = FALSE
      )
    }

    Tj <- 0
    for (i in seq_len(m)) {
      rater <- table(rankings[i, ])
      ties <- rater[rater > 1]
      l <- as.numeric(ties)
      Tj <- Tj + sum(l^3 - l)
    }

    W <- (12 * sum(R^2) - 3 * (m^2) * n * ((n + 1)^2)) /
      (m^2 * (n^3 - n) - m * Tj)
  } else {
    S <- var(R) * (n - 1)
    W <- (12 * S) /
      (m^2 * (n^3 - n))
  }
  W
}

## CI ----



#' @importFrom utils tail
#' @keywords internal
.repsilon_ci <- function(data, ci, alternative, iterations) {
  stopifnot(length(ci) == 1, ci < 1, ci > 0)
  ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1


  boot_r_epsilon <- function(.data, .i) {
    split(.data$x, .data$groups) <- lapply(split(.data$x, .data$groups),
                                           sample,
                                           replace = TRUE
    )
    .repsilon(.data)
  }

  R <- boot::boot(
    data = data,
    statistic = boot_r_epsilon,
    R = iterations
  )

  bCI <- boot::boot.ci(R, conf = ci.level, type = "perc")$percent
  bCI <- tail(as.vector(bCI), 2)

  data.frame(
    CI = ci,
    CI_low = if (alternative == "less") 0 else bCI[1],
    CI_high = if (alternative == "greater") 1 else bCI[2]
  )
}

#' @importFrom utils tail
#' @keywords internal
.kendalls_w_ci <- function(data, ci, alternative, iterations) {
  stopifnot(length(ci) == 1, ci < 1, ci > 0)
  ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1

  boot_w <- function(.data, .i) {
    .kendalls_w(.data[.i, ], verbose = FALSE) # sample rows
  }

  R <- boot::boot(
    data = data,
    statistic = boot_w,
    R = iterations
  )

  bCI <- boot::boot.ci(R, conf = ci.level, type = "perc")$percent
  bCI <- tail(as.vector(bCI), 2)

  data.frame(
    CI = ci,
    CI_low = if (alternative == "less") 0 else bCI[1],
    CI_high = if (alternative == "greater") 1 else bCI[2]
  )
}
