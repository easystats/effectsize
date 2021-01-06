#' Effect size for non-parametric (rank sum) tests
#'
#' Compute the rank-biserial correlation, rank Epsilon squared, and Kendall's
#' *W* effect sizes for non-parametric (rank sum) tests.
#'
#' @inheritParams cohens_d
#' @param x Can be one of:
#'   - A numeric vector, or a character name of one in `data`.
#'   - A formula in to form of `x ~ groups` (for `rank_biserial()` and
#'   `rank_epsilon_squared()`) or `x ~ groups | blocks` (for `kendalls_w()`).
#'   - A list of vectors (for `rank_epsilon_squared()`).
#'   - A matrix of `blocks x groups` (for `kendalls_w()`).
#' @param y An optional numeric vector of data values to compare to `x`, or a
#'   character name of one in `data`. Ignored if `x` is not a vector.
#' @param groups A vector or factor object giving the group for the
#'   corresponding elements of `x`, or a character name of one in `data`.
#'   Ignored if `x` is not a vector.
#' @param blocks A vector giving the block for the corresponding elements of
#'   `x`, or a character name of one in `data`. Ignored if `x` is not a vector.
#' @param mu a number indicating the value around which (a-)symmetry (for
#'   one-sample or paired samples) or shift (for independant samples) is to be
#'   estimated. See [stats::wilcox.test].
#'
#' @details
#' Compute effect sizes for non-parametric (rank sum) tests.
#' \cr\cr
#' The rank-biserial correlation is appropriate for non-parametric tests of
#' differences - both for the one sample or paired samples case (that would
#' normally be tested with Wilcoxon's Signed Rank Test) and for two independant
#' samples case (that would normally be tested with Mann-Whitney's *U* Test).
#' See [stats::wilcox.test]. Values range from (`-1`) indicating that all values
#' of the second sample are smaller than the first sample, to (`+1`) indicating
#' that all values of the second sample are larger than the first sample.
#' \cr\cr
#' The rank Epsilon squared is appropriate for non-parametric tests of
#' differences between 2 or more samples (a rank based ANOVA). See
#' [stats::kruskal.test]. Values range from 0 to 1, with larger values
#' indicating larger differences between groups.
#' \cr\cr
#' Kendall's *W* is appropriate for non-parametric tests of differences between
#' 2 or more dependant samples (a rank based rmANOVA). See
#' [stats::friedman.test]. Values range from 0 to 1, with larger values
#' indicating larger differences between groups.
#'
#' # Confidence Intervals
#' Confidence Intervals are estimated using the bootstrap method.
#'
#' @return A data frame with the effect size (`r_rank_biserial`, `Kendalls_W` or
#'   `rank_epsilon_squared`) and its CI (`CI_low` and `CI_high`).
#'
#' @family effect size indices
#'
#' @examples
#' A <- c(48, 48, 77, 86, 85, 85)
#' B <- c(14, 34, 34, 77)
#' rank_biserial(A, B)
#'
#' x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
#' y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
#' rank_biserial(x, y, paired = TRUE)
#'
#' x <- c(1.15, 0.88, 0.90, 0.74, 1.21)
#' rank_biserial(x, mu = 1)
#'
#' x1 <- c(2.9, 3.0, 2.5, 2.6, 3.2) # normal subjects
#' x2 <- c(3.8, 2.7, 4.0, 2.4)      # with obstructive airway disease
#' x3 <- c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis
#' x <- c(x1, x2, x3)
#' g <- factor(rep(1:3, c(5, 4, 5)))
#' rank_epsilon_squared(x, g)
#'
#' wb <- aggregate(warpbreaks$breaks,
#'                 by = list(w = warpbreaks$wool,
#'                           t = warpbreaks$tension),
#'                 FUN = mean)
#' kendalls_w(x ~ w | t, data = wb)
#'
#' @references
#' - Cureton, E. E. (1956). Rank-biserial correlation. Psychometrika, 21(3), 287-290.
#' - Kerby, D. S. (2014). The simple difference formula: An approach to teaching nonparametric correlation. Comprehensive Psychology, 3, 11-IT.
#' - Tomczak, M., & Tomczak, E. (2014). The need to report effect size estimates revisited. An overview of some recommended measures of effect size.
#'
#' @export
rank_biserial <- function(x, y = NULL, data = NULL, mu = 0,
                          ci = 0.95, iterations = 200,
                          paired = FALSE,
                          verbose = TRUE,
                          ...) {
  if (inherits(x, "htest")) {
    if (!grepl("Wilcoxon", x$method))
      stop("'x' is not a Wilcoxon-test!", call. = FALSE)
    return(effectsize(x, ci = ci, iterations = iterations))
  }

  out <- .deal_with_cohens_d_arguments(x, y, data)

  x <- out$x
  y <- out$y

  if (is.null(y)) {
    y <- rep(0, length.out = length(x))
    paired <- TRUE
  }

  if (paired) {
    r_rbs <- .r_rbs_paired(x, y, mu = mu, verbose = verbose)
  } else {
    r_rbs <- .r_rbs_indep(x, y, mu = mu, verbose = verbose)
  }


  out <- data.frame(r_rank_biserial = r_rbs)

  if (is.numeric(ci)) {
    out <- cbind(out, .rbs_ci_boot(
      x,
      y,
      mu = mu,
      paired = paired,
      ci = ci,
      iterations = iterations
    ))
  }

  class(out) <- c("effectsize_difference", "effectsize_table", class(out)) # "see_effectsize_table"
  attr(out, "paired") <- paired
  attr(out, "mu") <- mu
  return(out)
}


#' @rdname rank_biserial
#' @export
#' @importFrom stats na.omit
rank_epsilon_squared <- function(x, groups, data = NULL, ci = 0.95, iterations = 200, ...) {

  if (inherits(x, "htest")) {
    if (!grepl("Kruskal-Wallis", x$method))
      stop("'x' is not a Kruskal-Wallis-test!", call. = FALSE)
    return(effectsize(x, ci = ci, iterations = iterations))
  }

  data <- .rank_anova_xg(x, groups, data)
  data <- stats::na.omit(data)
  x <- data$x
  groups <- data$groups

  E <- .repsilon(x, groups)

  out <- data.frame(rank_epsilon_squared = E)

  if (is.numeric(ci)) {
    out <- cbind(out, .repsilon_ci(data, ci, iterations))
  }

  class(out) <- c("effectsize_table", class(out)) # "see_effectsize_table"
  return(out)
}

#' @rdname rank_biserial
#' @export
#' @importFrom stats na.omit
kendalls_w <- function(x, groups, blocks, data = NULL, ci = 0.95, iterations = 200, ...) {

  if (inherits(x, "htest")) {
    if (!grepl("Friedman", x$method))
      stop("'x' is not a Friedman-test!", call. = FALSE)
    return(effectsize(x, ci = ci, iterations = iterations))
  }

  data <- .kendalls_w_data(x, groups, blocks, data)
  data <- stats::na.omit(data)
  x <- data$x
  groups <- data$groups
  blocks <- data$blocks

  W <- .kendalls_w(x, groups, blocks)

  out <- data.frame(Kendalls_W = W)

  if (is.numeric(ci)) {
    out <- cbind(out, .kendalls_w_ci(data, ci, iterations))
  }

  class(out) <- c("effectsize_table", class(out))
  return(out)
}

# rank_eta_squared <- function(x, g, data = NULL, ci = 0.95, iterations = 200) {
#
#   data <- .rank_anova_xg(x, g, data)
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
#     warning("Nope. Not yet.")
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
#' @importFrom stats na.omit
.r_rbs_paired <- function(x, y, mu, verbose) {
  d <- (x - y) - mu

  r_sign <- ranktransform(d, sign = TRUE, verbose = verbose)
  r_sign <- stats::na.omit(r_sign)

  r_pos <-  sum(r_sign[r_sign > 0])
  r_neg <- -sum(r_sign[r_sign < 0])
  T_ <- min(r_pos, r_neg)
  n <- length(r_sign)

  r_rbs <- 4 * abs(T_ - (r_pos + r_neg) / 2) / (n * (n + 1))
  if (r_pos >= r_neg) r_rbs <- -r_rbs
  r_rbs <- sign(r_rbs) * pmin(abs(r_rbs), 1)
  r_rbs
}

#' @keywords internal
#' @importFrom stats na.omit
.r_rbs_indep <- function(x, y, mu, verbose){
  x <- stats::na.omit(x)
  y <- stats::na.omit(y)

  Ry <- ranktransform(c(x - mu, y), verbose = verbose)
  Group <- c(rep("A", length(x)),
             rep("B", length(y)))

  oo <- rev(order(Ry))
  Ry_sort <- Ry[oo]
  Group_sort <- Group[oo]

  # count inversions
  inv <- numeric(length = length(Ry))
  agr <- numeric(length = length(Ry))

  for (b in seq_along(Ry)) {
    if (Group_sort[b]=="B") {
      inv[b] <- sum(tail(Group_sort == "A" &
                           Ry_sort < Ry_sort[b],
                         -b))
    } else {
      agr[b] <- sum(tail(Group_sort == "B" &
                           Ry_sort < Ry_sort[b],
                         -b))
    }
  }

  Q_ <- sum(inv)
  P_ <- sum(agr)

  # pmax
  Group_pmax <- rev(sort(Group))
  agr_max <- numeric(length = length(Ry))
  for (b in seq_along(Ry)) {
    if (Group_pmax[b]=="B") {
      agr_max[b] <- sum(tail(Group_pmax == "A" &
                               Ry_sort < Ry_sort[b],
                             -b))
    }
  }

  P_max <- sum(agr_max)

  rr <- (P_-Q_)/P_max
  rr <- sign(rr) * pmin(abs(rr), 1)
  rr
}

#' @keywords internal
#' @importFrom stats kruskal.test
.repsilon <- function(x, groups) {
  model <- stats::kruskal.test(x, groups)

  H <- unname(model$statistic)
  n <- length(groups)

  E <- H / ((n^2 - 1)/(n + 1))
}


#' @keywords internal
#' @importFrom stats friedman.test
.kendalls_w <- function(x, groups, blocks) {
  model <- stats::friedman.test(x, groups, blocks)

  Chi <- unname(model$statistic)
  N <- length(unique(groups))
  k <- length(unique(blocks))

  W <- Chi / (N * (k - 1))
}

## CI ----

#' @keywords internal
#' @importFrom bayestestR ci
.rbs_ci_boot <- function(x, y, mu = 0, paired = FALSE, ci = 0.95, iterations = 200) {

  if (!requireNamespace("boot")) {
    warning("For CIs, the 'boot' package must be installed.")
    return(NULL)
  }

  stopifnot(length(ci) == 1, ci < 1, ci > 0)

  if (paired) {
    data <- data.frame(x, y)
    boot_rbs <- function(.data, .i) {
      .data <- .data[.i, ]
      .x <- .data$x
      .y <- .data$y
      suppressWarnings(.r_rbs_paired(.x, .y, mu = mu))
    }
  } else {
    data <- data.frame(
      i = seq_along(c(x, y))
    )

    boot_rbs <- function(.data, .i) {
      .x <- sample(x, replace = TRUE)
      .y <- sample(y, replace = TRUE)

      suppressWarnings(.r_rbs_indep(.x, .y, mu = mu))
    }
  }

  R <- boot::boot(data = data,
                  statistic = boot_rbs,
                  R = iterations)

  out <- as.data.frame(
    bayestestR::ci(na.omit(R$t), ci = ci, verbose = FALSE)
  )
  out$CI <- ci
  out
}

#' @keywords internal
.repsilon_ci <- function(data, ci, iterations){
  if (!requireNamespace("boot")) {
    warning("'boot' package required for estimating CIs for Glass' delta. Please install the package and try again.", call. = FALSE)
    return(NULL)
  }

  stopifnot(length(ci) == 1, ci < 1, ci > 0)

  boot_r_epsilon <- function(.data, .i) {
    split(.data$x, .data$groups) <- lapply(split(.data$x, .data$groups),
                                           sample, replace = TRUE)
    .repsilon(.data$x, .data$groups)
  }

  R <- boot::boot(data = data,
                  statistic = boot_r_epsilon,
                  R = iterations)

  out <- as.data.frame(
    bayestestR::ci(na.omit(R$t), ci = ci, verbose = FALSE)
  )
  out$CI <- ci
  out
}

#' @keywords internal
.kendalls_w_ci <- function(data, ci, iterations) {
  if (!requireNamespace("boot")) {
    warning("'boot' package required for estimating CIs for Glass' delta. Please install the package and try again.", call. = FALSE)
    return(NULL)
  }

  stopifnot(length(ci) == 1, ci < 1, ci > 0)

  boot_w <- function(.data, .i) {
    rp <- sample(split(.data, .data$blocks), replace = TRUE)
    rp <- mapply(function(tmp, L) {
      tmp$blocks <- L
      tmp
    }, rp, factor(seq_along(rp)), SIMPLIFY = FALSE)
    .data <- do.call("rbind", rp)

    .kendalls_w(.data$x, .data$groups, .data$blocks)
  }

  R <- boot::boot(data = data,
                  statistic = boot_w,
                  R = iterations)

  out <- as.data.frame(
    bayestestR::ci(na.omit(R$t), ci = ci, verbose = FALSE)
  )
  out$CI <- ci
  out
}

## data ----


#' @keywords internal
#' @importFrom stats model.frame lm
.rank_anova_xg <- function(x, groups, data) {
  if (inherits(frm <- x, "formula")) {
    if (length(frm) != 3)
      stop("Formula must have the 'outcome ~ group'.", call. = FALSE)

    mf <- stats::model.frame(stats::lm(formula = frm, data = data))

    x <- mf[[1]]
    if (ncol(mf) == 2) {
      groups <- factor(mf[[2]])
    } else {
      stop("Formula must have the 'outcome ~ group'.", call. = FALSE)
    }
  } else if (inherits(x, "list")) {
    groups <- rep(names(x), sapply(x, length))
    x <- unsplit(x, groups)
  } else  if (is.character(x)) {
    x <- data[[x]]
    groups <- data[[groups]]
  } else if (length(x) != length(groups)) {
    stop("x and g must be of the same length.", call. = FALSE)
  }

  data.frame(x, groups)
}

#' @keywords internal
#' @importFrom stats model.frame lm
.kendalls_w_data <- function(x, groups, blocks, data = NULL) {
  if (inherits(frm <- x, "formula")) {
    if ((length(frm) != 3L) ||
        (length(frm[[3L]]) != 3L) ||
        (frm[[3L]][[1L]] != as.name("|")) ||
        (length(frm[[3L]][[2L]]) != 1L) ||
        (length(frm[[3L]][[3L]]) != 1L))
      stop("Formula must have the 'x ~ groups | blocks'.", call. = FALSE)

    frm[[3L]][[1L]] <- as.name("+")

    mf <- stats::model.frame(stats::lm(formula = frm, data = data))

    x <- mf[[1]]
    groups <- mf[[2]]
    blocks <- mf[[3]]
  } else if (inherits(x, c("table", "matrix", "array"))) {
    data <- data.frame(x = c(x),
                       groups = rep(colnames(x), each=nrow(x)),
                       blocks = rep(rownames(x), ncol(x)))
    x <- data[[1]]
    groups <- data[[2]]
    blocks <- data[[3]]
  } else if (is.character(x)) {
    x <- data[[x]]
    groups <- data[[groups]]
    blocks <- data[[blocks]]
  } else if (length(x) != length(groups) || length(x) != length(blocks)) {
    stop("x, groups and blocks must be of the same length.", call. = FALSE)
  }

  data.frame(x, groups, blocks)
}