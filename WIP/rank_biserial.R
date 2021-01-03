#' Effect size for non-parametric differences
#'
#' Compute the rank-biserial correlation (r_RB) for non-parametric differences -
#' both for the one sample or paired samples case (that would normally be tested
#' with Wilcoxon's Signed Rank Test) and for two independant samples case (that
#' would normally be tested with Mann-Whitney's U Test). See
#' [stats:wilcox.test].
#'
#' @inheritParams cohens_d
#' @param mu a number indicating the value around which (a-)symmetry (for
#'   one-sample or paired samples) or shift (for independant samples) is to be
#'   estimated. See [stats:wilcox.test].
#'
#' @details
#'
#' ## Confidence Intervals
#' Confidence Intervals are estimated using the bootstrap method.
#'
#' @return A data frame with the effect size (`r_rank_biserial`) and its CI
#'   (`CI_low` and `CI_high`).
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
#' @references
#' - Cureton, E. E. (1956). Rank-biserial correlation. Psychometrika, 21(3), 287-290.
#' - Kerby, D. S. (2014). The simple difference formula: An approach to teaching nonparametric correlation. Comprehensive Psychology, 3, 11-IT.
#'
#' @export
rank_biserial <- function(x, y = NULL, data = NULL, mu = 0,
                          ci = 0.95, iterations = 200,
                          paired = FALSE) {

  out <- effectsize:::.deal_with_cohens_d_arguments(x, y, data)

  x <- out$x
  y <- out$y

  if (is.null(y)) {
    y <- rep(0, length.out = length(x))
    paired <- TRUE
  }

  if (paired) {
    r_rbs <- .r_rbs_paired(x, y, mu = mu)
  } else {
    r_rbs <- .r_rbs_indep(x, y, mu = mu)
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

  class(out) <- c("effectsize_table", class(out))
  return(out)
}

#' @keywords internal
#' @importFrom stats na.omit
.r_rbs_paired <- function(x, y, mu) {
  d <- (x - y) - mu

  r_sign <- effectsize::ranktransform(d, sign = TRUE)
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
.r_rbs_indep <- function(x, y, mu){
  x <- stats::na.omit(x)
  y <- stats::na.omit(y)

  Ry <- effectsize::ranktransform(c(x - mu, y))
  Group = c(rep("A", length(x)),
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
