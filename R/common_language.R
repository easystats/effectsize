#' Estimate Common Language Effect Sizes (CLES)
#'
#' @inheritParams cohens_d
#' @param rank Use non-parametric rank-based estimation (see [rank_biserial()])
#'   instead of parametric estimation (see [cohens_d()]).
#'
#' @details
#' These measures of effect size present group differences in probabilistic
#' terms:
#' - **Probability of superiority** is the probability that, when sampling an
#'   observation from each of the groups at random, that the observation from
#'   the second group will be larger than the sample from the first group.
#' - **Cohen's U3** is the proportion of the second group that is smaller than
#'   the median of the first group.
#' - **Overlap** (OVL) is the proportion overlap between the two distributions.
#'
#' For unequal group sizes, it is recommended to use the rank based CLES (`rank
#' = TRUE`).
#'
#' @section Confidence Intervals (CIs):
#' For parametric (`rank = FALSE`) CLES, the CIs are transformed CIs for Cohen's
#' *d* ([`d_to_cles()`]). For non-parametric (`rank = TRUE`) CLES, the CI of
#' *Pr(superiority)* is a transformed CI of the rank-biserial correlation
#' ([`rb_to_cles()`]), while for Cohen's *U3* and the Overlap coefficient the
#' confidence intervals are based on the proportion's confidence intervals
#' returned by [`stats::prop.test()`], which give a good close approximation.
#'
#' @return A data frame containing the common language effect sizes (and
#'   optionally their CIs).
#'
#' @references
#' - Cohen, J. (1977). Statistical power analysis for the behavioral sciences.
#' New York: Routledge.
#'
#' - Reiser, B., & Faraggi, D. (1999). Confidence intervals for the overlapping
#' coefficient: the normal equal variance case. Journal of the Royal Statistical
#' Society, 48(3), 413-418.
#'
#' - Ruscio, J. (2008). A probability-based measure of effect size: robustness
#' to base rates and other factors. Psychological methods, 13(1), 19–30.
#'
#' @seealso [d_to_cles()] [sd_pooled()]
#' @family effect size indices
#'
#' @examples
#' cles(mpg ~ am, data = mtcars)
#'
#' cles(mpg ~ am, data = mtcars, rank = TRUE)
#'
#' ## Individual CLES
#' A <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
#' B <- c(1.15, 0.88, 0.90, 0.74, 1.21)
#'
#' p_superiority(A, B)
#'
#' cohens_u3(A, B, rank = TRUE)
#'
#' p_overlap(A, B)
#'
#' @export
cles <- function(x,
                 y = NULL,
                 data = NULL,
                 mu = 0,
                 ci = 0.95,
                 alternative = "two.sided",
                 verbose = TRUE,
                 rank = FALSE,
                 ...) {
  if (inherits(x, "htest")) {
    if (!grepl("(t-test|Wilcoxon)", x$method)) {
      stop("'x' is not a t-test or a Wilcoxon-test!", call. = FALSE)
    }
    return(effectsize(x, type = "cles", verbose = verbose, ...))
  } else if (inherits(x, "BFBayesFactor")) {
    if (!inherits(x@numerator[[1]], c("BFoneSample", "BFindepSample"))) {
      stop("'x' is not a t-test!", call. = FALSE)
    }
    return(effectsize(x, type = "cles", ci = ci, verbose = verbose, ...))
  }

  data <- .get_data_2_samples(x, y = y, data = data, verbose = verbose)
  x <- na.omit(data[["x"]])
  y <- na.omit(data[["y"]])

  if (rank) {
    rb <- rank_biserial(
      x = x,
      y = y,
      paired = FALSE,
      mu = mu,
      ci = ci,
      alternative = alternative,
      verbose = verbose,
      ...
    )
    out <- rbind(
      rb_to_cles(rb),
      .rank_U3(x, y, ci = ci, alternative = alternative),
      .rank_overlap(x, y, ci = ci, alternative = alternative)
    )
    attr(out, "table_footer") <- c("\n- Rank based CLES", "cyan")
    out
  } else {
    d <- cohens_d(
      x = x,
      y = y,
      paired = FALSE, pooled_sd = TRUE,
      mu = mu,
      ci = ci,
      alternative = alternative,
      verbose = verbose,
      ...
    )
    d_to_cles(d)
  }
}

#' @export
#' @rdname cles
common_language <- cles

# #' @export
# #' @rdname cles
# cohens_u3 <- function(x,
#                       y = NULL,
#                       data = NULL,
#                       mu = 0,
#                       ci = 0.95,
#                       alternative = "two.sided",
#                       verbose = TRUE,
#                       rank = FALSE,
#                       ...) {
#   .cles_which(
#     "u3",
#     x,
#     y = y,
#     data = data,
#     mu = mu,
#     ci = ci,
#     alternative = alternative,
#     verbose = verbose,
#     rank = rank,
#     ...
#   )
# }

# #' @export
# #' @rdname cles
# p_superiority <- function(x,
#                           y = NULL,
#                           data = NULL,
#                           mu = 0,
#                           ci = 0.95,
#                           alternative = "two.sided",
#                           verbose = TRUE,
#                           rank = FALSE,
#                           ...) {
#   .cles_which(
#     "p",
#     x,
#     y = y,
#     data = data,
#     mu = mu,
#     ci = ci,
#     alternative = alternative,
#     verbose = verbose,
#     rank = rank,
#     ...
#   )
# }

# #' @export
# #' @rdname cles
# p_overlap <- function(x,
#                           y = NULL,
#                           data = NULL,
#                           mu = 0,
#                           ci = 0.95,
#                           alternative = "two.sided",
#                           verbose = TRUE,
#                           rank = FALSE,
#                           ...) {
#   .cles_which(
#     "ovl",
#     x,
#     y = y,
#     data = data,
#     mu = mu,
#     ci = ci,
#     alternative = alternative,
#     verbose = verbose,
#     rank = rank,
#     ...
#   )
# }

# Utils -------------------------------------------------------------------

#' @keywords internal
.rank_U3 <- function(x, y, ci = 0.95, alternative = "two.sided") {
  k <- sum(y < median(x))
  N <- length(y)

  suppressWarnings(
    prop <- prop.test(k, N,
                      conf.level = if (is.null(ci)) 0.95 else ci,
                      alternative = alternative)
  )
  out <- as.data.frame(parameters::model_parameters(prop))
  out$Proportion <- k/N

  out$Parameter <- "Cohen's U3"
  out <- out[,c("Parameter", "Proportion", "CI", "CI_low", "CI_high")]
  colnames(out)[2] <- "Coefficient"
  if (is.null(ci)) out[3:5] <- NULL
  out
}

#' @keywords internal
.rank_overlap <- function(x, y, ci = 0.95, alternative = "two.sided") {
  if (rank_biserial(x, y)[[1]] < 0) {
    k <- sum(x >= min(y)) + sum(y <= max(x))
  } else {
    k <- sum(y >= min(x)) + sum(x <= max(y))
  }
  N <- length(c(x,y))

  suppressWarnings(
    prop <- prop.test(k, N,
                      conf.level = if (is.null(ci)) 0.95 else ci,
                      alternative = alternative)
  )
  out <- as.data.frame(parameters::model_parameters(prop))
  out$Proportion <- k/N

  out$Parameter <- "Overlap"
  out <- out[,c("Parameter", "Proportion", "CI", "CI_low", "CI_high")]
  colnames(out)[2] <- "Coefficient"
  if (is.null(ci)) out[3:5] <- NULL
  out
}

# #' @keywords internal
# .cles_which <- function(type,
#                         x,
#                         y = NULL,
#                         data = NULL,
#                         mu = 0,
#                         ci = 0.95,
#                         alternative = "two.sided",
#                         verbose = TRUE,
#                         rank = FALSE,
#                         ...) {
#   CLES <- cles(
#     x,
#     y = y,
#     data = data,
#     mu = mu,
#     ci = ci,
#     alternative = alternative,
#     verbose = verbose,
#     rank = rank,
#     ...
#   )
#
#   if (type == "p")  {
#     out <- CLES[1, ]
#     colnames(out)[2] <- "p_superiority"
#   } else if (type == "u3") {
#     out <- CLES[2, ]
#     colnames(out)[2] <- "Cohens_U3"
#   } else if (type == "ovl") {
#     out <- CLES[3, ]
#     colnames(out)[2] <- "overlap"
#   }
#   out[[1]] <- NULL
#   out
# }