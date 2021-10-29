#' Convert Standardized Mean Difference to Common Language Effect Sizes
#'
#' @param d,rbs A numeric value of Cohen's d / rank-biserial correlation *or*
#'   the output from [cohens_d()] / [rank_biserial()].
#'
#' @details
#' This function use the following formulae for Cohen's *d*:
#' \deqn{Cohen's U_3 = \Phi(d)}{U3 = pnorm(d)}
#' \cr\cr
#' \deqn{Overlap = 2 \times \Phi(-|d|/2)}{Overlap = 2 * pnorm(-abs(d) / 2)}
#' \cr\cr
#' \deqn{Pr(superiority) = \Phi(d/\sqrt{2})}{Pr(superiority) = pnorm(d / sqrt(2))}
#'
#' @return A list of `Cohen's U3`, `Overlap`, `Probability of superiority`, a
#'   numeric vector of `Probability of superiority`, or a data frame, depending
#'   on the input.
#'
#' @note
#' These calculations assume that the populations have equal variance and are
#' normally distributed.
#'
#' @seealso [cohens_d()], [rank_biserial()]
#' @family convert between effect sizes
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
#' @export
#' @aliases convert_d_to_common_language d_to_common_language
#' @importFrom stats pnorm
d_to_cles <- function(d) {
  UseMethod("d_to_cles")
}

#' @export
d_to_cles.numeric <- function(d) {
  list(
    "Cohen's U3" = stats::pnorm(d),
    Overlap = 2 * stats::pnorm(-abs(d) / 2),
    "Probability of superiority" = stats::pnorm(d / sqrt(2))
  )
}

#' @export
d_to_cles.effectsize_difference <- function(d) {
  if (!any(colnames(d) %in% c("Cohens_d", "Hedges_g")) ||
      attr(d, "paired") ||
      !attr(d, "pooled_sd")) {
    stop("Common language effect size only applicable to 2-sample Cohen's d with pooled SD.")
  }

  out <- lapply(d[,colnames(d) %in% c("Cohens_d", "Hedges_g", "CI_low", "CI_high")],
                function(x) unlist(d_to_cles(x)))
  out <- as.data.frame(out)
  out$Parameter <- rownames(out)
  rownames(out) <- NULL
  colnames(out)[1] <- "Coefficient"

  if ("CI" %in% colnames(d)) {
    out$CI <- d$CI
    out <- out[c("Parameter", "Coefficient", "CI", "CI_low", "CI_high")]

    if (sign(d$CI_low) != sign(d$CI_high)) {
      if (d[[1]] > 0) {
        out$CI_low[2] <- out$CI_high[2]
      }
      out$CI_high[2] <- 1
    }
  } else {
    out <- out[c("Parameter", "Coefficient")]
  }

  class(out) <- c("effectsize_table", class(out))
  out
}


#' @export
#' @aliases rbs_to_common_language convert_rbs_to_common_language
#' @rdname d_to_cles
rbs_to_cles <- function(rbs) {
  UseMethod("rbs_to_cles")
}

#' @export
rbs_to_cles.numeric <- function(rbs) {
  (rbs + 1)/2
}

#' @export
rbs_to_cles.effectsize_difference <- function(rbs) {
  if (!any(colnames(rbs) == "r_rank_biserial") ||
      attr(rbs, "paired")) {
    stop("Common language effect size only applicable to 2-sample rank-biserial correlation.")
  }

  out <- lapply(rbs[,colnames(rbs) %in% c("r_rank_biserial", "CI_low", "CI_high")],
                rbs_to_cles)
  out <- as.data.frame(out)
  out$Parameter <- "Probability of superiority"
  rownames(out) <- NULL
  colnames(out)[1] <- "Coefficient"

  if ("CI" %in% colnames(rbs)) {
    out$CI <- rbs$CI
    out <- out[c("Parameter", "Coefficient", "CI", "CI_low", "CI_high")]
  } else {
    out <- out[c("Parameter", "Coefficient")]
  }

  class(out) <- c("effectsize_table", class(out))
  out
}

# Aliases -----------------------------------------------------------------

#' @export
convert_d_to_common_language <- d_to_cles

#' @export
convert_rbs_to_common_language <- rbs_to_cles

#' @export
d_to_common_language <- d_to_cles

#' @export
rbs_to_common_language <- rbs_to_cles

