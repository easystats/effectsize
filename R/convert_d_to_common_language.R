#' Convert Standardized Mean Difference to Common Language Effect Sizes
#'
#' @inheritParams d_to_r
#'
#' @details
#' This function use the following formulae:
#' \deqn{Cohen's U_3 = \Phi(d)}{U3 = pnorm(d)}
#' \cr\cr
#' \deqn{Overlap = 2 \times \Phi(-|d|/2)}{Overlap = 2 * pnorm(-abs(d) / 2)}
#' \cr\cr
#' \deqn{Pr(superiority) = \Phi(d/\sqrt{2})}{Pr(superiority) = pnorm(d / sqrt(2))}
#'
#' @return A list of `Cohen's U3`, `Overlap`, `Probability of superiority`.
#'
#' @note
#' These calculations assume that the populations have equal variance and are
#' normally distributed.
#'
#' @seealso [cohens_d()]
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
#' @aliases convert_d_to_common_language
#' @importFrom stats pnorm
d_to_common_language <- function(d) {
  list(
    "Cohen's U3" = stats::pnorm(d),
    Overlap = 2 * stats::pnorm(-abs(d) / 2),
    "Probability of superiority" = stats::pnorm(d / sqrt(2))
  )
}

#' @export
convert_d_to_common_language <- d_to_common_language
