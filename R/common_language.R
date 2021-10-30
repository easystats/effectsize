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
#' - **Cohen's U3**
#' - **Overlap**
#' \cr\cr
#' For unequal group sizes, it is recommended to use the rank based CLES.
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
  cl <- match.call()
  cl$paired <- FALSE

  if (rank) {
    cl[[1]] <- as.name("rank_biserial")
    rb_to_cles(eval(cl))
  } else {
    cl$pooled_sd <- TRUE
    cl[[1]] <- as.name("cohens_d")

    d_to_cles(eval(cl))
  }
}

#' @export
#' @rdname cles
common_language <- cles