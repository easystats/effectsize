#' Estimate Common Language Effect Sizes
#'
#' @inheritParams cohens_d
#' @param np Use non-parametric estimation (see [rank_biserial()]) instead of
#'   parametric estimation (see [cohens_d()]).
#'
#' @return A data frame containing the common language effect sizes (and
#'   optionally their CIs).
#'
#' @export
common_language <- function(x,
                            y = NULL,
                            data = NULL,
                            mu = 0,
                            ci = 0.95,
                            alternative = "two.sided",
                            verbose = TRUE,
                            np = FALSE,
                            ...) {
  if (np) {
    es <- rank_biserial(
      x, y = y, data = data,
      mu = mu, paired = FALSE,
      ci = ci, alternative = alternative,
      verbose = verbose)
    rbs_to_common_language(es)
  } else {
    es <- cohens_d(
      x, y = y, data = data,
      mu = mu, paired = FALSE, pooled_sd = TRUE,
      ci = ci, alternative = alternative,
      verbose = verbose)
    d_to_common_language(es)
  }
}