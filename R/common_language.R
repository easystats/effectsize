#' Estimate Common Language Effect Sizes
#'
#' @inheritParams cohens_d
#' @param np Use non-parametric estimation (see [rank_biserial()]) instead of
#'   parametric estimation (see [cohens_d()]).
#'
#' @return A data frame containing the common language effect sizes (and
#'   optionally their CIs).
#'
#' @seealso [d_to_cles()] [sd_pooled()]
#' @family effect size indices
#'
#' @export
cles <- function(x,
                 y = NULL,
                 data = NULL,
                 mu = 0,
                 ci = 0.95,
                 alternative = "two.sided",
                 verbose = TRUE,
                 np = FALSE,
                 ...) {
  cl <- match.call()
  cl$paired <- FALSE

  if (np) {
    cl[[1]] <- as.name("rank_biserial")
    rbs_to_cles(eval(cl))
  } else {
    cl$pooled_sd <- TRUE
    cl[[1]] <- as.name("cohens_d")

    d_to_cles(eval(cl))
  }
}

#' @export
#' @rdname cles
common_language <- cles