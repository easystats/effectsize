#' Effect size for contingency tables
#'
#' Compute Cramer's V and phi (\eqn{\phi}) from contingency tables.
#'
#' @inheritParams stats::chisq.test
#' @param ci Confidence Interval (CI) level
#' @param adjust Should the effect size be bias-corrected? Defaults to `FALSE`.
#' @param CI Deprecated in favor of `ci`.
#' @param ... Arguments passed to [chisq.test()], such as `p`.
#'
#' @return A data frame with the effect size(s) between 0-1, and confidence interval(s).
#'
#' @seealso [chisq_to_phi()] for details regarding estimation and CIs.
#'
#' @examples
#' M <- rbind(c(150, 130, 35, 55),
#'            c(100, 50,  10, 40),
#'            c(165, 65,  2,  25))
#'
#' dimnames(M) <- list(Study = c("Psych", "Econ", "Law"),
#'                     Music = c("Pop", "Rock", "Jazz", "Classic"))
#' M
#'
#' phi(M)
#'
#' cramers_v(M)
#'
#' @importFrom stats chisq.test
#' @export
phi <- function(x, y = NULL, ci = 0.95, adjust = FALSE, CI, ...){
  if (!missing(CI)) {
    ci <- CI
    warning("'CI' argument is deprecated. Use 'ci' instead.")
  }

  res <- suppressWarnings(stats::chisq.test(x, y, ...))
  Obs <- res$observed
  Exp <- res$expected

  chisq_to_phi(chisq = .chisq(Obs, Exp),
               n = sum(Obs),
               nrow = nrow(Obs),
               ncol = ncol(Obs),
               ci = ci,
               adjust = adjust)
}

#' @rdname phi
#' @importFrom stats chisq.test
#' @export
cramers_v <- function(x, y = NULL, ci = 0.95, adjust = FALSE, CI,...){
  if (!missing(CI)) {
    ci <- CI
    warning("'CI' argument is deprecated. Use 'ci' instead.")
  }

  res <- suppressWarnings(stats::chisq.test(x, y, ...))
  Obs <- res$observed
  Exp <- res$expected

  chisq_to_cramers_v(chisq = .chisq(Obs, Exp),
                     n = sum(Obs),
                     nrow = nrow(Obs),
                     ncol = ncol(Obs),
                     ci = ci,
                     adjust = adjust)
}


#' @keywords internal
.chisq <- function(Obs, Exp) {
  sum(((Obs - Exp) ^ 2) / Exp)
}
