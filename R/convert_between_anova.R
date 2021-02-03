#' Convert between ANOVA effect sizes
#'
#' @param es Any measure of variance explained such as Eta-, Epsilon-, Omega-,
#'   or R-Squared, partial or otherwise. See details.
#' @param f,f2 Cohen's *f* or *f*-squared.
#'
#' @details
#' Any measure of variance explained can be converted to a corresponding Cohen's
#' *f* via:
#' \cr\cr
#' \deqn{f^2 = \frac{\eta^2}{1 - \eta^2}}
#' \cr\cr
#' \deqn{\eta^2 = \frac{f^2}{1 + f^2}}
#' \cr\cr
#' If a partial Eta-Squared is used, the resulting Cohen's *f* is a
#' partial-Cohen's *f*; If a less biased estimate of variance explained is used
#' (such as Epsilon- or Omega-Squared), the resulting Cohen's *f* is likewise a
#' less biased estimate of Cohen's *f*.
#'
#' @seealso [eta_squared()] for more details.
#' @family convert between effect sizes
#'
#' @references
#' - Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd Ed.). New York: Routledge.
#' - Steiger, J. H. (2004). Beyond the F test: Effect size confidence intervals and tests of close fit in the analysis of variance and contrast analysis. Psychological Methods, 9, 164-182.
#'
#' @export
eta2_to_f2 <- function(es) {
  es / (1 - es)
}

#' @export
#' @rdname eta2_to_f2
eta2_to_f <- function(es) {
  sqrt(eta2_to_f2(es))
}


#' @export
#' @rdname eta2_to_f2
f2_to_eta2 <- function(f2) {
  f2 / (1 + f2)
}


#' @export
#' @rdname eta2_to_f2
f_to_eta2 <- function(f) {
  f2_to_eta2(f^2)
}




# eta2_to_F <- function(eta2, df, df_error, ...) {
#   eta2 * df_error / ((1 - eta2) * df)
# }
#
#
# f_to_omega2 <- function(f, df, df_error, ci = 0.9, ...) {
#   eta2 <- f_to_eta2(f)
#   fvalue <- eta2_to_F(eta2, df, df_error)
#   F_to_omega2(
#     f = fvalue,
#     df = df,
#     df_error = df_error,
#     ci = ci
#   )
# }
