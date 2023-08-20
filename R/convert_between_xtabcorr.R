#' Convert Between Effect Sizes for Contingency Tables Correlations
#'
#' Enables a conversion between different indices of effect size, such as
#' Cohen's *w* to \ifelse{latex}{\eqn{Fei}}{פ (Fei)}, and Cramer's *V* to
#' Tschuprow's *T*.
#'
#' @param w,c,v,t,fei Effect size to be converted
#' @inheritParams chisq_to_tschuprows_t
#' @inheritParams fei
#'
#' @family convert between effect sizes
#' @seealso [cramers_v()] [chisq_to_fei()]
#'
#' @examples
#' library(effectsize)
#'
#' ## 2D tables
#' ## ---------
#' data("Music_preferences2")
#' Music_preferences2
#'
#' cramers_v(Music_preferences2, adjust = FALSE)
#'
#' v_to_t(0.80, 3, 4)
#'
#' tschuprows_t(Music_preferences2)
#'
#'
#'
#' ## Goodness of fit
#' ## ---------------
#' data("Smoking_FASD")
#' Smoking_FASD
#'
#' cohens_w(Smoking_FASD, p = c(0.015, 0.010, 0.975))
#'
#' w_to_fei(0.11, p = c(0.015, 0.010, 0.975))
#'
#' fei(Smoking_FASD, p = c(0.015, 0.010, 0.975))
#'
#' @examplesIf require(pwr)
#' ## Power analysis
#' ## --------------
#' # See https://osf.io/cg64s/
#'
#' p0 <- c(0.35, 0.65)
#' Fei <- 0.3
#'
#' pwr::pwr.chisq.test(
#'   w = fei_to_w(Fei, p = p0),
#'   df = length(p0) - 1,
#'   sig.level = 0.01,
#'   power = 0.85
#' )
#'
#' @references
#' - Ben-Shachar, M.S., Patil, I., Thériault, R., Wiernik, B.M., Lüdecke, D.
#' (2023). Phi, Fei, Fo, Fum: Effect Sizes for Categorical Data That Use the
#' Chi‑Squared Statistic. Mathematics, 11, 1982. \doi{10.3390/math11091982}
#' - Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd Ed.). New York: Routledge.
#'
#' @export
w_to_fei <- function(w, p) {
  w / sqrt(1 / min(p) - 1)
}

#' @export
#' @rdname w_to_fei
w_to_v <- function(w, nrow, ncol) {
  w / sqrt(pmin(nrow - 1, ncol - 1))
}

#' @export
#' @rdname w_to_fei
w_to_t <- function(w, nrow, ncol) {
  w / sqrt(sqrt((nrow - 1) * (ncol - 1)))
}

#' @export
#' @rdname w_to_fei
w_to_c <- function(w) {
  w / sqrt(w^2 + 1)
}

## To w -----------------------

#' @export
#' @rdname w_to_fei
fei_to_w <- function(fei, p) {
  fei * sqrt(1 / min(p) - 1)
}

#' @export
#' @rdname w_to_fei
v_to_w <- function(v, nrow, ncol) {
  v * sqrt(pmin(nrow - 1, ncol - 1))
}

#' @export
#' @rdname w_to_fei
t_to_w <- function(t, nrow, ncol) {
  t * sqrt(sqrt((nrow - 1) * (ncol - 1)))
}

#' @export
#' @rdname w_to_fei
c_to_w <- function(c) {
  c / sqrt(1 - c^2)
}

## Other ----------------------

#' @export
#' @rdname w_to_fei
v_to_t <- function(v, nrow, ncol) {
  w_to_t(v_to_w(v, nrow, ncol), nrow, ncol)
}

#' @export
#' @rdname w_to_fei
t_to_v <- function(t, nrow, ncol) {
  w_to_v(t_to_w(t, nrow, ncol), nrow, ncol)
}
