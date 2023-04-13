#' Convert Between *d*, *r*, and Odds Ratio
#'
#' Enables a conversion between different indices of effect size, such as
#' Cohen's *w* to \ifelse{latex}{\eqn{Fei}}{פ (Fei)}, and Cramer's *V* to
#' Tschuprow's *T*.
#'
#' @param v,t,w,fei Effect size to be converted
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
#' @export
v_to_t <- function(v, nrow, ncol) {
  f.v <- sqrt(pmin(nrow - 1, ncol - 1))
  f.t <- sqrt(sqrt((nrow - 1) * (ncol - 1)))
  v * f.v / f.t
}

#' @export
#' @rdname v_to_t
t_to_v <- function(t, nrow, ncol) {
  f.v <- sqrt(pmin(nrow - 1, ncol - 1))
  f.t <- sqrt(sqrt((nrow - 1) * (ncol - 1)))
  t * f.t / f.v
}

#' @export
#' @rdname v_to_t
fei_to_w <- function(fei, p) {
  f <- sqrt(1 / min(p) - 1)
  fei * f
}

#' @export
#' @rdname v_to_t
w_to_fei <- function(w, p) {
  f <- sqrt(1 / min(p) - 1)
  w / f
}
