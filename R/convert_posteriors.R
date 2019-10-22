#' Convert posterior distributions from a Bayesian model
#'
#' Convert posterior distributions from a Bayesian model to indices of effect size.
#'
#' @param model A Bayesian statistical model.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' library(rstanarm)
#' model <- stan_glm(mpg ~ cyl, data = mtcars, refresh = 0, chains = 2)
#' @export
convert_posteriors_to_r <- function(model, ...) {

  dof <- parameters::dof(model)
  t <- convert_posteriors_to_t(model)
  r <- t
  for (i in 1:ncol(t)) {
    r[, i] <- convert_t_to_r(t = t[, i], df_error = dof[i])
  }
  r
}

#' @rdname convert_posteriors_to_r
#' @export
posteriors_to_r <- convert_posteriors_to_r








#' @rdname convert_posteriors_to_r
#' @export
convert_posteriors_to_t <- function(model, ...) {
  posteriors <- insight::get_parameters(model, ...)
  as.data.frame(sapply(posteriors, .compute_t))
}

#' @rdname convert_posteriors_to_r
#' @export
posteriors_to_t <- convert_posteriors_to_t







#' @importFrom stats sd
#' @keywords internal
.compute_t <- function(posterior) {
  denominator <- stats::sd(posterior, na.rm = TRUE)
  posterior / denominator
}
