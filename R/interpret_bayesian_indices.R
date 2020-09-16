#' Interpret Bayesian diagnostic indices
#'
#' Interpretation of Bayesian indices, such as Effective Sample Size (ESS), Rhat, or percentage in ROPE.
#'
#' @param ess Value or vector of Effective Sample Size (ESS) values.
#' @param rhat Value or vector of Rhat values.
#' @param rules A character string (see details) or a custom set of [rules()].
#'
#' @details
#' Rules sets:
#' - **ESS**: Can be `"burkner2017"` (default).
#' - **Rhat**: Can be `"vehtari2019"` (default) or `"gelman1992"`.
#'
#'
#' @examples
#' interpret_ess(1001)
#' interpret_ess(c(852, 1200))
#'
#' interpret_rhat(1.00)
#' interpret_rhat(c(1.5, 0.9))
#' @references
#' \itemize{
#'   \item Bürkner, P. C. (2017). brms: An R package for Bayesian multilevel models using Stan. Journal of Statistical Software, 80(1), 1-28.
#'   \item Gelman, A., & Rubin, D. B. (1992). Inference from iterative simulation using multiple sequences. Statistical science, 7(4), 457-472.
#'   \item Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P. C. (2019). Rank-normalization, folding, and localization: An improved Rhat for assessing convergence of MCMC. arXiv preprint arXiv:1903.08008.
#' }
#' @export
interpret_ess <- function(ess, rules = "burkner2017") {
  if (is.rules(rules)) {
    return(interpret(abs(ess), rules))
  } else {
    if (rules == "burkner2017") {
      return(interpret(abs(ess), rules(c(1000), c("unsufficient", "sufficient"))))
    } else {
      stop("rules must be 'burkner2017' or an object of type rules.")
    }
  }
}



#' @rdname interpret_ess
#' @export
interpret_rhat <- function(rhat, rules = "vehtari2019") {
  if (is.rules(rules)) {
    return(interpret(abs(rhat), rules))
  } else {
    if (rules == "vehtari2019") {
      return(interpret(abs(rhat), rules(c(1.01), c("converged", "failed"))))
    } else if (rules == "gelman1992") {
      return(interpret(abs(rhat), rules(c(1.1), c("converged", "failed"))))
    } else {
      stop("rules must be 'vehtari2019', 'gelman1992' or an object of type rules.")
    }
  }
}
