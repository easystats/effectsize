#' Interpret Bayesian Diagnostic Indices
#'
#' Interpretation of Bayesian diagnostic indices, such as Effective Sample Size (ESS) and Rhat.
#'
#' @param ess Value or vector of Effective Sample Size (ESS) values.
#' @param rhat Value or vector of Rhat values.
#' @param rules A character string (see *Rules*) or a custom set of [rules()].
#'
#' @section Rules:
#'
#' ## ESS
#'
#' ```{r, echo = FALSE, results = "asis"}
#' insight::print_md(.ess_rules, "ESS", "Bürkner, P. C. (2017) (`{.rn}`; default):")
#' ```
#'
#' ## Rhat
#' ```{r, echo = FALSE, results = "asis"}
#' titles <- c("Vehtari et al. (2019) (`{.rn}`; default):",
#'             "Gelman & Rubin (1992) (``):")
#'
#' insight::print_md(.rhat_rules, "Rhat", titles)
#' ```
#'
#'
#' @examples
#' interpret_ess(1001)
#' interpret_ess(c(852, 1200))
#'
#' interpret_rhat(1.00)
#' interpret_rhat(c(1.5, 0.9))
#' @references
#'   - Bürkner, P. C. (2017). brms: An R package for Bayesian multilevel models
#'   using Stan. Journal of Statistical Software, 80(1), 1-28.
#'
#'   - Gelman, A., & Rubin, D. B. (1992). Inference from iterative simulation
#'   using multiple sequences. Statistical science, 7(4), 457-472.
#'
#'   - Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P. C.
#'   (2019). Rank-normalization, folding, and localization: An improved Rhat for
#'   assessing convergence of MCMC. arXiv preprint arXiv:1903.08008.
#'
#' @keywords interpreters
#' @export
interpret_ess <- function(ess, rules = "burkner2017") {
  rules <- .match.rules(rules,.ess_rules)

  interpret(ess, rules)
}



#' @rdname interpret_ess
#' @export
interpret_rhat <- function(rhat, rules = "vehtari2019") {
  rules <- .match.rules(rules,.rhat_rules)

  interpret(rhat, rules)
}


# rules -------------------------------------------------------------------

#' @keywords internal
.ess_rules <- c(
  rules(c(1000), c("insufficient", "sufficient"), name = "burkner2017", right = FALSE)
)

#' @keywords internal
.rhat_rules <- c(
  rules(c(1.01), c("converged", "failed"), name = "vehtari2019", right = FALSE),
  rules(c(1.1), c("converged", "failed"), name = "gelman1992", right = FALSE)
)
