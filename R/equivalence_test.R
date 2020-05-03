#' @importFrom bayestestR equivalence_test
#' @export
bayestestR::equivalence_test

#' @title Test for Practical Equivalence
#'
#' @description Perform a \strong{Test for Practical Equivalence} for indices of effect size.
#'
#' @param x An effect size table, such as returned by \code{cohens_d, eta_squared, F_to_r}, etc.
#' @param range The range of practical equivalence of an effect. If a single value is provided,
#'   the test is done against \code{c(-range, range)}. For effect sizes that cannot be negative,
#'   the lower bound is set to 0. If \code{"default"}, will be set to \code{[-.1, .1]}.
#' @param rule How should acceptance and rejection be decided? See details.
#' @param ... Arguments passed to or from other methods.
#'
#' @details
#' The CIs used in the equivalence test are the ones in the provided effect size table.
#' For results equivalent (ha!) to those that can be obtained using the TOST approach (e.g., Lakens, 2017),
#' appropriate CIs should be extracted using the function used to make the effect size table
#' (\code{cohens_d, eta_squared, F_to_r}, etc). See examples.
#'
#' \subsection{The Different Rules}{
#' \code{"classic"} - \strong{the classic method}: \itemize{
#'     \item If the CI is completely within the ROPE - \emph{Accept H0}
#'     \item Else, if the CI does not contain 0 - \emph{Reject H0}
#'     \item Else - \emph{Undecided}
#' }
#' \code{"cet"} - \strong{conditional equivalence testing}: \itemize{
#'     \item If the CI does not contain 0 - \emph{Reject H0}
#'     \item Else, If the CI is completely within the ROPE - \emph{Accept H0}
#'     \item Else - \emph{Undecided}
#' }
#' \code{"bayes"} - \strong{The Bayesian approach}, as put forth by Kruschke: \itemize{
#'     \item If the CI does is completely outsie the ROPE - \emph{Reject H0}
#'     \item Else, If the CI is completely within the ROPE - \emph{Accept H0}
#'     \item Else - \emph{Undecided}
#' }
#' }
#'
#' @seealso For more details, see \code{\link[bayestestR:equivalence_test]{equivalence_test}}.
#'
#' @return A data frame.
#'
#' @references
#' \itemize{
#'   \item Campbell, H., & Gustafson, P. (2018). Conditional equivalence testing: An alternative remedy for publication bias. PLOS ONE, 13(4), e0195145. https://doi.org/10.1371/journal.pone.0195145
#'   \item Kruschke, J. K. (2014). Doing Bayesian data analysis: A tutorial with R, JAGS, and Stan. Academic Press
#'   \item Kruschke, J. K. (2018). Rejecting or accepting parameter values in Bayesian estimation. Advances in Methods and Practices in Psychological Science, 1(2), 270-280. doi: 10.1177/2515245918771304
#'   \item Lakens, D. (2017). Equivalence Tests: A Practical Primer for t Tests, Correlations, and Meta-Analyses. Social Psychological and Personality Science, 8(4), 355–362. https://doi.org/10.1177/1948550617697177
#' }
#'
#' @examples
#'
#' \donttest{
#' model <- aov(mpg ~ factor(am) * factor(cyl), data = mtcars)
#' es <- eta_squared(model)
#' equivalence_test(es, range = 0.15)
#'
#' ds <- t_to_d(t = c(0.45, -0.65, 7, -2.2, 2.25),
#'              df_error = c(675, 525, 2000, 900, 1875),
#'              ci = 0.9) # TOST approach
#' equivalence_test(ds, range = 0.2)
#'
#' # Can also plot
#' if (require(see)) plot(equivalence_test(ds, range = 0.2))
#' if (require(see)) plot(equivalence_test(ds, range = 0.2, rule = "cet"))
#' if (require(see)) plot(equivalence_test(ds, range = 0.2, rule = "bayes"))
#' }
#'
#' @export
equivalence_test.effectsize_table <- function(x, range = "default", rule = c("classic","cet","bayes"), ...) {
  rule <- match.arg(rule)

  if (!all(c("CI", "CI_low", "CI_high") %in% colnames(x))) {
    stop("CI values missing from effect size table.", call. = FALSE)
  }

  if (any(range == "default")) {
    range <- c(-0.1, 0.1)
  }
  range <- sort(range)

  if (any(colnames(x) %in% es_info$name[es_info$direction=="onetail"])) {
    if (all(range < 0)) {
      stop("'range' is completely negative. For this effect size, only positive values can be tested.", call. = FALSE)
    } else if (length(range) > 1) {
      if (all(range > 0)) {
        warning("'range' is completely positive. Using only the smallest value as a cutoff.")
        range <- range[1]
      } else {
        range <- range[2]
      }
    }

    signif <- x$CI_low > 0
    in_rope <- x$CI_high < range
    out_rope <- range < x$CI_low

    range <- c(0,range)

  } else {
    if (length(range) == 1) {
      range <- c(-range,range)
    }
    range <- sort(range)

    signif <- x$CI_high < 0 | 0 < x$CI_low
    in_rope <- range[1] < x$CI_low & x$CI_high < range[2]
    out_rope <- x$CI_high < range[1] | range[2] < x$CI_low
  }

  x$ROPE_Equivalence <- "Undecided"
  if (rule == "classic") {
    x$ROPE_Equivalence[in_rope] <- "Accepted"
    x$ROPE_Equivalence[signif & !in_rope] <- "Rejected"
  } else if (rule == "cet") {
    x$ROPE_Equivalence[signif] <- "Rejected"
    x$ROPE_Equivalence[in_rope & !signif] <- "Accepted"
  } else {
    x$ROPE_Equivalence[out_rope] <- "Rejected"
    x$ROPE_Equivalence[in_rope] <- "Accepted"
  }

  # x$ROPE_Equivalence[x$CI_low == x$CI_high] <- NA_character_


  attr(x, "rope") <- range
  attr(x, "rule") <- rule
  class(x) <- c("equivalence_test_effectsize", "see_equivalence_test_effectsize", "data.frame")
  return(x)
}
