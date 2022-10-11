#' @title Test Effect Size for Practical Equivalence to the Null
#'
#' @description Perform a **Test for Practical Equivalence** for indices of
#'   effect size.
#'
#' @param x An effect size table, such as returned by [cohens_d()],
#'   [eta_squared()], [F_to_r()], etc.
#' @param range The range of practical equivalence of an effect. For one-sides
#'   CIs, a single value can be proved for the lower / upper bound to test
#'   against (but see more details below). For two-sided CIs, a single value is
#'   duplicated to `c(-range, range)`. If `"default"`, will be set to `[-.1,
#'   .1]`.
#' @param rule How should acceptance and rejection be decided? See details.
#' @param ... Arguments passed to or from other methods.
#'
#' @details
#' The CIs used in the equivalence test are the ones in the provided effect size
#' table. For results equivalent (ha!) to those that can be obtained using the
#' TOST approach (e.g., Lakens, 2017), appropriate CIs should be extracted using
#' the function used to make the effect size table (`cohens_d`, `eta_squared`,
#' `F_to_r`, etc), with `alternative = "two.sided"`. See examples.
#'
#' ## The Different Rules
#' - `"classic"` - **the classic method**:
#'   - If the CI is completely within the ROPE - *Accept H0*
#'   - Else, if the CI does not contain 0 - *Reject H0*
#'   - Else - *Undecided*
#' - `"cet"` - **conditional equivalence testing**:
#'   - If the CI does not contain 0 - *Reject H0*
#'   - Else, If the CI is completely within the ROPE - *Accept H0*
#'   - Else - *Undecided*
#' - `"bayes"` - **The Bayesian approach**, as put forth by Kruschke:
#'   - If the CI does is completely outside the ROPE - *Reject H0*
#'   - Else, If the CI is completely within the ROPE - *Accept H0*
#'   - Else - *Undecided*
#'
#' @seealso For more details, see [bayestestR::equivalence_test()].
#'
#' @return A data frame with the results of the equivalence test.
#'
#' @references
#' - Campbell, H., & Gustafson, P. (2018). Conditional equivalence testing: An
#' alternative remedy for publication bias. PLOS ONE, 13(4), e0195145.
#' https://doi.org/10.1371/journal.pone.0195145
#'
#' - Kruschke, J. K. (2014). Doing Bayesian data analysis: A tutorial with R,
#' JAGS, and Stan. Academic Press
#'
#' - Kruschke, J. K. (2018). Rejecting or accepting parameter values in Bayesian
#' estimation. Advances in Methods and Practices in Psychological Science, 1(2),
#' 270-280. doi: 10.1177/2515245918771304
#'
#' - Lakens, D. (2017). Equivalence Tests: A Practical Primer for t Tests,
#' Correlations, and Meta-Analyses. Social Psychological and Personality
#' Science, 8(4), 355–362. https://doi.org/10.1177/1948550617697177
#'
#' @examples
#' \donttest{
#'
#' data("hardlyworking")
#' model <- aov(salary ~ age + factor(n_comps) * cut(seniority, 3), data = hardlyworking)
#' es <- eta_squared(model, ci = 0.9, alternative = "two.sided")
#' equivalence_test(es, range = 0.15) # TOST
#'
#' data("RCT_table")
#' OR <- oddsratio(RCT_table, alternative = "greater")
#' equivalence_test(OR, range = 1)
#'
#' ds <- t_to_d(
#'   t = c(0.45, -0.65, 7, -2.2, 2.25),
#'   df_error = c(675, 525, 2000, 900, 1875),
#'   ci = 0.9, alternative = "two.sided" # TOST
#' )
#' # Can also plot
#' if (require(see)) plot(equivalence_test(ds, range = 0.2))
#' if (require(see)) plot(equivalence_test(ds, range = 0.2, rule = "cet"))
#' if (require(see)) plot(equivalence_test(ds, range = 0.2, rule = "bayes"))
#' }
#'
#' @export
equivalence_test.effectsize_table <- function(x,
                                              range = "default",
                                              rule = c("classic", "cet", "bayes"), ...) {
  rule <- match.arg(rule)

  if (!all(c("CI", "CI_low", "CI_high") %in% colnames(x))) {
    insight::format_error("CI values missing from effect size table.")
  }

  if (any(range == "default")) {
    range <- c(-0.1, 0.1)
  }

  # Validate range ---
  x_es_info <- es_info[get_effectsize_name(colnames(x)), ]

  if (length(range) == 1) {
    alt <- attr(x, "alternative", exact = TRUE)
    if (alt == "less") {
      range <- c(range, x_es_info$ub)
    } else if (alt == "greater") {
      range <- c(x_es_info$lb, range)
    } else {
      range <- c(-range, range)
    }
  }
  range <- sort(range)


  if (range[1] < x_es_info$lb) {
    range[1] <- x_es_info$lb
    insight::format_warning("Lower bound set to ", range[1], ".")
  }
  if (range[2] > x_es_info$ub) {
    range[2] <- x_es_info$ub
    insight::format_warning("Upper bound set to ", range[2], ".")
  }

  # Test ---
  signif <- x$CI_high < x_es_info$null | x_es_info$null < x$CI_low
  in_rope <- range[1] <= x$CI_low & x$CI_high <= range[2]
  out_rope <- x$CI_high < range[1] | range[2] < x$CI_low

  # Label ---
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

  class(x) <- c("equivalence_test_effectsize", "effectsize_table", "see_equivalence_test_effectsize", "data.frame")
  attr(x, "rope") <- range
  attr(x, "rule") <- rule
  return(x)
}
