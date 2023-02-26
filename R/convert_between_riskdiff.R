#' Convert Between Odds Ratios and Risk Ratios
#'
#' @param OR,RR,ARR Risk ratio of `p1/p0` Odds ratio of `odds(p1)/odds(p0)`
#'   (possibly log-ed), or Absolute Risk Reduction or `p1 - p0`. `OR` can also
#'   be a logistic regression model.
#' @param x Absolute Risk Reduction or Number Needed to Treat.
#' @param p0 Baseline risk
#' @param ... Arguments passed to and from other methods.
#' @inheritParams oddsratio_to_d
#' @inheritParams cohens_d
#'
#' @return Converted index, or if `OR` is a logistic regression model, a
#'   parameter table with the converted indices.
#'
#' @family convert between effect sizes
#' @seealso [oddsratio()], [riskratio()], [arr()], and [nnt()].
#'
#' @examples
#' p0 <- 0.4
#' p1 <- 0.7
#'
#' (OR <- probs_to_odds(p1) / probs_to_odds(p0))
#' (RR <- p1 / p0)
#' (ARR <- p1 - p0)
#' (NNT <- arr_to_nnt(ARR))
#'
#' riskratio_to_oddsratio(RR, p0 = p0)
#' oddsratio_to_riskratio(OR, p0 = p0)
#' riskratio_to_arr(RR, p0 = p0)
#' arr_to_oddsratio(nnt_to_arr(NNT), p0 = p0)
#'
#' m <- glm(am ~ factor(cyl),
#'   data = mtcars,
#'   family = binomial()
#' )
#' oddsratio_to_riskratio(m, verbose = FALSE) # RR is relative to the intercept if p0 not provided
#'
#' @references
#'
#' Grant, R. L. (2014). Converting an odds ratio to a range of plausible
#' relative risks for better communication of research findings. Bmj, 348,
#' f7450.
#'
#' @export
oddsratio_to_riskratio <- function(OR, p0, log = FALSE, verbose = TRUE, ...) {
  UseMethod("oddsratio_to_riskratio")
}

#' @export
oddsratio_to_riskratio.numeric <- function(OR, p0, log = FALSE, verbose = TRUE, ...) {
  if (log) OR <- exp(OR)

  RR <- OR / (1 - p0 + (p0 * OR))

  if (log) RR <- log(RR)
  return(RR)
}

#' @export
oddsratio_to_riskratio.default <- function(OR, p0, log = FALSE, verbose = TRUE, ...) {
  mi <- .get_model_info(OR, ...)
  if (!mi$is_binomial || !mi$is_logit) {
    insight::format_error("Model must be a binomial model with a logit-link (logistic regression).")
  }

  RR <- parameters::model_parameters(OR, exponentiate = !log, effects = "fixed", ...)
  RR$SE <- NULL
  RR$z <- NULL
  RR$df_error <- NULL
  RR$p <- NULL

  used_intercept <- missing(p0)
  if (used_intercept) {
    p0 <- RR[["Coefficient"]][RR$Parameter == "(Intercept)"]
    if (!log) p0 <- log(p0)
    p0 <- stats::plogis(p0)

    if (verbose) {
      insight::format_warning(
        "'p0' not provided.",
        sprintf(
          "RR is relative to the intercept (p0 = %s) - make sure your intercept is meaningful.",
          insight::format_value(p0)
        )
      )
    }
  }

  trans_cols <- colnames(RR) %in% c("Coefficient", "CI_low", "CI_high")
  RR[, trans_cols] <-
    lapply(RR[, trans_cols, drop = FALSE],
      oddsratio_to_riskratio,
      p0 = p0, log = log
    )

  if (verbose && any(c("CI_low", "CI_high") %in% colnames(RR))) {
    insight::format_alert("CIs are back-transformed from the logit scale.")
  }

  RR[RR$Parameter == "(Intercept)", "Coefficient"] <- p0
  RR[RR$Parameter == "(Intercept)", c("CI_low", "CI_high")] <- NA

  if (!used_intercept) {
    RR[RR$Parameter == "(Intercept)", "Parameter"] <- "(p0)"
  }

  attr(RR, "coefficient_name") <- if (log) "Log-RR" else "Risk Ratio"
  return(RR)
}



#' @rdname oddsratio_to_riskratio
#' @export
riskratio_to_oddsratio <- function(RR, p0, log = FALSE, verbose = TRUE, ...) {
  if (log) RR <- exp(RR)

  OR <- RR * (1 - p0) / (1 - RR * p0)

  if (log) OR <- log(OR)
  return(OR)
}

#' @rdname oddsratio_to_riskratio
#' @export
riskratio_to_arr <- function(RR, p0, log = FALSE, verbose = TRUE, ...) {
  if (log) RR <- exp(RR)
  RR * p0 - p0
}

#' @rdname oddsratio_to_riskratio
#' @export
oddsratio_to_arr <- function(OR, p0, log = FALSE, verbose = TRUE, ...) {
  if (log) OR <- exp(OR)
  RR <- oddsratio_to_riskratio(OR, p0, log = FALSE, verbose = verbose)
  riskratio_to_arr(RR, p0, verbose = verbose)
}

#' @rdname oddsratio_to_riskratio
#' @export
arr_to_riskratio <- function(ARR, p0, log = FALSE, verbose = TRUE, ...) {
  RR <- ARR / p0 + 1
  if (log) RR <- log(RR)
  RR
}

#' @rdname oddsratio_to_riskratio
#' @export
arr_to_oddsratio <- function(ARR, p0, log = FALSE, verbose = TRUE, ...) {
  RR <- arr_to_riskratio(ARR, p0, log = log, verbose = verbose)
  riskratio_to_oddsratio(RR, p0, log = log, verbose = verbose)
}

#' @rdname oddsratio_to_riskratio
#' @export
arr_to_nnt <- function(x) {
  1 / x
}

#' @rdname oddsratio_to_riskratio
#' @export
nnt_to_arr <- arr_to_nnt
