#' Convert Standardized Differences to Common Language Effect Sizes
#'
#' @param d,rb A numeric vector of Cohen's d / rank-biserial correlation *or*
#'   the output from [cohens_d()] / [rank_biserial()].
#'
#' @details
#' This function use the following formulae for Cohen's *d*:
#' \deqn{Pr(superiority) = \Phi(d/\sqrt{2})}{Pr(superiority) = pnorm(d / sqrt(2))}
#' \cr
#' \deqn{\text{Cohen's }U_3 = \Phi(d)}{U3 = pnorm(d)}
#' \cr
#' \deqn{\text{Cohen's }U_2 = \Phi(|d|/2)}{U2 = pnorm(abs(d)/2)}
#' \cr
#' \deqn{\text{Cohen's }U_1 = (2\times U_2 - 1)/U_2}{U1 = (2 * U2 - 1) / U2}
#' \cr
#' \deqn{Overlap = 2 \times \Phi(-|d|/2)}{Overlap = 2 * pnorm(-abs(d) / 2)}
#' \cr
#' And the following for the rank-biserial correlation:
#' \deqn{Pr(superiority) = (r_{rb} + 1)/2}{Pr(superiority) = (rb + 1)/2}
#'
#' @return A list of `Cohen's U3`, `Overlap`, `Pr(superiority)`, a
#'   numeric vector of `Pr(superiority)`, or a data frame, depending
#'   on the input.
#'
#' @note
#' These calculations assume that the populations have equal variance and are
#' normally distributed.
#'
#' @seealso See [cohens_u3] for descriptions of the effect sizes (also,
#'   [cohens_d()], [rank_biserial()]).
#' @family convert between effect sizes
#'
#' @references
#' - Cohen, J. (1977). Statistical power analysis for the behavioral sciences.
#' New York: Routledge.
#'
#' - Reiser, B., & Faraggi, D. (1999). Confidence intervals for the overlapping
#' coefficient: the normal equal variance case. Journal of the Royal Statistical
#' Society, 48(3), 413-418.
#'
#' - Ruscio, J. (2008). A probability-based measure of effect size: robustness
#' to base rates and other factors. Psychological methods, 13(1), 19–30.
#'
#' @name diff_to_cles
#' @aliases d_to_cles rb_to_cles



# p_superiority ------------------------------------------------------

#' @export
#' @rdname diff_to_cles
d_to_p_superiority <- function(d) {
  UseMethod("d_to_p_superiority")
}

#' @export
#' @importFrom stats pnorm
d_to_p_superiority.numeric <- function(d) {
  stats::pnorm(d / sqrt(2))
}

#' @export
#' @rdname diff_to_cles
rb_to_p_superiority <- function(rb) {
  UseMethod("rb_to_p_superiority")
}

#' @export
rb_to_p_superiority.numeric <- function(rb) {
  (rb + 1) / 2
}

# U2 ----------------------------------------------------------------------

#' @export
#' @rdname diff_to_cles
d_to_U2 <- function(d) {
  UseMethod("d_to_U2")
}

#' @export
#' @importFrom stats pnorm
d_to_U2.numeric <- function(d) {
  stats::pnorm(abs(d)/2)
}

# U1 ----------------------------------------------------------------------

#' @export
#' @rdname diff_to_cles
d_to_U1 <- function(d) {
  UseMethod("d_to_U1")
}

#' @export
d_to_U1.numeric <- function(d) {
  P <- d_to_U2(d)
  (2 * P - 1) / P
}

# U3 ----------------------------------------------------------------------

#' @export
#' @rdname diff_to_cles
d_to_U3 <- function(d) {
  UseMethod("d_to_U3")
}

#' @export
#' @importFrom stats pnorm
d_to_U3.numeric <- function(d) {
  stats::pnorm(d)
}


# Overlap -----------------------------------------------------------------

#' @export
#' @rdname diff_to_cles
d_to_overlap <- function(d) {
  UseMethod("d_to_overlap")
}

#' @export
#' @importFrom stats pnorm
d_to_overlap.numeric <- function(d) {
  2 * stats::pnorm(-abs(d) / 2)
}


# From Cohen's d ----------------------------------------------------------

#' @export
d_to_p_superiority.effectsize_difference <- function(d) {
  out <- .cohens_d_to_cles(d, converter = d_to_p_superiority)
  colnames(out)[1] <- "p_superiority"
  out
}

#' @export
d_to_U1.effectsize_difference <- function(d) {
  out <- .cohens_d_to_cles(d, converter = d_to_U1)
  colnames(out)[1] <- "Cohens_U1"

  if ("CI" %in% colnames(out)) {
    if (d$Cohens_d < 0) {
      out[3:4] <- out[4:3]
      if (attr(out, "alternative") == "less") {
        attr(out, "alternative") <- "greater"
      } else if (attr(out, "alternative") == "greater") {
        attr(out, "alternative") <- "less"
      }
    }

    if (sign(d$CI_low) != sign(d$CI_high)) {
      out$CI_low <- 0
    }
  }

  out
}

#' @export
d_to_U2.effectsize_difference <- function(d) {
  out <- .cohens_d_to_cles(d, converter = d_to_U2)
  colnames(out)[1] <- "Cohens_U2"

  if ("CI" %in% colnames(out)) {
    if (d$Cohens_d < 0) {
      out[3:4] <- out[4:3]
      if (attr(out, "alternative") == "less") {
        attr(out, "alternative") <- "greater"
      } else if (attr(out, "alternative") == "greater") {
        attr(out, "alternative") <- "less"
      }
    }

    if (sign(d$CI_low) != sign(d$CI_high)) {
      out$CI_low <- 0.5
    }
  }

  out
}

#' @export
d_to_U3.effectsize_difference <- function(d) {
  out <- .cohens_d_to_cles(d, converter = d_to_U3)
  colnames(out)[1] <- "Cohens_U3"
  out
}

#' @export
d_to_overlap.effectsize_difference <- function(d) {
  out <- .cohens_d_to_cles(d, converter = d_to_overlap)
  colnames(out)[1] <- "Overlap"

  if ("CI" %in% colnames(out)) {
    if (d$Cohens_d > 0) {
      out[3:4] <- out[4:3]
      if (attr(out, "alternative") == "less") {
        attr(out, "alternative") <- "greater"
      } else if (attr(out, "alternative") == "greater") {
        attr(out, "alternative") <- "less"
      }
    }

    if (sign(d$CI_low) != sign(d$CI_high)) {
      out$CI_high <- 1
    }
  }

  out
}

## Main ----------------

#' @keywords internal
.cohens_d_to_cles <- function(d, converter) {
  if (!any(colnames(d) %in% c("Cohens_d", "Hedges_g")) ||
      attr(d, "paired") ||
      !attr(d, "pooled_sd")) {
    stop("Common language effect size only applicable to 2-sample Cohen's d with pooled SD.", call. = FALSE)
  }

  cols_to_convert <- colnames(d) %in% c("Cohens_d", "Hedges_g", "CI_low", "CI_high")

  out <- d
  out[cols_to_convert] <- lapply(d[cols_to_convert], converter)
  out <- as.data.frame(out)
  class(out) <- c("effectsize_table", class(out))
  out
}





# From r {rbs} ------------------------------------------------------------

#' @export
rb_to_p_superiority.effectsize_difference <- function(rb) {
  if (!any(colnames(rb) == "r_rank_biserial") ||
    attr(rb, "paired")) {
    stop("Common language effect size only applicable to 2-sample rank-biserial correlation.", call. = FALSE)
  }

  out <- lapply(
    rb[, colnames(rb) %in% c("r_rank_biserial", "CI_low", "CI_high")],
    rb_to_p_superiority
  )
  out <- as.data.frame(out)
  colnames(out)[1] <- "Pr(superiority)"

  class(out) <- c("effectsize_table", class(out))
  attr(out, "table_footer") <- "Non-parametric CLES"
  out
}

