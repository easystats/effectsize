#' Methods for `effectsize` tables
#'
#' Printing, formatting and plotting methods for `effectsize` tables.
#'
#' @param x Object to print.
#' @inheritParams insight::format_value
#' @param ... Arguments passed to or from other functions.
#'
#' @export
print.effectsize_table <- function(x, digits = 2, ...) {
  x_orig <- x

  if (!is.null(alt <- attr(x, "alternative")) && alt != "two.sided") {
    ci_footer <- sprintf(
      "\n- One-sided CIs: %s bound fixed at (%s).",
      if (alt == "less") "lower" else "upper",
      as.character(if (alt == "less") x$CI_low[1] else x$CI_high[1])
    )

    attr(x, "table_footer") <-
      c(attr(x, "table_footer"), list(c(ci_footer, "cyan")))
  }

  x <- format(x, digits = digits)

  cat(insight::export_table(x, digits = digits, ...))

  invisible(x_orig)
}

#' @rdname print.effectsize_table
#' @export
format.effectsize_table <- function(x, digits = 2, ...) {
  i <- is_effectsize_name(colnames(x))
  labs <- get_effectsize_label(colnames(x))
  colnames(x)[i] <- labs[i]

  attr(x, "ci") <- NULL
  attr(x, "ci_method") <- NULL

  out <- insight::format_table(x, digits = digits, ci_digits = digits, preserve_attributes = TRUE, ...)
  if (!is.null(rule_name <- attr(x, "rule_name", exact = TRUE))) {
    attr(out, "table_footer") <- c(
      attr(out, "table_footer"),
      list(c(paste0("\n(Interpretation rule: ", rule_name, ")"), "blue"))
    )
  }
  out
}


# Print Methods --------------------------------------------------------

#' @export
print.effectsize_std_params <- function(x, digits = 2, ...) {
  x_orig <- x

  footer <- caption <- subtitle <- NULL

  caption <- c(sprintf("# Standardization method: %s", attr(x, "std_method")), "blue")

  # robust / two_sd
  if (attr(x, "two_sd") || attr(x, "robust")) {
    footer <- sprintf(
      "\n- Scaled by %s %s.\n",
      ifelse(attr(x, "two_sd"), "two", "one"),
      ifelse(attr(x, "robust"), "MAD(s) from the median", "SD(s) from the mean")
    )
    footer <- c(footer, "cyan")
  }

  # include_response
  if (!attr(x, "include_response")) {
    msg <- "(Response is unstandardized)\n"
    if (length(footer)) {
      footer[1] <- paste0(footer[1], msg)
    } else {
      footer <- paste0("\n", msg)
      footer <- c(footer, "cyan")
    }
  }

  attr(x, "table_footer") <- footer
  attr(x, "table_caption") <- caption
  attr(x, "table_subtitle") <- subtitle
  print.effectsize_table(x, digits = digits, ...)
  invisible(x_orig)
}


#' @export
print.equivalence_test_effectsize <- function(x, digits = 2, ...) {
  x_orig <- x

  caption <- footer <- subtitle <- NULL

  ## Title (caption)
  if (attr(x, "rule", exact = TRUE) == "cet") {
    caption <- "# Conditional Test for Practical Equivalence\n"
  } else {
    caption <- "# Test for Practical Equivalence\n"
  }
  caption <- c(caption, "blue")


  ## Rope range
  .rope <- attr(x, "rope", exact = TRUE)
  subtitle <- sprintf("\tROPE: [%.*f %.*f]", digits, .rope[1], digits, .rope[2])


  ## ROPE_Equivalence
  if (attr(x, "rule", exact = TRUE) == "bayes") {
    footer <- c("\n(Using Bayesian guidlines)", "green")
  }


  attr(x, "table_footer") <- footer
  attr(x, "table_caption") <- caption
  attr(x, "table_subtitle") <- subtitle
  print.effectsize_table(x, digits = digits, ...)
  invisible(x_orig)
}

#' @export
#' @rdname print.effectsize_table
#' @param append_CL Should the Common Language Effect Sizes be printed as well?
#'   Only applicable to Cohen's *d*, Hedges' *g* for independent samples of
#'   equal variance (pooled sd) (See [d_to_common_language()])
print.effectsize_difference <- function(x, digits = 2, append_CL = FALSE, ...) {
  x_orig <- x

  footer <- caption <- subtitle <- NULL

  ## Add footer
  mu <- attr(x, "mu")
  if (mu != 0) {
    mu <- sprintf("\n- Deviation from a difference of %s.", mu)
    footer <- c(footer, list(c(mu, "cyan")))
  }

  if (!is.null(sd_type <- attr(x, "pooled_sd", exact = TRUE))) {
    sd_type <- sprintf(
      "\n- Estimated using %s.",
      ifelse(sd_type, "pooled SD", "un-pooled SD")
    )

    footer <- c(footer, list(c(sd_type, "cyan")))
  }

  attr(x, "table_footer") <- footer
  attr(x, "table_caption") <- caption
  attr(x, "table_subtitle") <- subtitle
  print.effectsize_table(x, digits = digits, ...)


  if (append_CL) {
    if ("r_rank_biserial" %in% colnames(x_orig)) {
      to_cl_coverter <- rbs_to_common_language
    } else {
      to_cl_coverter <- d_to_common_language
    }

    tryCatch({
      CL <- to_cl_coverter(x_orig)
      attr(CL, "table_caption") <- c("\n\n# Common Language Effect Sizes", "blue")
      print(CL, digits = digits)
    }, error = function(...) invisible(NULL))
  }

  invisible(x_orig)
}


#' @export
#' @importFrom utils as.roman
print.effectsize_anova <- function(x, digits = 2, ...) {
  x_orig <- x

  footer <- caption <- subtitle <- NULL

  ## Title (caption)
  anova_type <- attr(x, "anova_type", exact = TRUE)
  if (is.null(anova_type) || is.na(anova_type)) {
    caption <- "# Effect Size for ANOVA"
  } else {
    caption <- paste0("# Effect Size for ANOVA (Type ", utils::as.roman(anova_type), ")")
  }
  caption <- c(caption, "blue")

  ## Footer
  obs <- attr(x, "generalized")
  if (is.character(obs) || isTRUE(obs)) {
    if (isTRUE(obs)) {
      footer <- "\n- Observed variabels: All"
    } else {
      footer <- paste0("\n- Observed variabels: ", paste0(obs, collapse = ", "))
    }
    footer <- list(c(footer, "cyan"))
  }

  attr(x, "table_footer") <- footer
  attr(x, "table_caption") <- caption
  attr(x, "table_subtitle") <- subtitle
  print.effectsize_table(x, digits = digits, ...)
  invisible(x_orig)
}


# Format Methods --------------------------------------------------------

#' @export
format.equivalence_test_effectsize <- function(x, digits = 2, ...) {
  colnames(x)[colnames(x) == "ROPE_Equivalence"] <- "H0"
  format.effectsize_table(x, digits = digits, ...)
}
