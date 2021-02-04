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

  insight::format_table(x, digits = digits, ci_digits = digits, preserve_attributes = TRUE, ...)
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

  if (any(colnames(x) == "Hedges_g")) {
    correction <- sprintf(
      "\n- Bias corrected using %s method.",
      ifelse(attr(x, "correction") == 1, "Hedges and Olkin's", "Hunter and Schmidt's")
    )

    footer <- c(footer, list(c(correction, "cyan")))
  }


  attr(x, "table_footer") <- footer
  attr(x, "table_caption") <- caption
  attr(x, "table_subtitle") <- subtitle
  print.effectsize_table(x, digits = digits, ...)


  if (append_CL) {
    if (any(colnames(x_orig) %in% c("Cohens_d", "Hedges_g")) &&
      attr(x_orig, "pooled_sd") &&
      !attr(x_orig, "paired")) {
      # Common lang
      cl <- d_to_common_language(x_orig[[any(colnames(x_orig) %in% c("Cohens_d", "Hedges_g"))]])
      cl <- lapply(cl, insight::format_value, as_percent = TRUE, digits = digits)
      cl <- data.frame(cl, check.names = FALSE)
      cat(insight::export_table(cl,
        digits = digits,
        caption = c("\n\n# Common Language Effect Sizes", "blue"), ...
      ))
    }
  }

  invisible(x_orig)
}



# Format Methods --------------------------------------------------------

#' @export
format.equivalence_test_effectsize <- function(x, digits = 2, ...) {
  colnames(x)[colnames(x) == "ROPE_Equivalence"] <- "H0"
  format.effectsize_table(x, digits = digits, ...)
}
