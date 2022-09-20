#' Methods for `{effectsize}` Tables
#'
#' Printing, formatting and plotting methods for `effectsize` tables.
#'
#' @param x Object to print.
#' @inheritParams insight::format_value
#' @inheritParams is_effectsize_name
#' @param ... Arguments passed to or from other functions.
#'
#' @seealso [insight::display()]
#'
#' @export
print.effectsize_table <- function(x, digits = 2, use_symbols = getOption("es.use_symbols", FALSE), ...) {
  x_fmt <- format(x, digits = digits, output = "text", use_symbols = use_symbols, ...)
  cat(insight::export_table(x_fmt, format = NULL, ...))
  invisible(x)
}

#' @export
#' @rdname print.effectsize_table
print_md.effectsize_table <- function(x, digits = 2, use_symbols = getOption("es.use_symbols", FALSE), ...) {
  x_fmt <- format(x, digits = digits, output = "markdown", ...)
  insight::export_table(x_fmt, format = "markdown", ...)
}

#' @export
#' @rdname print.effectsize_table
print_html.effectsize_table <- function(x, digits = 2, use_symbols = getOption("es.use_symbols", FALSE),  ...) {
  x_fmt <- format(x, digits = digits, output = "html", ...)
  insight::export_table(x_fmt, format = "html", ...)
}

#' @rdname print.effectsize_table
#' @param output Which output is the formatting intended for? Affects how title
#'   and footers are formatted.
#' @export
format.effectsize_table <- function(x, digits = 2, output = c("text", "markdown", "html"), use_symbols = getOption("es.use_symbols", FALSE), ...) {
  output <- match.arg(output)

  ## Clean footer
  footer <- attr(x, "table_footer")

  if (!is.null(alt <- attr(x, "alternative")) && alt != "two.sided") {
    bound <- if (alt == "less") x$CI_low[1] else x$CI_high[1]
    bound_ <- insight::format_value(bound, digits = digits)
    if (!is.character(digits) &&
      !isTRUE(all.equal(bound, as.numeric(bound_)))) {
      bound_ <- paste0(bound_, "~")
    }

    side <- if (alt == "less") "lower" else "upper"

    ci_footer <- sprintf(
      "One-sided CIs: %s bound fixed at [%s].",
      side, bound_
    )
    footer <- c(footer, ci_footer)
  }

  # if (isTRUE(attr(x, "approximate"))) {
  #   approx_footer <- "Effect size is approximated."
  #   footer <- c(footer, approx_footer)
  # }

  if (!is.null(rule_name <- attr(attr(x, "rules"), "rule_name", exact = TRUE))) {
    rule_footer <- sprintf("Interpretation rule: %s", rule_name)
    footer <- c(footer, rule_footer)
  }

  if (output == "text" && !is.null(footer)) {
    footer <- lapply(footer, function(ftr) {
      c(paste0("\n- ", ftr), .pcl["footer"])
    })
  }
  attr(x, "table_footer") <- footer


  ## Clean caption
  caption <- attr(x, "table_caption")
  if (output == "text" && !is.null(caption)) {
    caption <- c(paste0("# ", caption), .pcl["title"])
  }
  attr(x, "table_caption") <- caption


  ## Clean subtitle
  subtitle <- attr(x, "table_subtitle")
  if (output == "text" && !is.null(subtitle)) {
    subtitle <- c(paste0("\n", subtitle), .pcl["subtitle"])
  }
  attr(x, "table_subtitle") <- subtitle


  ## Clean column names
  i <- is_effectsize_name(colnames(x))
  labs <- get_effectsize_label(colnames(x), use_symbols = use_symbols)
  colnames(x)[i] <- labs[i]

  attr(x, "ci") <- NULL
  attr(x, "ci_method") <- NULL

  insight::format_table(x,
    digits = digits, ci_digits = digits,
    preserve_attributes = TRUE, ...
  )
}


# Print -------------------------------------------------------------------

#' @export
#' @rdname print.effectsize_table
#' @param append_CLES Which Common Language Effect Sizes should be printed as
#'   well? Only applicable to Cohen's *d*, Hedges' *g* for independent samples
#'   of equal variance (pooled sd) or for the rank-biserial correlation for
#'   independent samples (See [d_to_cles]).
print.effectsize_difference <- function(x, digits = 2, append_CLES = NULL, ...) {
  x_orig <- x

  print.effectsize_table(x, digits = digits, ...)

  if (is.logical(append_CLES) || is.character(append_CLES)) {
    if (isTRUE(attr(x_orig, "paired"))) {
      stop("CLES only applicable to independent samples.")
    }

    if (colnames(x_orig)[1] == "r_rank_biserial") {
      cles_tab <- rb_to_p_superiority(x_orig)
    } else if (colnames(x_orig)[1] %in% c("Cohens_d", "Hedges_g")) {
      if (!isTRUE(attr(x_orig, "pooled_sd"))) {
        stop("CLES only applicable to pooled-sd Cohen's d / Hedge's g.")
      }

      if (isTRUE(append_CLES)) {
        append_CLES <- c("p_superiority", "u1", "u2", "u3", "overlap")
      }

      foos <- lapply(paste0("d_to_", append_CLES), match.fun)
      cles <- lapply(foos, function(f) f(x_orig))
      names(cles) <- sapply(cles, function(x) colnames(x)[1])
      cles <- lapply(cles, function(x) {colnames(x)[1] <- "CLES"; x})

      cles_tab <- do.call(rbind, cles)
      cles_tab$Name <- get_effectsize_label(names(cles))

      cles_tab <- cles_tab[c(ncol(cles_tab), seq_len(ncol(cles_tab)-1))]

    } else {
      stop("CLES not applicable for this effect size.")
    }

    insight::print_color("\n\n## Common Language Effect Sizes:\n", .pcl["subtitle"])
    print(cles_tab, digits = digits, ...)
  }

  invisible(x_orig)
}

# Format ------------------------------------------------------------------

#' @export
format.effectsize_difference <- function(x, digits = 2, ...) {
  caption <- subtitle <- footer <- NULL

  ## Add footer
  mu <- attr(x, "mu")
  if (mu != 0) {
    mu_footer <- sprintf("Deviation from a difference of %g.", mu)
    footer <- c(footer, mu_footer)
  }

  if (!is.null(sd_type <- attr(x, "pooled_sd", exact = TRUE))) {
    sd_type <- sprintf(
      "Estimated using %spooled SD.",
      ifelse(sd_type, "", "un-")
    )

    footer <- c(footer, sd_type)
  }

  attr(x, "table_footer") <- footer
  attr(x, "table_caption") <- caption
  attr(x, "table_subtitle") <- subtitle
  format.effectsize_table(x, digits = digits, ...)
}

#' @export
#' @importFrom utils as.roman
format.effectsize_anova <- function(x, digits = 2, ...) {
  footer <- caption <- subtitle <- NULL

  ## Title (caption)
  anova_type <- attr(x, "anova_type", exact = TRUE)
  if (is.null(anova_type) || is.na(anova_type)) {
    type <- ""
  } else {
    type <- sprintf(" (Type %s)", utils::as.roman(anova_type))
  }
  caption <- sprintf("Effect Size for ANOVA%s", type)

  ## Footer
  obs <- attr(x, "generalized")
  if (is.character(obs) || isTRUE(obs)) {
    if (isTRUE(obs)) {
      obs <- "All"
    } else {
      obs <- paste0(obs, collapse = ", ")
    }
    gen_footer <- sprintf("Observed variables: %s", obs)
    footer <- c(footer, gen_footer)
  }

  attr(x, "table_footer") <- footer
  attr(x, "table_caption") <- caption
  attr(x, "table_subtitle") <- subtitle
  format.effectsize_table(x, digits = digits, ...)
}

#' @export
format.equivalence_test_effectsize <- function(x, digits = 2, ...) {
  colnames(x)[colnames(x) == "ROPE_Equivalence"] <- "H0"

  caption <- footer <- subtitle <- NULL

  ## Title (caption)
  if (attr(x, "rule", exact = TRUE) == "cet") {
    rule <- "Conditional "
  } else {
    rule <- ""
  }
  caption <- sprintf("%sTest for Practical Equivalence", rule)


  ## Rope range
  .rope <- attr(x, "rope", exact = TRUE)
  .rope <- insight::format_value(.rope, digits = digits)
  subtitle <- sprintf("ROPE: [%s, %s]", .rope[1], .rope[2])


  ## ROPE_Equivalence
  if (attr(x, "rule", exact = TRUE) == "bayes") {
    footer <- "Using Bayesian guidlines"
  }

  attr(x, "table_footer") <- footer
  attr(x, "table_caption") <- caption
  attr(x, "table_subtitle") <- subtitle
  format.effectsize_table(x, digits = digits, ...)
}


# Colors ------------------------------------------------------------------

.pcl <- c(title = "blue", subtitle = "blue", footer = "cyan", interpret = "italic")

# "red", "yellow", "green", "blue", "violet","cyan", "grey", "bold", "italic"
