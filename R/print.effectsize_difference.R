#' @export
#' @rdname cohens_d
#' @inheritParams insight::format_value
#' @param append_CL Should the Common Language Effect Sizes be printed as well?
#'   Not applicable to Glass' Delta (See [d_to_common_language()])
#' @param ... Not used.
print.effectsize_difference <- function(x, digits = 2, append_CL = FALSE, ...) {
  x_orig <- x
  print.effectsize_table(x, digits = digits)

  ## Add note
  # note <- NULL
  # if (any(colnames(x) %in% c("Cohens_d", "Hedges_g"))) {
  #   note <- c(note, ifelse(attr(x, "pooled_sd"), "Pooled SD", "Unpooled SD"))
  # }
  # if (attr(x, "correction")) {
  #   note <- c(note, "Bias-corrected")
  # }
  # if (!is.null(note)) cat(c("\n[",paste0(note, collapse = "; ")), "]\n")

  ## Common lang
  if (append_CL && any(colnames(x) %in% c("Cohens_d", "Hedges_g"))) {
    cl <- d_to_common_language(x[[any(colnames(x) %in% c("Cohens_d", "Hedges_g"))]])
    cl <- sapply(cl, function(ff) sprintf("%g%% CI", round(ff * 100, digits = digits)))
    cl <- paste(paste0("* ", names(cl),": ",cl), collapse = "\n")
    cat("\n")
    insight::print_color(cl, "cyan")
  }

  invisible(x_orig)
}