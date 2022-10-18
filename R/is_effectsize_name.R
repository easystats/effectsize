#' Checks for a Valid Effect Size Name
#'
#' For use by other functions and packages.
#'
#' @param x A character, or a vector.
#' @param ignore_case Should case of input be ignored?
#' @param use_symbols Should proper symbols be printed (`TRUE`) instead of
#'   transliterated effect size names (`FALSE`). See [effectsize_options].
#' @export
is_effectsize_name <- function(x, ignore_case = TRUE) {
  x_comp <- es_info$name

  if (ignore_case) {
    x <- tolower(x)
    x_comp <- tolower(x_comp)
  }

  x %in% x_comp
}

#' @export
#' @rdname is_effectsize_name
get_effectsize_name <- function(x, ignore_case = TRUE) {
  x[is_effectsize_name(x, ignore_case = ignore_case)]
}

#' @export
#' @rdname is_effectsize_name
get_effectsize_label <- function(x, ignore_case = TRUE, use_symbols = getOption("es.use_symbols", FALSE)) {
  x_comp <- es_info$name
  use_symbols <- .resolve_use_symbols(use_symbols)

  if (ignore_case) {
    x <- tolower(x)
    x_comp <- tolower(x_comp)
  }

  idx <- match(x, x_comp)
  es_info[idx, ifelse(use_symbols, "symbol", "label")]
}


# es_info -----------------------------------------------------------------

# See data-raw/es_info.R

# Utils -------------------------------------------------------------------


#' @keywords internal
.resolve_use_symbols <- function(use_symbols) {
  use_symbols && l10n_info()[["UTF-8"]]
}
