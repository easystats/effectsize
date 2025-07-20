#' Deprecated / Defunct Functions
#'
#' @param ... Arguments to the deprecated function.

#' @aliases probs_to_odds.data.frame
#' @aliases oods_to_probs.data.frame
#'
#' @rdname effectsize_deprecated
#' @name effectsize_deprecated
NULL


# July 2025 ---------------------------------------------------------------

#' @keywords internal
.deprecated_df_methods <- function(funname) {
  insight::format_error(
    sprintf("%s(<data.frame>) is deprecated.", funname),
    "You can use:",
    sprintf("datawizard::data_modify(data, .at = ..., .modify = %s)", funname),
    "Or",
    sprintf("dplyr::mutate(data, dplyr::across(..., %s))", funname),
    "Instead."
  )
}
