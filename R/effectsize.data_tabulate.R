
# Cross-tab ---------------------------------------------------------------

#' @export
#' @rdname effectsize
#' @param drop Should empty rows/cols be dropped?
effectsize.datawizard_crosstabs <- function(model, type = NULL,
                                            drop = TRUE,
                                            verbose = TRUE, ...) {
  tab <- .get_dw_table(model, drop = drop, verbose = verbose)

  if (is.null(type)) type <- "cramers_v"

  f <- switch(tolower(type),
              v = ,
              cramers_v = cramers_v,
              t = ,
              tschuprows_t = tschuprows_t,
              w = ,
              cohens_w = cohens_w,
              phi = phi,
              c = ,
              pearsons_c = pearsons_c,
              or = ,
              oddsratio = oddsratio,
              rr = ,
              riskratio = riskratio,
              h = ,
              cohens_h = cohens_h,
              arr = arr,
              nnt = nnt
  )

  f(tab, verbose = verbose, ...)
}

#' @export
#' @rdname effectsize
effectsize.datawizard_crosstab <- effectsize.datawizard_crosstabs


# 1D tables ---------------------------------------------------------------

#' @export
#' @rdname effectsize
effectsize.datawizard_tables <- function(model, type = NULL,
                                         drop = TRUE,
                                         verbose = TRUE, ...) {
  tab <- .get_dw_table(model, drop = drop, verbose = verbose)

  if (is.null(type)) type <- "fei"

  f <- switch(tolower(type),
              w = ,
              cohens_w = cohens_w,
              c = ,
              pearsons_c = pearsons_c,
              fei = fei
  )

  f(tab, verbose = verbose, ...)
}

#' @export
#' @rdname effectsize
effectsize.datawizard_table <- effectsize.datawizard_tables


# Utils -------------------------------------------------------------------

#' @keywords internal
.get_dw_table <- function(x, drop = TRUE, verbose = TRUE) {
  ltab <- as.table(x, remove_na = drop,
                   verbose = verbose, simplify = FALSE)
  if (length(ltab) > 1L) {
    insight::format_error("Multilpe tables not yet supported.",
                          "Try lapply(model, effectsize, ...).")
  }

  tab <- ltab[[1]]

  if (length(dim(tab)) > 1L && isTRUE(drop) && any(tab==0)) {
    tab <- tab[rowSums(tab, na.rm = TRUE) > 0,
               colSums(tab, na.rm = TRUE) > 0]
  }

  tab
}
