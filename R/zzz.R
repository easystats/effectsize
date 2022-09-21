.onLoad <- function(libname, pkgname) {
  op <- options()
  op.es <- list(
    # es.digits = 2,
    # es.ci = c(0.95, 0.95),
    # es.alt = c("two.sided", "greater"),
    es.use_symbols = TRUE
  )

  toset <- !names(op.es) %in% names(op)
  if(any(toset)) options(op.es[toset])

  invisible(NULL)
}
#
# .onAttach <- function(libname, pkgname) {
#   # packageStartupMessage()
# }