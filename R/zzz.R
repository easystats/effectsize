#
# .onLoad <- function(libname, pkgname) {
#   op <- options()
#   op.es <- list(
#     es.ci.mag = 0.95,
#     es.ci.diff = 0.95,
#     es.alt.mag = "greater",
#     es.alt.diff = "two.sided"
#   )
#
#   toset <- !names(op.es) %in% names(op)
#   if(any(toset)) options(op.afex[toset])
#
#   invisible(NULL)
# }
#
# .onAttach <- function(libname, pkgname) {
#   # packageStartupMessage()
# }
