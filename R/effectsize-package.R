#' \code{effectsize}
#'
#' @title effectsize: Indices of Effect Size
#'
#' @description
#'
#' In both theoretical and applied research, it is often of interest to assess
#' the strength of an observed association. This is typically done to allow the
#' judgment of the magnitude of an effect, especially when units of measurement
#' are not meaningful. Though some indices of effect size, such as the
#' correlation coefficient (itself a standardized covariance coefficient) are
#' readily available, other measures are often harder to obtain.
#'
#' **effectsize** fills this important gap, providing utilities for easily
#' estimating a wide variety of standardized effect sizes (i.e., effect sizes
#' that are not tied to the units of measurement of the variables of interest)
#' and their confidence intervals (CIs), from a variety of statistical models
#' and hypothesis tests, such as [cohens_d()], [phi()], [eta_squared()], and
#' many more.
#'
#' See `vignette("effectsize", package = "effectsize")` for more details,
#' or `vignette(package = "effectsize")` for a full list of vignettes.
#'
#' References: Ben-Shachar et al. (2020) <doi:10.21105/joss.02815>.
#'
#' @docType package
#' @aliases effectsize-package
#' @name effectsize-package
#' @keywords internal
"_PACKAGE"