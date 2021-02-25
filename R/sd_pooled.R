#' Pooled Standard Deviation
#'
#' The Pooled Standard Deviation is a weighted average of standard deviations
#' for two or more groups, with more "weight" given to larger sample sizes.
#'
#' @inheritParams cohens_d
#'
#' @return Numeric, the pooled standard deviation.
#'
#' @examples
#' sd_pooled(mpg ~ am, data = mtcars)
#' @seealso [cohens_d()]
#'
#' @export
sd_pooled <- function(x, y = NULL, data = NULL, verbose = TRUE) {

  # This actually works, you must see if you want to keep this code. If you do,
  # following will work:
  # sd_pooled(mpg, hp, data = mtcars)
  # sd_pooled(x, y) # called from a different function, like cohens_d()

  # needs modification in in ".sd_pooled()" as well...

  # x1 <- try(expr = eval(x), silent = TRUE)
  # y1 <- try(expr = eval(y), silent = TRUE)
  #
  # if (inherits(x1, "try-error"))
  #   x <- deparse(substitute(x), width.cutoff = 500)
  # else
  #   x <- x1
  #
  # if (inherits(y1, "try-error"))
  #   y <- deparse(substitute(y), width.cutoff = 500)
  # else
  #   y <- y1

  .sd_pooled(x, y, data, robust = FALSE, verbose = verbose)
}



#' @rdname sd_pooled
#' @export
mad_pooled <- function(x, y = NULL, data = NULL, verbose = TRUE) {
  .sd_pooled(x, y, data, robust = TRUE, verbose = verbose)
}








#' @importFrom stats mad sd as.formula
.sd_pooled <- function(x, y = NULL, data = NULL, robust = FALSE, verbose = TRUE) {

  # Activate here for evaluation of arguments...

  # eval_args <- .evaluate_arguments(x, y, data)
  # out <- .deal_with_cohens_d_arguments(eval_args$x, eval_args$y, eval_args$data)

  out <- .deal_with_cohens_d_arguments(x, y, data, verbose)
  x <- na.omit(out$x)
  y <- na.omit(out$y)

  if (robust) {
    sd1 <- stats::mad(x)
    sd2 <- stats::mad(y)
  } else {
    sd1 <- stats::sd(x)
    sd2 <- stats::sd(y)
  }

  # sqrt((sd1^2 + sd2^2) / 2)

  # Cohen's more complicated formula:
  n1 <- length(x)
  n2 <- length(y)
  sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
}



# .evaluate_arguments <- function(x, y, data) {
#   eval_x <- .evaluate_argument(x)
#   if (!is.null(eval_x$variable)) x <- eval_x$variable
#   if (!is.null(eval_x$data) && is.null(data)) data <- get(eval_x$data)
#
#   eval_y <- .evaluate_argument(y)
#   if (!is.null(eval_y$variable)) y <- eval_y$variable
#   if (!is.null(eval_y$data) && is.null(data)) data <- get(eval_y$data)
#
#   list(x = x, y = y, data = data)
# }


# .evaluate_argument <- function(arg) {
#   data_frame <- NULL
#   if (!is.null(arg)) {
#     if (is.numeric(arg) && length(arg) > 1) {
#       # do nothiung
#     } else if (arg == "NULL") {
#       arg <- NULL
#     } else if (grepl("~", arg, fixed = TRUE)) {
#       arg <- stats::as.formula(arg)
#     } else if (grepl("\"", arg, fixed = TRUE)) {
#       arg <- gsub("\"", "", arg, fixed = TRUE)
#     } else if (grepl("$", arg, fixed = TRUE)) {
#       data_frame <- gsub("(.*)\\$(.*)", "\\1", arg)
#       arg <- gsub("(.*)\\$(.*)", "\\2", arg)
#     }
#   }
#   list(variable = arg, data = data_frame)
# }
