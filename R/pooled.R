#' Pooled Standard Deviation
#'
#' The Pooled Standard Deviation is a weighted average of standard deviations
#' for two or more groups, *assumed to have equal variance*. It represents the
#' common deviation among the groups, around each of their respective means.
#'
#' @inheritParams cohens_d
#' @inheritParams stats::mad
#'
#' @details
#' The standard version is calculated as:
#' \deqn{\sqrt{\frac{\sum (x_i - \bar{x})^2}{n_1 + n_2 - 2}}}{sqrt(sum(c(x - mean(x), y - mean(y))^2) / (n1 + n2 - 2))}
#' The robust version is calculated as:
#' \deqn{1.4826 \times Median(|\left\{x - Median_x,\,y - Median_y\right\}|)}{mad(c(x - median(x), y - median(y)), constant = 1.4826)}
#'
#' @return Numeric, the pooled standard deviation. For `cov_pooled()` a matrix.
#'
#' @examples
#' sd_pooled(mpg ~ am, data = mtcars)
#' mad_pooled(mtcars$mpg, factor(mtcars$am))
#'
#' cov_pooled(mpg + hp + cyl ~ am, data = mtcars)
#'
#'
#' @seealso [cohens_d()], [mahalanobis_D()]
#'
#' @export
sd_pooled <- function(x, y = NULL, data = NULL, verbose = TRUE, ...) {
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

  .sd_pooled(x, y, data, robust = FALSE, verbose = verbose, ...)
}



#' @rdname sd_pooled
#' @export
mad_pooled <- function(x, y = NULL, data = NULL, constant = 1.4826, verbose = TRUE, ...) {
  .sd_pooled(x, y, data, robust = TRUE, verbose = verbose, constant = constant, ...)
}

#' @rdname sd_pooled
#' @export
cov_pooled <- function(x, y = NULL, data = NULL, verbose = TRUE, ...) {
  data <- .get_data_multivariate(x, y, data = data, verbose = verbose)
  x <- data[["x"]]
  y <- data[["y"]]

  n1 <- nrow(x)
  n2 <- nrow(y)

  S1 <- cov(x)
  S2 <- cov(x)
  (S1 * (n1 - 1) + S2 * (n2 - 1)) / (n1 + n2 - 2)
}


# Utils -------------------------------------------------------------------



#' @importFrom stats mad sd as.formula ave
.sd_pooled <- function(x, y = NULL, data = NULL, robust = FALSE, verbose = TRUE, constant = 1, ...) {
  # Activate here for evaluation of arguments...

  # eval_args <- .evaluate_arguments(x, y, data)
  # out <- .get_data_2_samples(eval_args$x, eval_args$y, eval_args$data, verbose, ...)

  out <- .get_data_2_samples(x, y, data, verbose, ...)
  x <- na.omit(out$x)
  y <- na.omit(out$y)

  if (robust) {
    f <- constant
    center <- stats::median
    div <- stats::mad
  } else {
    n1 <- length(x)
    n2 <- length(y)

    f <- sqrt(c(n1 + n2 - 1) / c(n1 + n2 - 2))
    center <- mean
    div <- stats::sd
  }

  div(c(
    x - stats::ave(x, FUN = center),
    y - stats::ave(y, FUN = center)
  )) * f
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
