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
#' @seealso [cohens_d()], [mahalanobis_d()]
#'
#' @importFrom stats ave sd
#' @export
sd_pooled <- function(x, y = NULL, data = NULL, verbose = TRUE, ...) {
  data <- .get_data_2_samples(x, y, data, verbose, ...)
  x <- na.omit(data$x)
  y <- na.omit(data$y)

  V <- cov_pooled(data.frame(x = x),
                  data.frame(x = y))
  c(sqrt(V))
}



#' @rdname sd_pooled
#' @importFrom stats ave mad median
#' @export
mad_pooled <- function(x, y = NULL, data = NULL, constant = 1.4826, verbose = TRUE, ...) {
  data <- .get_data_2_samples(x, y, data, verbose, ...)
  x <- na.omit(data$x)
  y <- na.omit(data$y)

  n1 <- length(x)
  n2 <- length(y)

  Y <- c(x, y)
  G <- rep(1:2, times = c(n1, n2))
  Yc <- Y - stats::ave(Y, factor(G), FUN = stats::median)

  stats::mad(Yc, center = 0, constant = constant)
}


#' @rdname sd_pooled
#' @importFrom stats cov
#' @export
cov_pooled <- function(x, y = NULL, data = NULL, verbose = TRUE, ...) {
  data <- .get_data_multivariate(x, y, data = data, verbose = verbose)
  x <- na.omit(data[["x"]])
  y <- na.omit(data[["y"]])

  n1 <- nrow(x)
  n2 <- nrow(y)

  Y <- rbind(x, y)
  G <- rep(1:2, times = c(n1, n2))
  Yc <- lapply(Y, function(.y) .y - stats::ave(.y, factor(G), FUN = mean))
  Yc <- as.data.frame(Yc)

  stats::cov(Yc) * (n1 + n2 - 1) / (n1 + n2 - 2)
}

# TODO Add com_pooled?