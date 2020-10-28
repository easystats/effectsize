
# For standardize_parameters ----------------------------------------------

#' @keywords internal
.get_object <- function(x, attribute_name = "object_name") {
  obj_name <- attr(x, attribute_name, exact = TRUE)
  model <- NULL
  if (!is.null(obj_name)) {
    model <- tryCatch({
      get(obj_name, envir = parent.frame())
    }, error = function(e) {
      NULL
    })
    if (is.null(model) ||
        # prevent self reference
        inherits(model, "parameters_model")) {
      model <- tryCatch({
        get(obj_name, envir = globalenv())
      }, error = function(e) {
        NULL
      })
    }
  }
  model
}



# For standardize_info ----------------------------------------------------

#' @keywords internal
#' @importFrom stats weighted.mean
.mean <- function(x, weights = NULL) {
  if (!.are_weights(weights)) {
    return(mean(x, na.rm = TRUE))
  }

  stopifnot(all(weights > 0, na.rm = TRUE))

  stats::weighted.mean(x, weights, na.rm = TRUE)
}

#' @keywords internal
#' @importFrom stats sd
.sd <- function(x, weights = NULL) {
  # from cov.wt
  if (!.are_weights(weights)) {
    return(stats::sd(x, na.rm = TRUE))
  }

  stopifnot(all(weights > 0, na.rm = TRUE))

  weights1 <- weights / sum(weights)
  center <- sum(weights1 * x)
  xc <- sqrt(weights1) * (x - center)
  var <- (t(xc) %*% xc) / (1 - sum(weights1 ^ 2))
  sqrt(as.vector(var))
}



#' @keywords internal
#' @importFrom stats mad
.mad <- function(x, weights = NULL, constant = 1.4826) {
  # From matrixStats
  if (!.are_weights(weights)) {
    return(stats::mad(x, na.rm = TRUE))
  }

  stopifnot(all(weights > 0, na.rm = TRUE))

  center <- .median(x, weights = weights)
  x <- abs(x - center)
  constant * .median(x, weights = weights)
}



#' @keywords internal
#' @importFrom stats median
.median <- function(x, weights = NULL) {
  # From spatstat + wiki
  if (!.are_weights(weights)) {
    return(stats::median(x, na.rm = TRUE))
  }

  stopifnot(all(weights > 0, na.rm = TRUE))

  oo <- order(x)
  x <- x[oo]
  weights <- weights[oo]
  Fx <- cumsum(weights) / sum(weights)

  lefties <- which(Fx <= 0.5)
  left <- max(lefties)
  if (length(lefties) == 0) {
    result <- x[1]
  } else if (left == length(x)) {
    result <- x[length(x)]
  } else {
    result <- x[left]

    if (!(Fx[left - 1] < 0.5 && 1 - Fx[left] < 0.5)) {
      right <- left + 1
      y <- x[left] * Fx[left] + x[right] * Fx[right]
      if (is.finite(y)) result <- y
    }
  }

  return(result)
}

.are_weights <- function(w) {
  !is.null(w) && length(w) && !all(w == 1) && !all(w == w[1])
}
