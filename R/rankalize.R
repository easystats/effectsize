#' (Signed) rank transformation
#'
#' Transform numeric values with the integers of their rank (i.e., 1st smallest, 2nd smallest, 3rd smallest, etc.). Setting the \code{sign} argument to \code{TRUE} will give you signed ranks, where the ranking is done according to absolute size but where the sign is presereved (i.e., 2, 1, -3, 4).
#'
#' @inheritParams standardize.data.frame
#'
#' @param x Object.
#' @param sign Logical, if \code{TRUE}, return signed ranks.
#' @param method Treatment of ties. Can be one of "average" (default), "first", "last", "random", "max" or "min". See \code{\link{rank}} for details.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' rankalize(c(0, 1, 5, -5, -2))
#' rankalize(c(0, 1, 5, -5, -2), sign = TRUE)
#'
#' head(rankalize(iris))
#'
#' @return A rank-transformed object.
#' @export
rankalize <- function(x, ...) {
  UseMethod("rankalize")
}






#' @rdname rankalize
#' @export
rankalize.numeric <- function(x, sign = FALSE, method = "average", verbose = TRUE, ...) {

  # Warning if all NaNs
  if (all(is.na(x))) {
    return(x)
  }


  # Warning if only one value
  if (length(unique(x)) == 1) {
    if (is.null(names(x))) {
      name <- deparse(substitute(x))
    } else {
      name <- names(x)
    }
    if (verbose) {
      warning(paste0("Variable `", name, "` contains only one unique value and will not be normalized."))
    }
    return(x)
  }


  # Warning if logical vector
  if (length(unique(x)) == 2) {
    if (is.null(names(x))) {
      name <- deparse(substitute(x))
    } else {
      name <- names(x)
    }
    if (verbose) {
      warning(paste0("Variable `", name, "` contains only two different values. Consider converting it to a factor."))
    }
  }


  if(sign){
    out <- sign(x) * rank(x, ties.method = method, na.last = "keep")
  } else{
    out <- rank(x, ties.method = method, na.last = "keep")
  }

  out
}








#' @export
rankalize.factor <- function(x, ...) {
  x
}




#' @rdname rankalize
#' @export
rankalize.grouped_df <- function(x, select = NULL, exclude = NULL, sign = FALSE, method = "average", ...) {
  info <- attributes(x)
  # dplyr >= 0.8.0 returns attribute "indices"
  grps <- attr(x, "groups", exact = TRUE)

  # dplyr < 0.8.0?
  if (is.null(grps)) {
    grps <- attr(x, "indices", exact = TRUE)
    grps <- lapply(grps, function(x) x + 1)
  } else {
    grps <- grps[[".rows"]]
  }

  x <- as.data.frame(x)
  for (rows in grps) {
    x[rows, ] <- rankalize(
      x[rows, ],
      select = select,
      exclude = exclude,
      sign = sign,
      method = method,
      ...
    )
  }
  # set back class, so data frame still works with dplyr
  attributes(x) <- info
  x
}


#' @rdname rankalize
#' @export
rankalize.data.frame <- function(x, select = NULL, exclude = NULL, sign = FALSE, method = "average", ...) {
  if (is.null(select)) {
    select <- names(x)
  }

  if (!is.null(exclude)) {
    select <- setdiff(select, exclude)
  }

  x[select] <- lapply(x[select], rankalize, sign = sign, method = method)
  x
}
