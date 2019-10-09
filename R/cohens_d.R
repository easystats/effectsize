#' Indices of effect size (Cohen's d, Hedges' g, ...)
#'
#' Compute different indices of effect size.
#'
#' @param x A continuous variable or a formula.
#' @param y A continuous variable, a factor with two groups or a formula.
#' @param data An optional data frame (or similar: see model.frame) containing the variables.
#' @examples
#' cohens_d(iris$Sepal.Length, iris$Sepal.Width)
#'
#' @importFrom stats var
#' @export
cohens_d <- function(x, y = NULL, data = NULL) {

  # Sanity checks
  # TODO: check if same length

  # Preprocess data
  if(is.character(x)){
    if(is.null(data)){
      stop("Please provide data argument")
    } else{
      x <- data[[x]]
    }
  }

  if(is.character(y)){
    if(is.null(data)){
      stop("Please provide data argument")
    } else{
      y <- data[[y]]
    }
  }


  # If y is a factor
  if(!is.null(y)){
    if(is.factor(y) | is.character(y)){
      if(length(unique(y)) > 2){
        stop("Cannot compute the difference as a factor with more than 2 levels has been provided.")
      } else{
        groups <- as.character(y)
        y <- x[groups == unique(groups)[2]]
        x <- x[groups == unique(groups)[1]]
      }
    }
  }


  # Compute index
  n_x <- length(x) - 1
  n_y <- length(y) - 1

  diff_of_means  <- abs(mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)) # mean difference

  common_std <- n_x * var(x, na.rm = TRUE) + n_y * var(y, na.rm = TRUE)
  denominator <- sqrt(common_std/(n_x + n_y))

  diff_of_means/denominator
}