#' Pooled Standard Deviation
#'
#' The Pooled Standard Deviation is a weighted average of standard deviations for two or more groups, with more "weight" given to larger sample sizes.
#'
#' @inheritParams cohens_d
#'
#' @export
sd_pooled <- function(x, y = NULL, data = NULL) {
  .sd_pooled(x, y, data, robust = FALSE)
}



#' @rdname sd_pooled
#' @export
mad_pooled <- function(x, y = NULL, data = NULL) {
  .sd_pooled(x, y, data, robust = TRUE)
}








#' @importFrom stats mad
#' @export
.sd_pooled <- function(x, y = NULL, data = NULL, robust = FALSE) {
  out <- .deal_with_cohensd_arguments(x, y, data)
  x <- out$x
  y <- out$y

  if(robust){
    sd1 <- mad(x, na.rm = TRUE)
    sd2 <- mad(y, na.rm = TRUE)
  } else{
    sd1 <- sd(x, na.rm = TRUE)
    sd2 <- sd(y, na.rm = TRUE)
  }


  sqrt((sd1^2 + sd2^2)/2)

  # Cohen's more complicated formula:
  # n1 <- length(x)
  # n2 <- length(y)
  # sqrt( (n1-1) * var(x) + (n2-1) * var(y) / n1 + n2 - 2)
}


#' @keywords internal
.deal_with_cohensd_arguments <- function(x, y = NULL, data = NULL){

  # Sanity checks
  if(inherits(x,"formula") | is.character(x) | is.character(y)){
    if(is.null(data)){
      stop("Please provide data argument.")
    }
  }


  # Preprocess data
  if(inherits(x, "formula")){
    data <- model.frame(x, data = data)
    x <- names(data)[1]
    y <- names(data)[2]
  }

  if(is.character(x)){
    x <- data[[x]]
  }

  if(is.character(y)){
    y <- data[[y]]
  }


  # If y is a factor
  if(!is.null(y)){
    if(is.factor(y) | is.character(y)){
      if(length(x) != length(y)){
        stop("The length of the group factor must be the same.")
      }
      if(length(unique(y)) > 2){
        stop("Cannot compute the difference as a factor with more than 2 levels has been provided.")
      } else{
        groups <- as.character(y)
        y <- x[groups == unique(groups)[2]]
        x <- x[groups == unique(groups)[1]]
      }
    }
  }

  list(x = x, y = y)
}