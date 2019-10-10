#' Indices of effect size (Cohen's d, Hedges' g, ...)
#'
#' Compute different indices of effect size.
#'
#' @param x A continuous variable or a formula.
#' @param y A continuous variable, a factor with two groups or a formula.
#' @param data An optional data frame containing the variables.
#' @param paired Should the values be considered as paired.
#' @param correct For Cohen's d, a correction to the formula to make it less biased for small samples (McGrath & Meyer, 2006).
#'
#' @examples
#' cohens_d(iris$Sepal.Length, iris$Sepal.Width)
#' cohens_d(Sepal.Length ~ Species, data = iris[iris$Species %in% c("versicolor", "setosa"), ])
#'
#' @references \itemize{
#'  \item Cohen, J. (2013). Statistical power analysis for the behavioral sciences. Routledge.
#'  \item McGrath, R. E., & Meyer, G. J. (2006). When effect sizes disagree: the case of r and d. Psychological methods, 11(4), 386.
#' }
#' @importFrom stats var model.frame
#' @export
cohens_d <- function(x, y = NULL, data = NULL, paired = FALSE, correct = FALSE) {

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


  # Compute index
  diff_of_means  <- mean(y, na.rm = TRUE) - mean(x, na.rm = TRUE)


  if(paired){
    denominator <- sd(x-y, na.rm = TRUE)
  } else {
    denominator <- .get_pooled_sd(x, y, correct = correct)
  }


  # Raw method
  # denominator <- sd(c(x, y), na.rm = TRUE)



  diff_of_means / denominator
}





#' @keywords internal
.get_pooled_sd <- function(x, y, correct = TRUE) {

  s1 <- var(x)
  s2 <- var(y)
  n1 <- length(x)
  n2 <- length(y)

  numerator <- (n1-1) * s1 + (n2-1) * s2

  # McGrath & Meyer (2006)
  if(correct){
    n <- n1 + n2
    denominator <- n * (n - 3)/(n-2.25)
  } else{
    denominator <- n1 + n2 - 2
  }
  sqrt(numerator / denominator)
}

