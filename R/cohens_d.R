#' Indices of effect size (Cohen's d, Hedges' g, Glass' delta, ...)
#'
#' Compute different indices of effect size.
#'
#' @param x A continuous variable or a formula.
#' @param y A continuous variable, a factor with two groups or a formula.
#' @param data An optional data frame containing the variables.
#' @param paired Should the values be considered as paired.
#' @param correct For Cohen's d, a correction to the formula to make it less biased for small samples (McGrath & Meyer, 2006). Can also be "raw", in which case the SD from both combined groups is computed instead of the \code{\link{sd_pooled}}.
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
  .effect_size_difference(x, y = y, data = data, type = "d", paired = paired, correct = correct)
}


#' @rdname cohens_d
#' @keywords
glass_delta <- function(x, y = NULL, data = NULL, correct = FALSE) {
  .effect_size_difference(x, y = y, data = data, type = "delta", correct = correct)
}




#' @keywords internal
.effect_size_difference <- function(x, y = NULL, data = NULL, type = "d", paired = FALSE, correct = FALSE) {
  out <- .deal_with_cohensd_arguments(x, y, data)
  x <- out$x
  y <- out$y

  # Compute index
  diff_of_means  <- mean(y, na.rm = TRUE) - mean(x, na.rm = TRUE)


  if(type == "d"){
    if(paired){
      denominator <- sd(x-y, na.rm = TRUE)
    } else {
      denominator <- sd_pooled(x, y)
    }
  } else if (type == "delta"){
    denominator <- sd(x, na.rm = TRUE)
  }



  d <- diff_of_means / denominator


  # McGrath & Meyer (2006)
  if(correct == TRUE){
    n <- length(c(x, y))
    d <- d * ((n - 3)/(n-2.25)) * ((n - 2)/n)
  } else if(correct == "raw"){
    d <- diff_of_means / sd(c(x, y), na.rm = TRUE)
  }

  d
}