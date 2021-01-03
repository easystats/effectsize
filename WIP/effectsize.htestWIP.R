#'   - A **Wilcoxon test** returns *rank biserial correlation*.
effectsize.htestWIP <- function(model, type = NULL, verbose = TRUE, ...) {
  if (grepl("Wilcoxon", model$method)) {
    # Wilcoxon
    if (is.null(data <- insight::get_data(model))) {
      stop("Unable to retrieve data from htest object.",
           "\nTry using 'cohens_g()' directly.",
           call. = FALSE)
    }

    paired <- grepl("signed rank", model$method, fixed = TRUE)
    mu <- model$null.value

    x <- data$x
    y <- data$y

    out <- rank_biserial(x, y, mu = mu, paired = paired, ...)
    return(out)
  }
}