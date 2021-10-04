#' @export
#' @rdname effectsize
#' @inheritParams bayestestR::describe_posterior
#' @importFrom insight get_data get_parameters check_if_installed
#' @importFrom bayestestR describe_posterior
effectsize.BFBayesFactor <- function(model, type = NULL, verbose = TRUE, test = NULL, ...) {
  insight::check_if_installed("BayesFactor")

  xtra_class <- xtra_footer <- mu <- paired <- NULL

  if (length(model) > 1) {
    if (verbose) {
      warning("Multiple models detected. Using first only.", call. = FALSE)
    }
    model <- model[1]
  }

  if (inherits(model@numerator[[1]], "BFcontingencyTable")) {
    # Chisq ----
    if (is.null(type)) type <- "cramers_v"

    f <- switch(tolower(type),
                v = ,
                cramers_v = cramers_v,
                w = ,
                cohens_w = ,
                phi = phi,
                c = ,
                pearsons_c = pearsons_c,
                h = ,
                cohens_h = cohens_h,
                or = ,
                oddsratio = oddsratio,
                rr = ,
                riskratio = riskratio
    )
    data <- insight::get_data(model)
    posts <- insight::get_parameters(model)

    ES <- apply(posts, 1, function(a) {
      f(matrix(a, nrow = nrow(data)), ci = NULL)[[1]]
    })

    res <- data.frame(ES)
    colnames(res) <- colnames(f(data, ci = NULL))
  } else if (inherits(model@numerator[[1]], c("BFoneSample", "BFindepSample"))) {
    # t-test ----
    samps <- as.data.frame(BayesFactor::posterior(model, iterations = 4000, progress = FALSE))

    paired <- inherits(model@numerator[[1]], "BFoneSample")
    if (!paired) {
      mu <- 0
      D <- samps$delta
    } else {
      mu <- as.numeric(gsub("Null, mu=", "", model@denominator@shortName))
      D <- (samps$mu - mu) / sqrt(samps$sig2)
    }

    res <- data.frame(Cohens_d = D)
    xtra_class <- "effectsize_difference"
  } else if (inherits(model@numerator[[1]], "BFcorrelation")) {
    # Corr ----
    rho <- insight::get_parameters(model)[["rho"]]
    res <- data.frame(rho = rho)
  } else if (inherits(model@numerator[[1]], "BFproportion")) {
    # Prop ----
    res <- insight::get_parameters(model)
    p0 <- model@denominator@identifier[["p0"]]
    xtra_footer <- list(c(sprintf("\n- Against the null: p = %s.", p0), "cyan"))
  } else {
    stop("No effect size for this type of BayesFactor object.")
  }

  # Clean up
  out <- bayestestR::describe_posterior(res, test = test, ...)
  colnames(out)[2] <- out$Parameter
  out$Parameter <- NULL

  class(out) <- c(xtra_class, "effectsize_table", "see_effectsize_table", class(out))

  attr(out, "paired") <- paired
  attr(out, "table_footer") <- xtra_footer
  attr(out, "mu") <- mu

  attr(out, "ci") <- out$CI
  attr(out, "ci_method") <- if (!is.null(out$CIs)) list(method = "MCMC")
  attr(out, "correction") <- NULL
  attr(out, "pooled_sd") <- NULL
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- "two.sided"
  out
}