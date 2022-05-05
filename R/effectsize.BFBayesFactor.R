#' @export
#' @rdname effectsize
#' @inheritParams bayestestR::describe_posterior
#' @importFrom insight get_data get_parameters check_if_installed
#' @importFrom bayestestR describe_posterior
effectsize.BFBayesFactor <- function(model, type = NULL, verbose = TRUE, test = NULL, ...) {
  insight::check_if_installed("BayesFactor")

  if (length(model) > 1) {
    if (verbose) {
      warning("Multiple models detected. Using first only.", call. = FALSE)
    }
    model <- model[1]
  }

  if (inherits(model@numerator[[1]], "BFcontingencyTable")) {
    pars <- .effectsize_contingencyTableBF(model, type = type, verbose = verbose)
  } else if (inherits(model@numerator[[1]], c("BFoneSample", "BFindepSample"))) {
    pars <- .effectsize_ttestBF(model, type = type, verbose = verbose)
  } else if (inherits(model@numerator[[1]], "BFcorrelation")) {
    pars <- .effectsize_correlationBF(model, type = type, verbose = verbose)
  } else if (inherits(model@numerator[[1]], "BFproportion")) {
    pars <- .effectsize_proportionBF(model, type = type, verbose = verbose)
  } else {
    stop("No effect size for this type of 'BayesFactor' object.")
  }

  # Clean up
  out <- bayestestR::describe_posterior(pars$res, test = test, ...)
  if (isTRUE(type == "cles")) {
    colnames(out)[2] <-  "Coefficient"
  } else {
    colnames(out)[2] <- out$Parameter
    out$Parameter <- NULL
  }

  class(out) <- c(pars$xtra_class, "effectsize_table", "see_effectsize_table", class(out))
  .someattributes(out) <- pars$attr
  .someattributes(out) <- list(
    ci = out$CI,
    # ci_method - inherited from bayestestR
    correction = NULL,
    pooled_sd = NULL,
    approximate = FALSE,
    alternative = "two.sided"
  )
  out
}

#' @keywords internal
.effectsize_contingencyTableBF <- function(model, type = NULL, verbose = TRUE) {
  if (is.null(type)) type <- "cramers_v"

  f <- switch(tolower(type),
              v = ,
              cramers_v = cramers_v,
              w = ,
              cohens_w = cohens_w,
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

  list(res = res,
       attr = NULL,
       xtra_class = NULL)
}


#' @keywords internal
.effectsize_ttestBF <- function(model, type = NULL, verbose = TRUE) {
  if (is.null(type)) type <- "d"
  type <- c("cohens_d" = "d", d = "d", "cles" = "cles")[type]

  samps <- as.data.frame(BayesFactor::posterior(model, iterations = 4000, progress = FALSE))

  paired <- inherits(model@numerator[[1]], "BFoneSample")
  if (!paired) {
    mu <- 0
    D <- samps$delta
  } else {
    mu <- model@numerator[[1]]@prior$mu
    D <- (samps$mu - mu) / sqrt(samps$sig2)
  }

  res <- data.frame(Cohens_d = D)
  if (type == "d") {
    xtra_class <- "effectsize_difference"
  } else if (type == "cles") {
    res <- data.frame(d_to_cles(res$Cohens_d), check.names = FALSE)
    xtra_class <- NULL
  }

  list(res = res,
       attr = list(mu = mu, paired = paired),
       xtra_class = xtra_class)
}


#' @keywords internal
.effectsize_correlationBF <- function(model, type = NULL, verbose = TRUE) {
  rho <- insight::get_parameters(model)[["rho"]]
  res <- data.frame(rho = rho)

  list(res = res,
       attr = NULL,
       xtra_class = NULL)
}


#' @keywords internal
.effectsize_proportionBF <- function(model, type = NULL, verbose = TRUE) {
  res <- insight::get_parameters(model)

  p0 <- model@denominator@identifier[["p0"]]
  xtra_footer <- list(c(sprintf("\n- Against the null: p = %s.", p0), "cyan"))

  list(res = res,
       attr = list(xtra_footer = xtra_footer),
       xtra_class = NULL)
}