#' @export
#' @rdname effectsize
effectsize.htest <- function(model, type = NULL, verbose = TRUE, ...) {
  if (grepl("t-test", model$method)) {
    # t-test ----
    if (is.null(data <- insight::get_data(model))) {
      if (verbose)
        warning("Unable to retrieve data from htest object. Using t_to_d() approximation.")
      out <- t_to_d(
        unname(model$statistic),
        unname(model$parameter),
        paired = !grepl("Two", model$method),
        ...
      )
    } else {
      if (is.null(type)) type <- "d"
      f <- switch(tolower(type),
                  d = ,
                  cohens_d = cohens_d,

                  g = ,
                  hedges_g = hedges_g
      )


      out <- f(data$x, data$y,
               mu = model$null.value,
               paired = !grepl("Two", model$method),
               pooled_sd = !grepl("Welch", model$method),
               ...)
    }

    return(out)
  } else if (grepl("correlation", model$method)) {
    # correlation ----
    out <- t_to_r(1, 1, ci = NULL)
    out$r <- unname(model$estimate)
    out$CI <- attr(model$conf.int, "conf.level")
    out$CI_low <- model$conf.int[1]
    out$CI_high <- model$conf.int[2]
    return(out)
  } else if (grepl("Pearson's Chi-squared", model$method) ||
             grepl("Chi-squared test for given probabilities", model$method)) {
    # Chisq ----
    if (is.null(type)) type <- "cramers_v"

    f <- switch(tolower(type),
                v = ,
                cramers_v = cramers_v,

                w = ,
                cohens_w = ,
                phi = phi,

                or = ,
                oddsratio = oddsratio,

                rr = ,
                riskratio = riskratio
    )

    out <- f(x = model$observed, ...)
    return(out)
  } else if (grepl("One-way", model$method)) {
    # one way anove ----
    if (grepl("not assuming", model$method, fixed = TRUE) && verbose) {
      warning("`var.equal = FALSE` - effect size is an approximation.", call. = FALSE)
    }

    if (is.null(type)) type <- "eta"

    f <- switch(tolower(type),
                eta = ,
                eta2 = ,
                eta_squared = F_to_eta2,

                epsilon = ,
                epsilon2 = ,
                epsilon_squared = F_to_epsilon2,

                omega = ,
                omega2 = ,
                omega_squared = F_to_omega2,

                f = ,
                cohens_f = F_to_f,

                f2 = ,
                f_squared = ,
                cohens_f2 = F_to_f2
    )

    out <- f(
      f = model$statistic,
      df = model$parameter[1],
      df_error = model$parameter[2],
      ...
    )
    colnames(out)[1] <- sub("_partial", "", colnames(out)[1])
    return(out)
  } else if (grepl("McNemar", model$method)) {
    # McNemar ----
    if (is.null(data <- insight::get_data(model))) {
      stop("Unable to retrieve data from htest object.",
           "\nTry using 'cohens_g()' directly.",
           call. = FALSE)
    }

    if (inherits(data, "table")) {
      out <- cohens_g(data, ...)
    } else {
      out <- cohens_g(data$x, data$y, ...)
    }
    return(out)
  } else if (grepl("Fisher's Exact", model$method)) {
    # Fisher's Exact ----
    stop("Cannot extract effect size from an 'htest' of Fisher's exact test.",
         "\nTry using 'cramers_v()' or 'phi()' directly.",
         call. = FALSE
    )
  } else if (grepl("Wilcoxon", model$method)) {
    # Wilcoxon ----
    stop("Cannot extract effect size from an 'htest' of Wilcoxon's test.",
         "\nTry using 'ranktransform()' and 'cohens_d()' directly.",
         call. = FALSE
    )
  } else {
    stop("This 'htest' method is not (yet?) supported.", call. = FALSE)
  }
}