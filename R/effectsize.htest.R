#' @export
#' @rdname effectsize
effectsize.htest <- function(model, type = NULL, verbose = TRUE, ...) {
  # Get data?
  data <- insight::get_data(model)
  approx <- is.null(data)

  dots <- list(...)

  if (grepl("t-test", model$method)) {
    # t-test ----
    if (is.null(dots$alternative)) dots$alternative <- model$alternative
    if (is.null(dots$ci)) dots$ci <- attr(model$conf.int,"conf.level")
    if (approx) {
      if (verbose) {
        warning("Unable to retrieve data from htest object. Using t_to_d() approximation.")
      }

      f <- t_to_d
      args <- list(
        t = unname(model$statistic),
        df_error = unname(model$parameter),
        paired = !grepl("Two", model$method)
      )
    } else {
      if (grepl(" by ", model$data.name, fixed = TRUE)) {
        data[[2]] <- factor(data[[2]])
      }

      if (is.null(type)) type <- "d"
      f <- switch(tolower(type),
        d = ,
        cohens_d = cohens_d,
        g = ,
        hedges_g = hedges_g
      )

      args <- list(
        x = data[[1]],
        y = if (ncol(data) == 2) data[[2]],
        mu = model$null.value,
        paired = !grepl("Two", model$method),
        pooled_sd = !grepl("Welch", model$method),
        verbose = verbose
      )
    }
    out <- do.call(f, c(args, dots))
    attr(out, "approximate") <- approx
    return(out)
  } else if (grepl("correlation", model$method)) {
    # correlation ----
    out <- t_to_r(1, 1, ci = NULL)
    out$r <- unname(model$estimate)
    out$CI <- attr(model$conf.int, "conf.level")
    out$CI_low <- model$conf.int[1]
    out$CI_high <- model$conf.int[2]
    attr(out, "ci") <- out$CI
    attr(out, "approximate") <- FALSE
    attr(out, "alternative") <- model$alternative
    return(out)
  } else if (grepl("Pearson's Chi-squared", model$method) ||
    grepl("Chi-squared test for given probabilities", model$method)) {
    # Chisq ----
    if (is.null(type)) type <- "cramers_v"

    if (grepl("(v|w|phi)$", tolower(type))) {
      f <- switch(tolower(type),
        v = ,
        cramers_v = chisq_to_cramers_v,
        w = ,
        cohens_w = ,
        phi = chisq_to_phi
      )

      Obs <- model$observed
      Exp <- model$expected

      if (!is.null(dim(Exp))) {
        if (any(c(colSums(Obs), rowSums(Obs)) == 0L)) {
          stop("Cannot have empty rows/columns in the contingency tables.", call. = FALSE)
        }
        nr <- nrow(Obs)
        nc <- ncol(Obs)
      } else {
        nr <- length(Obs)
        nc <- 1
      }

      out <- f(
        chisq = .chisq(Obs, Exp),
        n = sum(Obs),
        nrow = nr,
        ncol = nc,
        ...
      )
      attr(out, "approximate") <- FALSE
      return(out)
    } else {
      f <- switch(tolower(type),
        or = ,
        oddsratio = oddsratio,
        rr = ,
        riskratio = riskratio,
        h = ,
        cohens_h = cohens_h
      )
    }

    out <- f(x = model$observed, ...)
    return(out)
  } else if (grepl("One-way", model$method)) {
    # one way anove ----
    if (approx <- grepl("not assuming", model$method, fixed = TRUE) && verbose) {
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
    attr(out, "approximate") <- approx
    return(out)
  } else if (grepl("McNemar", model$method)) {
    # McNemar ----
    if (approx) {
      stop("Unable to retrieve data from htest object.",
        "\nTry using 'cohens_g()' directly.",
        call. = FALSE
      )
    }

    if (inherits(data, "table")) {
      out <- cohens_g(data, ...)
    } else {
      out <- cohens_g(data[[1]], data[[2]], ...)
    }
    return(out)
  } else if (grepl("Fisher's Exact", model$method)) {
    # Fisher's Exact ----
    if (is.null(model$estimate)) {
      stop("Cannot extract effect size for Fisher's exact test on a table larger than 2x2.",
        call. = FALSE
      )
    }

    out <- oddsratio(c(1, 2), c(1, 2), ci = NULL)
    out[[1]] <- unname(model$estimate)
    out$CI <- attr(model$conf.int, "conf.level")
    out$CI_low <- model$conf.int[1]
    out$CI_high <- model$conf.int[2]
    attr(out, "ci") <- out$CI
    attr(out, "table_footer") <- c("\n- Maximum likelihood estimate (MLE) of the OR.", "cyan")
    attr(out, "approximate") <- FALSE
    attr(out, "alternative") <- model$alternative
    return(out)
  } else if (grepl("Wilcoxon", model$method)) {
    # Wilcoxon ----
    if (is.null(dots$alternative)) dots$alternative <- model$alternative
    if (is.null(dots$ci)) dots$ci <- attr(model$conf.int,"conf.level")

    if (approx) {
      stop("Unable to retrieve data from htest object.",
        "\nTry using 'rank_biserial()' directly.",
        call. = FALSE
      )
    }

    f <- rank_biserial
    args <- list(
      x = data[[1]],
      y = if (ncol(data) == 2) data[[2]],
      paired = grepl("signed rank", model$method, fixed = TRUE),
      mu = model$null.value
    )
    out <- do.call(f, c(args, dots))
    return(out)
  } else if (grepl("Kruskal-Wallis", model$method)) {
    # Kruskal-Wallis ----
    if (approx) {
      stop("Unable to retrieve data from htest object.",
        "\nTry using 'rank_epsilon_squared()' directly.",
        call. = FALSE
      )
    }
    if (inherits(data, "list")) {
      out <- rank_epsilon_squared(data, ...)
    } else { # data frame
      out <- rank_epsilon_squared(data[[1]], data[[2]], ...)
    }
    return(out)
  } else if (grepl("Friedman", model$method)) {
    # Friedman ----
    if (approx) {
      stop("Unable to retrieve data from htest object.",
        "\nTry using 'kendalls_w()' directly.",
        call. = FALSE
      )
    }

    if (inherits(data, "table")) {
      data <- data.frame(
        x = c(data),
        groups = rep(colnames(data), each = nrow(data)),
        blocks = rep(rownames(data), ncol(data))
      )
    }

    out <- kendalls_w(data[[1]], data[[2]], data[[3]], ...)
    return(out)
  } else {
    stop("This 'htest' method is not (yet?) supported.", call. = FALSE)
  }
}


#' @keywords internal
.chisq <- function(Obs, Exp) {
  sum(((Obs - Exp)^2) / Exp)
}
