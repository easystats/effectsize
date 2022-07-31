#' @export
#' @rdname effectsize
effectsize.htest <- function(model, type = NULL, verbose = TRUE, ...) {
  if (grepl("t-test", model$method)) {
    .effectsize_t.test(model, type = type, verbose = verbose, ...)
  } else if (grepl("Pearson's Chi-squared", model$method) ||
    grepl("Chi-squared test for given probabilities", model$method)) {
    .effectsize_chisq.test(model, type = type, verbose = verbose, ...)
  } else if (grepl("One-way", model$method)) {
    .effectsize_oneway.test(model, type = type, verbose = verbose, ...)
  } else if (grepl("McNemar", model$method)) {
    .effectsize_mcnemar.test(model, type = type, verbose = verbose, ...)
  } else if (grepl("Wilcoxon", model$method)) {
    .effectsize_wilcox.test(model, type = type, verbose = verbose, ...)
  } else if (grepl("Kruskal-Wallis", model$method)) {
    .effectsize_kruskal.test(model, type = type, verbose = verbose, ...)
  } else if (grepl("Friedman", model$method)) {
    .effectsize_friedman.test(model, type = type, verbose = verbose, ...)
  } else {
    if (verbose) {
      warning("This 'htest' method is not (yet?) supported.\n",
        "Returning 'parameters::model_parameters(model)'.",
        call. = FALSE
      )
    }
    parameters::model_parameters(model, verbose = verbose, ...)
  }
}

#' @keywords internal
.effectsize_t.test <- function(model, type = NULL, verbose = TRUE, ...) {
  # Get data?
  data <- insight::get_data(model)
  approx <- is.null(data)

  dots <- list(...)

  if (is.null(type)) type <- "d"

  dots$alternative <- model$alternative
  dots$ci <- attr(model$conf.int, "conf.level")
  dots$mu <- model$null.value

  if (type == "cles") {
    .fail_if_approx(approx, "cles")
  }

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

    f <- switch(tolower(type),
      d = ,
      cohens_d = cohens_d,
      g = ,
      hedges_g = hedges_g,
      cles = cles
    )

    args <- list(
      x = data[[1]],
      y = if (ncol(data) == 2) data[[2]],
      paired = !grepl("Two", model$method),
      pooled_sd = !grepl("Welch", model$method),
      verbose = verbose
    )

    if (type == "cles") {
      if (args$paired || !args$pooled_sd) {
        stop("Common language effect size only applicable to 2-sample Cohen's d with pooled SD.")
      }
      args$pooled_sd <- args$paired <- NULL
    }
  }
  out <- do.call(f, c(args, dots))
  attr(out, "approximate") <- approx
  out
}

#' @keywords internal
.effectsize_chisq.test <- function(model, type = NULL, verbose = TRUE, ...) {
  # Get data?
  data <- insight::get_data(model)
  approx <- is.null(data)

  dots <- list(...)

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

  if (is.null(type)) {
    if (nr == 1 || nc == 1) {
      type <- "normalized_chi"
    } else {
      type <- "cramers_v"
    }
  }

  if (grepl("(c|v|w|phi)$", tolower(type)) || tolower(type) %in% c("normalized_chi", "chi")) {
    if (tolower(type) %in% c("normalized_chi", "chi")) {
      p <- Exp
    } else {
      p <- NULL
    }

    f <- switch(tolower(type),
      v = ,
      cramers_v = chisq_to_cramers_v,
      w = ,
      cohens_w = chisq_to_cohens_w,
      phi = chisq_to_phi,
      c = ,
      pearsons_c = chisq_to_pearsons_c,
      chi = ,
      normalized_chi = chisq_to_normalized
    )

    out <- f(
      chisq = .chisq(Obs, Exp),
      n = sum(Obs),
      nrow = nr,
      ncol = nc,
      p = p,
      ...
    )
  } else {
    f <- switch(tolower(type),
      or = ,
      oddsratio = oddsratio,
      rr = ,
      riskratio = riskratio,
      h = ,
      cohens_h = cohens_h
    )

    out <- f(x = model$observed, ...)
  }

  attr(out, "approximate") <- FALSE
  out
}

#' @keywords internal
.effectsize_oneway.test <- function(model, type = NULL, verbose = TRUE, ...) {
  # Get data?
  data <- insight::get_data(model)
  approx <- is.null(data)

  dots <- list(...)

  if ((approx <- grepl("not assuming", model$method, fixed = TRUE)) && verbose) {
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
  out
}

#' @keywords internal
.effectsize_mcnemar.test <- function(model, type = NULL, verbose = TRUE, ...) {
  # Get data?
  data <- insight::get_data(model)
  approx <- is.null(data)

  dots <- list(...)

  .fail_if_approx(approx, "cohens_g")

  if (inherits(data, "table")) {
    out <- cohens_g(data, ...)
  } else {
    out <- cohens_g(data[[1]], data[[2]], ...)
  }
  out
}

#' @keywords internal
.effectsize_wilcox.test <- function(model, type = NULL, verbose = TRUE, ...) {
  # Get data?
  data <- insight::get_data(model)
  approx <- is.null(data)

  dots <- list(...)

  if (is.null(type)) type <- "rb"

  dots$alternative <- model$alternative
  dots$ci <- attr(model$conf.int, "conf.level")
  dots$mu <- model$null.value

  .fail_if_approx(approx, ifelse(type == "cles", "cles", "rank_biserial"))

  f <- switch(tolower(type),
    r = ,
    rb = ,
    rbs = ,
    r_rank_biserial = ,
    rank_biserial = rank_biserial,
    cles = cles
  )

  args <- list(
    x = data[[1]],
    y = if (ncol(data) == 2) data[[2]],
    paired = grepl("signed rank", model$method, fixed = TRUE)
  )

  if (type == "cles") {
    if (args$paired) {
      stop("Common language effect size only applicable to 2-sample rank-biserial correlation.")
    }
    args$paired <- NULL
    args$parametric <- FALSE
  }

  out <- do.call(f, c(args, dots))
  out
}

#' @keywords internal
.effectsize_kruskal.test <- function(model, type = NULL, verbose = TRUE, ...) {
  # Get data?
  data <- insight::get_data(model)
  approx <- is.null(data)

  dots <- list(...)

  .fail_if_approx(approx, "rank_epsilon_squared")


  if (inherits(data, "data.frame")) {
    out <- rank_epsilon_squared(data[[1]], data[[2]], ...)
  } else {
    # data frame
    out <- rank_epsilon_squared(data, ...)
  }
  out
}

#' @keywords internal
.effectsize_friedman.test <- function(model, type = NULL, verbose = TRUE, ...) {
  # Get data?
  data <- insight::get_data(model)
  approx <- is.null(data)

  dots <- list(...)

  .fail_if_approx(approx, "kendalls_w")

  if (inherits(data, "table")) {
    data <- as.data.frame(data)[c("Freq", "Var2", "Var1")]
  }

  out <- kendalls_w(data[[1]], data[[2]], data[[3]], ...)
  out
}


# Utils -------------------------------------------------------------------


#' @keywords internal
.chisq <- function(Obs, Exp) {
  sum(((Obs - Exp)^2) / Exp)
}

#' @keywords internal
.fail_if_approx <- function(approx, esf_name) {
  if (approx) {
    stop("Unable to retrieve data from htest object.",
      "\nTry using '", esf_name, "()' directly.",
      call. = FALSE
    )
  }
  invisible(NULL)
}
