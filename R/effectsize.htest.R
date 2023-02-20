#' @export
#' @rdname effectsize
effectsize.htest <- function(model, type = NULL, verbose = TRUE, ...) {
  if (grepl("t-test", model$method, fixed = TRUE)) {
    .effectsize_t.test(model, type = type, verbose = verbose, ...)
  } else if (grepl("Pearson's Chi-squared", model$method, fixed = TRUE)) {
    .effectsize_chisq.test_dep(model, type = type, verbose = verbose, ...)
  } else if (grepl("Chi-squared test for given probabilities", model$method, fixed = TRUE)) {
    .effectsize_chisq.test_gof(model, type = type, verbose = verbose, ...)
  } else if (grepl("Fisher's Exact", model$method, fixed = TRUE)) {
    .effectsize_fisher.test(model, type = type, verbose = verbose, ...)
  } else if (grepl("One-way", model$method, fixed = TRUE)) {
    .effectsize_oneway.test(model, type = type, verbose = verbose, ...)
  } else if (grepl("McNemar", model$method, fixed = TRUE)) {
    .effectsize_mcnemar.test(model, type = type, verbose = verbose, ...)
  } else if (grepl("Wilcoxon", model$method, fixed = TRUE)) {
    .effectsize_wilcox.test(model, type = type, verbose = verbose, ...)
  } else if (grepl("Kruskal-Wallis", model$method, fixed = TRUE)) {
    .effectsize_kruskal.test(model, type = type, verbose = verbose, ...)
  } else if (grepl("Friedman", model$method, fixed = TRUE)) {
    .effectsize_friedman.test(model, type = type, verbose = verbose, ...)
  } else {
    if (verbose) {
      insight::format_warning(
        "This 'htest' method is not (yet?) supported.",
        "Returning 'parameters::model_parameters(model)'."
      )
    }
    parameters::model_parameters(model, verbose = verbose, ...)
  }
}

#' @keywords internal
.effectsize_t.test <- function(model, type = NULL, verbose = TRUE, ...) {
  # Get data?
  data <- .get_data_htest_alt(model)
  approx <- is.null(data)

  dots <- list(...)

  if (is.null(type) || tolower(type) == "cohens_d") type <- "d"
  if (tolower(type) == "hedges_g") type <- "g"

  dots$alternative <- model$alternative
  dots$ci <- attr(model$conf.int, "conf.level")
  dots$mu <- model$null.value
  dots$paired <- !grepl("Two", model$method, fixed = TRUE)
  dots$verbose <- verbose

  if (!type %in% c("d", "g")) {
    .fail_if_approx(approx, "cles")
  }

  if (approx) {
    if (verbose) {
      insight::format_warning(
        "Unable to retrieve data from htest object.",
        "Returning an {.b approximate} effect size using t_to_d()."
      )
    }

    f <- t_to_d
    args <- list(
      t = unname(model$statistic),
      df_error = unname(model$parameter)
    )
  } else {
    args <- list(
      x = data[[1]],
      y = if (ncol(data) == 2) data[[2]],
      pooled_sd = !grepl("Welch", model$method, fixed = TRUE)
    )

    if (type %in% c("d", "g")) {
      f <- switch(tolower(type),
        d = cohens_d,
        g = hedges_g
      )
    } else {
      if (!dots$paired && !args$pooled_sd) {
        insight::format_error("Common language effect size only applicable to Cohen's d with pooled SD.")
      }

      f <- switch(tolower(type),
        u1 = cohens_u1,
        u2 = cohens_u2,
        u3 = cohens_u3,
        vda = ,
        p_superiority = p_superiority,
        overlap = p_overlap
      )
    }
  }

  out <- do.call(f, c(args, dots))
  attr(out, "approximate") <- approx
  out
}

#' @keywords internal
.effectsize_chisq.test_dep <- function(model, type = NULL, verbose = TRUE, ...) {
  # Get data?
  data <- insight::get_data(model)
  approx <- is.null(data)

  dots <- list(...)

  Obs <- model$observed
  Exp <- model$expected

  if (any(c(colSums(Obs), rowSums(Obs)) == 0L)) {
    insight::format_error("Cannot have empty rows/columns in the contingency tables.")
  }
  nr <- nrow(Obs)
  nc <- ncol(Obs)

  if (is.null(type)) type <- "cramers_v"

  if (grepl("(c|v|t|w|phi)$", tolower(type)) && tolower(type) != "nnt") {
    f <- switch(tolower(type),
      v = ,
      cramers_v = chisq_to_cramers_v,
      t = ,
      tschuprows_t = chisq_to_tschuprows_t,
      w = ,
      cohens_w = chisq_to_cohens_w,
      phi = chisq_to_phi,
      c = ,
      pearsons_c = chisq_to_pearsons_c
    )

    out <- f(
      chisq = .chisq(Obs, Exp),
      n = sum(Obs),
      nrow = nr,
      ncol = nc,
      verbose = verbose,
      ...
    )
  } else {
    f <- switch(tolower(type),
      or = ,
      oddsratio = oddsratio,
      rr = ,
      riskratio = riskratio,
      h = ,
      cohens_h = cohens_h,
      arr = arr,
      nnt = nnt
    )

    out <- f(x = model$observed, ...)
  }

  attr(out, "approximate") <- FALSE
  out
}

#' @keywords internal
.effectsize_fisher.test <- function(model, type = NULL, verbose = TRUE, ...) {
  if (is.null(type)) type <- "cramers_v"

  # If OR - return OR
  if (tolower(type) %in% c("or", "oddsratio")) {
    out <- data.frame(Odds_ratio = unname(model[["estimate"]]))
    ci_method <- NULL

    ci <- model[["conf.int"]]
    if (!is.null(ci)) {
      out$CI <- attr(ci, "conf.level")
      out$CI_low <- ci[1]
      out$CI_high <- ci[2]
      ci_method <- list("normal")
    }

    class(out) <- c("effectsize_table", "see_effectsize_table", "data.frame")
    .someattributes(out) <-
      .nlist(
        ci = out$CI, ci_method,
        approximate = FALSE,
        alternative = model[["alternative"]]
      )
    return(out)
  }

  dots <- list(...)
  if (!is.null(model[["conf.int"]])) dots$ci <- attr(model[["conf.int"]], "conf.level")
  if (!is.null(model[["alternative"]])) dots$alternative <- model[["alternative"]]

  data <- insight::get_data(model)
  .fail_if_approx(is.null(data), type)

  f <- switch(tolower(type),
    v = ,
    cramers_v = cramers_v,
    t = ,
    tschuprows_t = tschuprows_t,
    w = ,
    cohens_w = cohens_w,
    phi = phi,
    c = ,
    pearsons_c = pearsons_c,
    or = ,
    oddsratio = oddsratio,
    rr = ,
    riskratio = riskratio,
    h = ,
    cohens_h = cohens_h,
    arr = arr,
    nnt = nnt
  )

  if (is.table(data)) {
    args <- list(x = data)
  } else {
    args <- list(x = data[[1]], y = data[[2]])
  }

  do.call(f, c(args, dots))
}

#' @keywords internal
.effectsize_chisq.test_gof <- function(model, type = NULL, verbose = TRUE, ...) {
  # Get data?
  data <- insight::get_data(model)
  approx <- is.null(data)

  dots <- list(...)

  Obs <- model$observed
  Exp <- model$expected
  nr <- length(Obs)
  p <- Exp

  if (is.null(type)) type <- "fei"

  f <- switch(tolower(type),
    w = ,
    cohens_w = chisq_to_cohens_w,
    c = ,
    pearsons_c = chisq_to_pearsons_c,
    fei = chisq_to_fei,
    insight::format_error("The selected effect size is not supported for goodness-of-fit tests.")
  )

  out <- f(
    chisq = .chisq(Obs, Exp),
    n = sum(Obs),
    nrow = nr,
    ncol = 1,
    p = p,
    verbose = verbose,
    ...
  )

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
    insight::format_alert("`var.equal = FALSE` - effect size is an {.b approximation.}")
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
    verbose = verbose,
    ...
  )
  colnames(out)[1] <- sub("_partial", "", colnames(out)[1], fixed = TRUE)
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
    out <- cohens_g(data, verbose = verbose, ...)
  } else {
    out <- cohens_g(data[[1]], data[[2]], verbose = verbose, ...)
  }
  out
}

#' @keywords internal
.effectsize_wilcox.test <- function(model, type = NULL, verbose = TRUE, ...) {
  # Get data?
  data <- .get_data_htest_alt(model)
  approx <- is.null(data)

  dots <- list(...)

  if (is.null(type) || tolower(type) == "rank_biserial") type <- "rb"

  dots$alternative <- model$alternative
  dots$ci <- attr(model$conf.int, "conf.level")
  dots$mu <- model$null.value
  dots$paired <- grepl("signed rank", model$method, fixed = TRUE)

  .fail_if_approx(approx, type)

  f <- switch(tolower(type),
    rb = rank_biserial,
    u1 = cohens_u1,
    u2 = cohens_u2,
    u3 = cohens_u3,
    overlap = p_overlap,
    vda = ,
    p_superiority = p_superiority,
    wmw_odds = wmw_odds
  )

  args <- list(
    x = data[[1]],
    y = if (ncol(data) == 2) data[[2]],
    verbose = verbose
  )

  if (tolower(type) != "rb") {
    if (dots$paired) {
      insight::format_error("Common language effect size only applicable to 2-sample rank-biserial correlation.")
    }
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

  if (is.null(type)) type <- "epsilon"

  .fail_if_approx(approx, "rank_epsilon_squared")

  f <- switch(type,
    epsilon = rank_epsilon_squared,
    eta = rank_eta_squared
  )

  if (inherits(data, "data.frame")) {
    out <- f(data[[1]], data[[2]], verbose = verbose, ...)
  } else {
    # data frame
    out <- f(data, verbose = verbose, ...)
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

  out <- kendalls_w(data[[1]], data[[2]], data[[3]], verbose = verbose, ...)
  out
}

# Utils -------------------------------------------------------------------


#' @keywords internal
.chisq <- function(Obs, Exp) {
  sum(((Obs - Exp)^2) / Exp)
}

#' @keywords internal
.get_data_htest_alt <- function(model) {
  # See https://github.com/easystats/insight/issues/722
  data <- insight::get_data(model)
  if (is.null(data)) {
    return(NULL)
  }

  if (grepl("(Two|sum)", model$method)) {
    if (grepl(" and ", model$data.name, fixed = TRUE)) {
      data <- datawizard::reshape_longer(data)[, 2:1]
    }
    data[[2]] <- factor(data[[2]])
  } else if (grepl("(Paired|signed)", model$method) && ncol(data) == 2) {
    if (grepl(" by ", model$data.name, fixed = TRUE)) {
      data <- datawizard::reshape_wider(data,
        values_from = colnames(data)[1],
        names_from = colnames(data)[2]
      )
    }
  }

  na.omit(data)
}

#' @keywords internal
.fail_if_approx <- function(approx, esf_name) {
  if (approx) {
    insight::format_error(
      "Unable to retrieve data from htest object.",
      sprintf("Try using '%s()' directly.", esf_name)
    )
  }
  invisible(NULL)
}
