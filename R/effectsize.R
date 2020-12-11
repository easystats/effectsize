#' Effect Size
#'
#' This function tries to return the best effect-size measure for the provided
#' input model. See details.
#'
#' @param model An object of class `htest`, or a statistical model. See details.
#' @param type The effect size of interest. See details.
#' @param ... Arguments passed to or from other methods. See details.
#'
#' @details
#'
#' - For an object of class `htest`:
#'   - A **t-test** returns *Cohen's d* via [t_to_d()].
#'   - A **correlation test** returns *r*.
#'   - A **Chi-squared tests of independence or goodness-of-fit**, depending on `type`: `"cramers_v"` (default), `"phi"` or `"cohens_w"`, `"oddsratio"`, or `"riskratio"`.
#'   - A **One-way ANOVA test**, depending on `type`: `"eta"` (default), `"omega"` or `"epsilon"` -squared, `"f"`, or `"f2"`.
#' - For an object of class `BFBayesFactor`, using [bayestestR::describe_posterior()],
#'   - A **t-test** returns *Cohen's d*.
#'   - A **correlation test** returns *r*.
#'   - A **contingency table test**, depending on `type`: `"cramers_v"` (default), `"phi"` or `"cohens_w"`, `"oddsratio"`, or `"riskratio"`.
#' - Objects of class `anova`, `aov`, or `aovlist`, depending on `type`: `"eta"` (default), `"omega"` or `"epsilon"` -squared, `"f"`, or `"f2"`.
#' - Other objects are passed to [standardize_parameters()].
#'
#' **For statistical models it is recommended to directly use the listed
#' functions, for the full range of options they provide.**
#'
#' @family effect size indices
#'
#' @examples
#'
#' ## Hypothesis Testing
#' ## ------------------
#' contingency_table <- as.table(rbind(c(762, 327, 468), c(484, 239, 477), c(484, 239, 477)))
#' Xsq <- chisq.test(contingency_table)
#' effectsize(Xsq)
#' effectsize(Xsq, type = "phi")
#'
#' Ts <- t.test(1:10, y = c(7:20))
#' effectsize(Ts)
#'
#' Aov <- oneway.test(extra ~ group, data = sleep, var.equal = TRUE)
#' effectsize(Aov)
#' effectsize(Aov, type = "omega")
#'
#'
#' ## Bayesian Hypothesis Testing
#' ## ---------------------------
#' if (require(BayesFactor)) {
#'   bf1 <- ttestBF(mtcars$mpg[mtcars$am == 1], mtcars$mpg[mtcars$am == 0])
#'   effectsize(bf1, test = NULL)
#'
#'   bf2 <- correlationBF(iris$Sepal.Length, iris$Sepal.Width)
#'   effectsize(bf2, test = NULL)
#'
#'   data(raceDolls)
#'   bf3 <- contingencyTableBF(raceDolls, sampleType = "poisson", fixedMargin = "cols")
#'   effectsize(bf3, test = NULL)
#'   effectsize(bf3, type = "oddsratio", test = NULL)
#' }
#'
#'
#' ## Models and Anova Tables
#' ## -----------------------
#' fit <- lm(mpg ~ factor(cyl) * wt + hp, data = mtcars)
#' effectsize(fit)
#'
#' anova_table <- anova(fit)
#' effectsize(anova_table)
#' effectsize(anova_table, type = "epsilon")
#' @export
effectsize <- function(model, ...) {
  UseMethod("effectsize")
}

#' @export
#' @rdname effectsize
effectsize.htest <- function(model, type = NULL, ...) {
  if (grepl("t-test", model$method)) {
    # message("Using t_to_d().")
    out <- t_to_d(
      unname(model$statistic),
      unname(model$parameter),
      paired = !grepl("Two", model$method),
      ...
    )
    return(out)
  } else if (grepl("correlation", model$method)) {
    out <- t_to_r(1, 1, ci = NULL)
    out$r <- unname(model$estimate)
    out$CI <- attr(model$conf.int, "conf.level")
    out$CI_low <- model$conf.int[1]
    out$CI_high <- model$conf.int[2]
    return(out)
  } else if (grepl("Pearson's Chi-squared", model$method) ||
    grepl("Chi-squared test for given probabilities", model$method)) {
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
    if (grepl("not assuming", model$method, fixed = TRUE)) {
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
    stop("Cannot extract Cohen's g from an 'htest' object.",
      "\nTry using 'cohens_g()' directly.",
      call. = FALSE
    )
  } else if (grepl("Fisher's Exact", model$method)) {
    stop("Cannot extract effect size from an 'htest' of Fisher's exact test.",
      "\nTry using 'cramers_v()' or 'phi()' directly.",
      call. = FALSE
    )
  } else if (grepl("Wilcoxon", model$method)) {
    stop("Cannot extract effect size from an 'htest' of Wilcoxon's test.",
      "\nTry using 'ranktransform()' and 'cohens_d()' directly.",
      call. = FALSE
    )
  } else {
    stop("This 'htest' method is not (yet?) supported.", call. = FALSE)
  }
}

#' @export
#' @rdname effectsize
#' @importFrom insight get_data get_parameters
#' @importFrom bayestestR describe_posterior
effectsize.BFBayesFactor <- function(model, type = NULL, ...) {
  if (!requireNamespace("BayesFactor")) {
    stop("This function requires 'BayesFactor' to work. Please install it.")
  }

  if (length(model) > 1) {
    warning("Multiple models detected. Using first only.", call. = FALSE)
    model <- model[1]
  }

  if (inherits(model@numerator[[1]], "BFcontingencyTable")) {
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

    data <- insight::get_data(model)
    N <- sum(data)
    cells <- prod(dim(data))

    posts <- as.matrix(BayesFactor::posterior(model, iterations = 4000, progress = FALSE))
    posts <- posts[, seq_len(cells)]
    if (sum(posts[1, ]) == 1) {
      posts <- posts * N
    }

    ES <- apply(posts, 1, function(a) {
      f(matrix(a, nrow = nrow(data)), ci = NULL)[[1]]
    })

    res <- data.frame(ES)
    colnames(res) <- colnames(f(matrix(posts[1, ], nrow = nrow(data)), ci = NULL))
  } else if (inherits(model@numerator[[1]], c("BFoneSample", "BFindepSample"))) {
    D <- as.matrix(BayesFactor::posterior(model, iterations = 4000, progress = FALSE))[, "delta"]
    res <- data.frame(Cohens_d = D)
  } else if (inherits(model@numerator[[1]], "BFcorrelation")) {
    rho <- insight::get_parameters(model)[["rho"]]
    res <- data.frame(rho = rho)
  } else if (inherits(model@numerator[[1]], "BFproportion")) {
    res <- insight::get_parameters(model)
  } else {
    stop("No effect size for this type of BayesFactor object.")
  }

  bayestestR::describe_posterior(res, ...)
}


#' @export
effectsize.anova <- function(model, type = NULL, ...) {
  if (is.null(type)) type <- "eta"

  f <- switch(tolower(type),
    eta = ,
    eta2 = ,
    eta_squared = eta_squared,

    epsilon = ,
    epsilon2 = ,
    epsilon_squared = epsilon_squared,

    omega = ,
    omega2 = ,
    omega_squared = omega_squared,

    f = ,
    cohens_f = cohens_f,

    f2 = ,
    f_squared = ,
    cohens_f2 = cohens_f_squared
  )

  f(model, ...)
}

#' @export
#' @rdname effectsize
effectsize.aov <- effectsize.anova

#' @export
effectsize.aovlist <- effectsize.anova


#' @export
effectsize.easycorrelation <- function(model, ...) {
  if (is.null(r_name <- attr(model, "coefficient_name"))) {
    r_name <- "r"
  }

  r_cols <- 1:which(colnames(model) == r_name)
  if (!is.null(attr(model, "ci"))) {
    model$CI <- attr(model, "ci")
    CI_cols <- c("CI", "CI_low", "CI_high")
    CI_cols <- sapply(CI_cols, function(ici) which(colnames(model) == ici))
    r_cols <- c(r_cols, CI_cols)
  }

  out <- model[, r_cols, drop = FALSE]
  class(out) <- c("effectsize_table", "data.frame")
  out
}


#' @export
effectsize.default <- function(model, ...) {
  # message("Using standardize_parameters().")
  standardize_parameters(model, ...)
}
